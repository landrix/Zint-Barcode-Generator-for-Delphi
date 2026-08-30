<#
  Compares the C function inventory against docs/ports/_functions.tsv.

  Why this exists: the test suites are ports of the C test suites, so they
  inherit their blind spots. A function that C never tests can be missing from
  the port entirely without a single test turning red - z_set_height is the
  example that prompted this script. Tests answer "does what we ported behave
  like C". This answers "did we port all of it".

  What it checks:
    * Every C function of a module marked "done" has a row in _functions.tsv.
      An unclassified function is a gap nobody has looked at yet.
    * Every row naming a Pascal routine actually finds that routine in the
      Delphi unit.
    * No row refers to a C function that no longer exists (stale after a
      reference bump).

  Modules not marked "done" are reported but never fail the run - the
  inventory is meant to be filled in module by module, not in one go.

  A row saying "ported" is a human verdict, not a proof of equality. This
  script finds omissions, never wrong behaviour. For that there are the tests
  and, eventually, a differential test against the C library.

  Exit code 0 = no findings for done modules, 1 = findings (or an error).
#>
param(
  [string]$Module = "",
  [switch]$Skeleton,
  [switch]$Quiet
)

$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
Push-Location $repoRoot
try {
  $cRoot = Join-Path $repoRoot "Lib\zint-master-2026-03-13-b3a3c0d\backend"
  if (-not (Test-Path $cRoot)) {
    throw "C reference not found: $cRoot (see docs/PORTING_WORKFLOW.md, section 2b)"
  }

  $modulesFile = Join-Path $repoRoot "docs\ports\_modules.tsv"
  $functionsFile = Join-Path $repoRoot "docs\ports\_functions.tsv"

  $modules = @(Import-Csv -Path $modulesFile -Delimiter "`t")
  if ($Module -ne "") {
    $modules = @($modules | Where-Object { $_.modul -eq $Module })
    if ($modules.Count -eq 0) { throw "Unknown module: $Module" }
  }

  # A C function definition: start of line, return type, name, args, brace.
  $fnRegex = [regex]'(?m)^(?:INTERNAL\s+|static\s+)?[A-Za-z_][A-Za-z0-9_ \t\*]*?\b([a-z_][a-z0-9_]*)\s*\([^;]*?\)\s*\{'
  $notFunctions = @("if", "for", "while", "switch", "return", "sizeof", "main")

  function Get-CFunctions {
    param([string]$CFile)
    $path = Join-Path $cRoot $CFile
    if (-not (Test-Path $path)) { return $null }
    $src = [IO.File]::ReadAllText($path)
    $names = New-Object System.Collections.Generic.List[string]
    foreach ($m in $fnRegex.Matches($src)) {
      $n = $m.Groups[1].Value
      if ($notFunctions -notcontains $n -and -not $names.Contains($n)) { $names.Add($n) }
    }
    return ($names | Sort-Object)
  }

  function Get-PascalRoutines {
    param([string]$Unit)
    $path = Join-Path $repoRoot $Unit
    if (-not (Test-Path $path)) { return @() }
    $src = [IO.File]::ReadAllText($path)
    $names = @()
    foreach ($m in [regex]::Matches($src, '(?im)^\s*(?:function|procedure)\s+([A-Za-z_]\w*)')) {
      $names += $m.Groups[1].Value.ToLower()
    }
    return ($names | Sort-Object -Unique)
  }

  # --- Skeleton mode: emit TSV rows for everything not yet classified -------
  if ($Skeleton) {
    Write-Output "c_datei`tc_funktion`tstatus`tpascal`tbemerkung"
    $known = @{}
    if (Test-Path $functionsFile) {
      foreach ($r in Import-Csv -Path $functionsFile -Delimiter "`t") {
        $known[($r.c_datei + "|" + $r.c_funktion)] = $true
      }
    }
    foreach ($m in $modules) {
      $fns = Get-CFunctions -CFile $m.c_datei
      if ($null -eq $fns) { continue }
      foreach ($f in $fns) {
        if (-not $known.ContainsKey($m.c_datei + "|" + $f)) {
          Write-Output ("{0}`t{1}`t?`t`t" -f $m.c_datei, $f)
        }
      }
    }
    return
  }

  if (-not (Test-Path $functionsFile)) {
    throw "Inventory not found: $functionsFile (create it with -Skeleton)"
  }
  $rows = @(Import-Csv -Path $functionsFile -Delimiter "`t")

  $validStatus = @("ported", "inline", "partial", "missing", "n/a")
  $problems = @()
  $stats = @()

  # --- Stale rows: a C function that no longer exists -----------------------
  $cFunctionCache = @{}
  foreach ($r in $rows) {
    if (-not $cFunctionCache.ContainsKey($r.c_datei)) {
      $cFunctionCache[$r.c_datei] = Get-CFunctions -CFile $r.c_datei
    }
    $fns = $cFunctionCache[$r.c_datei]
    if ($null -eq $fns) {
      $problems += "stale: $($r.c_datei) is not in the C reference (row for $($r.c_funktion))"
      continue
    }
    if ($fns -notcontains $r.c_funktion) {
      $problems += "stale: $($r.c_datei):$($r.c_funktion) no longer exists in the C reference"
    }
    if ($validStatus -notcontains $r.status) {
      $problems += "bad status '$($r.status)' for $($r.c_datei):$($r.c_funktion) (allowed: $($validStatus -join ', '))"
    }
  }

  # --- Per module ------------------------------------------------------------
  foreach ($m in $modules) {
    $fns = Get-CFunctions -CFile $m.c_datei
    if ($null -eq $fns) { continue }
    $isDone = $m.status -eq "done"
    $moduleRows = @($rows | Where-Object { $_.c_datei -eq $m.c_datei })
    $routines = Get-PascalRoutines -Unit $m.delphi_unit

    $unclassified = @()
    foreach ($f in $fns) {
      if (-not ($moduleRows | Where-Object { $_.c_funktion -eq $f })) {
        $unclassified += $f
      }
    }

    foreach ($r in $moduleRows) {
      if ($r.status -eq "missing" -or $r.status -eq "n/a") { continue }
      if ([string]::IsNullOrWhiteSpace($r.pascal)) {
        $problems += "$($r.c_datei):$($r.c_funktion) is '$($r.status)' but names no Pascal routine"
        continue
      }
      foreach ($name in ($r.pascal -split '\s*,\s*')) {
        if ($routines -notcontains $name.ToLower()) {
          $problems += "$($r.c_datei):$($r.c_funktion) -> '$name' not found in $($m.delphi_unit)"
        }
      }
    }

    if ($isDone -and $unclassified.Count -gt 0) {
      $problems += ("{0} is marked done but {1} C function(s) are unclassified: {2}" -f
        $m.modul, $unclassified.Count, ($unclassified -join ", "))
    }

    $gaps = @($moduleRows | Where-Object { $_.status -eq "missing" -or $_.status -eq "partial" })
    $stats += [pscustomobject]@{
      Modul        = $m.modul
      Status       = $m.status
      CFunktionen  = $fns.Count
      Erfasst      = $moduleRows.Count
      Offen        = $unclassified.Count
      Luecken      = $gaps.Count
    }
  }

  if (-not $Quiet) {
    $stats | Sort-Object Offen -Descending | Format-Table -AutoSize | Out-String | Write-Host
  }

  $covered = ($stats | Where-Object { $_.Offen -eq 0 -and $_.Erfasst -gt 0 }).Count
  $gapTotal = ($stats | Measure-Object -Property Luecken -Sum).Sum
  Write-Host ("check-c-inventory: {0} of {1} modules fully classified, {2} documented gap(s)." -f
    $covered, $stats.Count, $gapTotal)

  if ($problems.Count -gt 0) {
    Write-Host ""
    foreach ($p in $problems) { Write-Host ("  " + $p) }
    Write-Host ""
    throw ("{0} inventory problem(s). A module marked done must have every C function classified." -f $problems.Count)
  }
}
finally {
  Pop-Location
}

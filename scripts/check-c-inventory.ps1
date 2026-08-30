<#
  Compares the C function inventory against docs/ports/_functions.tsv.

  Why this exists: the test suites are ports of the C test suites, so they
  inherit their blind spots. A function that C never tests can be missing from
  the port entirely without a single test turning red - z_set_height is the
  example that prompted this script. Tests answer "does what we ported behave
  like C". This answers "did we port all of it".

  What it checks:
    * Every C file in backend/ is either assigned to a module (_modules.tsv,
      c_datei, comma separated for multi-file modules) or listed as out of
      scope below. An unassigned file is invisible to the inventory, which is
      how zint_dpd and zint_upu_s10 stayed unnoticed.
    * Every C function of a module marked "done" has a row in _functions.tsv.
    * Every row naming a Pascal routine actually finds that routine in the
      Delphi unit.
    * Every "partial" or "missing" row is named in docs/ports/<module>.md, so a
      gap cannot be recorded in the inventory alone and then forgotten.
    * No row refers to a C function that no longer exists (stale after a
      reference bump).

  Modules not marked "done" are counted, not faulted, for unclassified
  functions - the inventory is filled in module by module. The other checks
  above apply to every module.

  A row saying "ported" is a human verdict, not a proof of equality. This
  script finds omissions, never wrong behaviour. For that there are the tests
  and, eventually, a differential test against the C library.

  Exit code 0 = no findings, 1 = findings (or an error).
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

  $allModules = @(Import-Csv -Path $modulesFile -Delimiter "`t")
  $modules = $allModules
  if ($Module -ne "") {
    $modules = @($allModules | Where-Object { $_.modul -eq $Module })
    if ($modules.Count -eq 0) { throw "Unknown module: $Module" }
  }

  # Rendering, file formats and the CLI - the Delphi side draws its own output.
  $outOfScope = @(
    "raster.c", "vector.c", "output.c", "filemem.c", "png.c", "svg.c", "emf.c",
    "eps.c", "ps.c", "tif.c", "gif.c", "bmp.c", "pcx.c", "zint.c", "dllversion.c"
  )

  # A C function definition: start of line, return type, name, args, brace.
  # The name may contain upper case - codablock.c has GetPossibleCharacterSet.
  $fnRegex = [regex]'(?m)^(?:INTERNAL\s+|static\s+)?[A-Za-z_][A-Za-z0-9_ \t\*]*?\b([A-Za-z_][A-Za-z0-9_]*)\s*\([^;]*?\)\s*\{'
  $notFunctions = @("if", "for", "while", "switch", "return", "sizeof", "main")

  function Get-CFiles {
    param($ModuleRow)
    return @($ModuleRow.c_datei -split '\s*,\s*' | Where-Object { $_ -ne "" })
  }

  # Returns $null only when the file does not exist; an existing file with no
  # matches returns an empty array. Conflating the two let a typo in c_datei
  # remove a done module from the check while the run stayed green.
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
    # ",@(...)" verhindert, dass PowerShell ein leeres Array beim return zu
    # $null entrollt - sonst waere die Unterscheidung oben wieder dahin
    # (sjis.h und gb2312.h sind reine Tabellen ohne Funktionsdefinition).
    return ,@($names | Sort-Object)
  }

  function Get-PascalRoutines {
    param([string]$Unit)
    $path = Join-Path $repoRoot $Unit
    if (-not (Test-Path $path)) { return $null }
    $src = [IO.File]::ReadAllText($path)
    $names = @()
    foreach ($m in [regex]::Matches($src, '(?im)^\s*(?:function|procedure)\s+([A-Za-z_]\w*)')) {
      $names += $m.Groups[1].Value.ToLower()
    }
    return ,@($names | Sort-Object -Unique)
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
      foreach ($cFile in (Get-CFiles $m)) {
        $fns = Get-CFunctions -CFile $cFile
        if ($null -eq $fns) { continue }
        foreach ($f in $fns) {
          if (-not $known.ContainsKey($cFile + "|" + $f)) {
            Write-Output ("{0}`t{1}`t?`t`t" -f $cFile, $f)
          }
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

  # --- Is every C file accounted for? --------------------------------------
  $assigned = @{}
  foreach ($m in $allModules) {
    foreach ($cFile in (Get-CFiles $m)) { $assigned[$cFile] = $m.modul }
  }
  foreach ($f in (Get-ChildItem -Path $cRoot -Filter "*.c" -File)) {
    if (-not $assigned.ContainsKey($f.Name) -and $outOfScope -notcontains $f.Name) {
      $problems += "unassigned: $($f.Name) belongs to no module in _modules.tsv and is not listed as out of scope"
    }
  }
  foreach ($m in $allModules) {
    foreach ($cFile in (Get-CFiles $m)) {
      if ($null -eq (Get-CFunctions -CFile $cFile)) {
        $problems += "$($m.modul): c_datei '$cFile' does not exist in the C reference"
      }
    }
  }

  # --- Stale rows and status values ----------------------------------------
  $cFunctionCache = @{}
  foreach ($r in $rows) {
    if (-not $cFunctionCache.ContainsKey($r.c_datei)) {
      $cFunctionCache[$r.c_datei] = Get-CFunctions -CFile $r.c_datei
    }
    $fns = $cFunctionCache[$r.c_datei]
    if ($null -eq $fns) {
      $problems += "stale: $($r.c_datei) is not in the C reference (row for $($r.c_funktion))"
    }
    elseif ($fns -notcontains $r.c_funktion) {
      $problems += "stale: $($r.c_datei):$($r.c_funktion) no longer exists in the C reference"
    }
    if ($validStatus -notcontains $r.status) {
      $problems += "bad status '$($r.status)' for $($r.c_datei):$($r.c_funktion) (allowed: $($validStatus -join ', '))"
    }
  }

  # --- Per module ------------------------------------------------------------
  foreach ($m in $modules) {
    $cFiles = Get-CFiles $m
    $fns = @()
    $anyFile = $false
    foreach ($cFile in $cFiles) {
      $f = Get-CFunctions -CFile $cFile
      if ($null -ne $f) { $anyFile = $true; $fns += $f }
    }
    if (-not $anyFile) { continue }

    $isDone = $m.status -eq "done"
    $moduleRows = @($rows | Where-Object { $cFiles -contains $_.c_datei })
    # "-" heisst: noch keine Unit, das Modul ist nicht portiert.
    $routines = @()
    if ($m.delphi_unit -ne "-" -and -not [string]::IsNullOrWhiteSpace($m.delphi_unit)) {
      $routines = Get-PascalRoutines -Unit $m.delphi_unit
      if ($null -eq $routines) {
        $problems += "$($m.modul): Delphi unit '$($m.delphi_unit)' not found"
        $routines = @()
      }
    }

    $docPath = Join-Path $repoRoot ("docs\ports\{0}.md" -f $m.modul)
    $docText = if (Test-Path $docPath) { [IO.File]::ReadAllText($docPath) } else { "" }

    $unclassified = @()
    foreach ($f in $fns) {
      if (-not ($moduleRows | Where-Object { $_.c_funktion -eq $f })) {
        $unclassified += $f
      }
    }

    foreach ($r in $moduleRows) {
      if ($r.status -eq "partial" -or $r.status -eq "missing") {
        # A gap recorded only in the inventory is a gap nobody will read.
        if ($docText -notmatch [regex]::Escape($r.c_funktion)) {
          $problems += "$($r.c_datei):$($r.c_funktion) is '$($r.status)' but is not named in docs/ports/$($m.modul).md"
        }
      }
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
      Modul       = $m.modul
      Status      = $m.status
      CFunktionen = $fns.Count
      Erfasst     = $moduleRows.Count
      Offen       = $unclassified.Count
      Luecken     = $gaps.Count
    }
  }

  if (-not $Quiet) {
    $stats | Sort-Object Offen -Descending | Format-Table -AutoSize | Out-String | Write-Host
  }

  $covered = @($stats | Where-Object { $_.Offen -eq 0 -and $_.Erfasst -gt 0 }).Count
  $gapTotal = 0
  foreach ($s in $stats) { $gapTotal += $s.Luecken }
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

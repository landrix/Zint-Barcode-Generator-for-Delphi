<#
  Finds C assertions that were dropped while porting a test case.

  Why this exists: check-c-inventory.ps1 answers "did we port all the code".
  This answers the neighbouring question "did we port all the checks". They are
  different failures. The inventory found that the port has no z_set_height;
  this script finds that test_postal.c asserts symbol->height in three test
  blocks - once per case - while Test_Postal.pas has 116 test methods and not
  one height assertion. The cases
  were ported and the assertion was left out - had it come along, the gap would
  have shown on the first run.

  How it works: for every module with a test unit, the asserted symbol fields
  of the C test file are collected - only the checked arguments, not the
  message text, or every errtxt mentioned in a failure message would count as
  checked. Each field is mapped to the token a Pascal test would have to use.
  A field that appears in no mapping, and one whose token does not occur in the
  test unit, is a finding.

  Findings fail the run only for modules with status "done". Everything else is
  reported: those modules are unfinished by definition, and a list of what they
  still owe is useful, not an error.

  Justified omissions go in scripts/check-c-assertions-ignore.txt with a
  reason. text_length is the standing example: TZintSymbol has no such field
  (docs/ports/library.md, Querschnittsdeltas).

  What this does NOT prove: that an assertion which exists is also correct, or
  that it covers the same cases as C. It finds fields nobody checks at all.

  Exit code 0 = no findings for done modules, 1 = findings (or an error).
#>
param(
  [string]$Module = "",
  [switch]$Quiet
)

$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
Push-Location $repoRoot
try {
  $cTests = Join-Path $repoRoot "Lib\zint-master-2026-03-13-b3a3c0d\backend\tests"
  if (-not (Test-Path $cTests)) {
    throw "C test suite not found: $cTests (see docs/PORTING_WORKFLOW.md, section 2b)"
  }

  $modulesFile = Join-Path $repoRoot "docs\ports\_modules.tsv"
  $modules = @(Import-Csv -Path $modulesFile -Delimiter "`t")
  if ($Module -ne "") {
    $modules = @($modules | Where-Object { $_.modul -eq $Module })
    if ($modules.Count -eq 0) { throw "Unknown module: $Module" }
  }

  # C field -> the token a Pascal test has to use to check it. Empty string
  # means the port has no counterpart at all; such fields belong on the ignore
  # list with a reason, never silently in here.
  $fieldMap = @{
    "rows"             = ".rows"
    "width"            = ".width"
    "height"           = ".height"
    "errtxt"           = "GetErrTxt"
    "text"             = "GetText"
    "text_length"      = ""
    "content_segs"     = "content_segs"
    "content_seg_count" = "content_segs_count"
    "encoded_data"     = "ModulesDump"
    "eci"              = ".eci"
    "option_1"         = ".option_1"
    "option_2"         = ".option_2"
    "option_3"         = ".option_3"
    "output_options"   = ".output_options"
    "symbology"        = ".symbology"
    "input_mode"       = ".input_mode"
    "primary"          = ".primary"
    "structapp"        = ".structapp"
  }

  $ignoreFile = Join-Path $PSScriptRoot "check-c-assertions-ignore.txt"
  $ignored = @{}
  if (Test-Path $ignoreFile) {
    foreach ($line in (Get-Content $ignoreFile)) {
      $t = $line.Trim()
      if ($t -eq "" -or $t.StartsWith("#")) { continue }
      $ignored[$t.ToLower()] = $true
    }
  }

  # Only the checked arguments count. Everything from the first string literal
  # on is the failure message: test_telepen.c prints symbol->errtxt in almost
  # every message, which would otherwise read as "errtxt is checked".
  # Befund aus dem Review: die blosse Anwesenheit eines Tokens in der Datei ist
  # keine Pruefung. Jede Testunit *setzt* option_1..3, eci, input_mode und
  # symbology - fuer diese Felder waere die Suche sonst wirkungslos. Gesucht
  # wird deshalb nur im Text der Assertionen.
  #
  # Anweisungsweise, nicht zeilenweise: die Testunits schreiben Assertionen
  # regelmaessig ueber mehrere Zeilen, der gepruefte Ausdruck steht dann erst
  # in der zweiten oder dritten. Ein Zeilenfilter verliert genau ihn.
  #
  # Vorher werden Pascal-Stringliterale entfernt. Sonst beendet ein ';' in
  # Testdaten wie 'ABC1234.;$' die Anweisung zu frueh - und ein Wort in einer
  # Meldung koennte als Pruefung durchgehen.
  function Get-AssertionText {
    param([string]$Unit)

    # Anweisungsweise, nicht zeilenweise: die Testunits schreiben Assertionen
    # regelmaessig ueber mehrere Zeilen, der gepruefte Ausdruck steht dann erst
    # in der zweiten oder dritten. Ein Zeilenfilter verliert genau ihn.
    #
    # Gescannt statt per Regex gestrippt: ein globales Entfernen der
    # Stringliterale verschluckt ganze Bereiche, sobald ein Apostroph in einem
    # Kommentar steht ("don't"), und mit ihnen die Assertionen darin. Der
    # Scanner kennt den String-Zustand und beendet eine Anweisung nur an einem
    # Semikolon ausserhalb eines Strings.
    $sb = New-Object System.Text.StringBuilder
    $i = 0
    $n = $Unit.Length
    while ($i -lt $n) {
      $hit = $Unit.IndexOf("ZAssert.", $i, [StringComparison]::OrdinalIgnoreCase)
      if ($hit -lt 0) { break }
      $j = $hit
      $inString = $false
      while ($j -lt $n) {
        $ch = $Unit[$j]
        if ($ch -eq "'") { $inString = -not $inString }
        elseif ($ch -eq ';' -and -not $inString) { break }
        $j++
      }
      [void]$sb.Append($Unit.Substring($hit, [Math]::Min($j, $n) - $hit))
      [void]$sb.Append("`n")
      $i = $j + 1
    }
    return $sb.ToString().ToLower()
  }

  function Get-AssertedFields {
    param([string]$Source)
    $fields = New-Object System.Collections.Generic.List[string]
    foreach ($m in [regex]::Matches($Source, 'assert_\w+\s*\(')) {
      $i = $m.Index + $m.Length
      $depth = 1
      $j = $i
      while ($j -lt $Source.Length -and $depth -gt 0) {
        $ch = $Source[$j]
        if ($ch -eq '(') { $depth++ }
        elseif ($ch -eq ')') { $depth-- }
        elseif ($ch -eq '"') { break }
        $j++
      }
      if ($j -le $i) { continue }
      $argsText = $Source.Substring($i, $j - $i)
      foreach ($f in [regex]::Matches($argsText, 'symbol->([a-z_0-9]+)')) {
        $name = $f.Groups[1].Value
        if (-not $fields.Contains($name)) { $fields.Add($name) }
      }
    }
    return ,@($fields | Sort-Object)
  }

  $problems = @()
  $stats = @()

  foreach ($m in $modules) {
    if ([string]::IsNullOrWhiteSpace($m.test_unit) -or $m.test_unit -eq "-") { continue }
    $unitPath = Join-Path $repoRoot ("UnitTests\{0}" -f $m.test_unit)
    if (-not (Test-Path $unitPath)) {
      $problems += "$($m.modul): test unit '$($m.test_unit)' not found"
      continue
    }
    $unit = Get-AssertionText -Unit ([IO.File]::ReadAllText($unitPath))

    $missing = @()
    $unmapped = @()
    $checked = 0
    $foundTestFile = $false
    foreach ($cFile in ($m.c_datei -split '\s*,\s*')) {
      if ($cFile -eq "") { continue }
      $base = $cFile -replace '\.[ch]$', ''
      $testFile = Join-Path $cTests ("test_{0}.c" -f $base)
      if (-not (Test-Path $testFile)) { continue }
      $foundTestFile = $true
      $src = [IO.File]::ReadAllText($testFile)
      # Kein zusaetzliches @(): die Funktion gibt das Array bereits mit ",@()"
      # gegen das Entrollen zurueck - ein weiteres @() ergaebe ein Array im Array,
      # und das erste Element waere kein Feldname, sondern die ganze Liste.
      $fields = Get-AssertedFields -Source $src
      # C vergleicht das Modulmuster ueber testUtilModulesCmp(symbol, ...) -
      # im Assert-Argument steht kein symbol->encoded_data. Ohne diesen Zusatz
      # waere die wichtigste C-Pruefung ausserhalb des Sichtfelds.
      if ($src -match 'testUtilModulesCmp|testUtilModulesDump') {
        if ($fields -notcontains "encoded_data") { $fields += "encoded_data" }
      }
      foreach ($field in $fields) {
        if ($ignored.ContainsKey(("{0}:{1}" -f $m.modul, $field).ToLower())) { continue }
        if (-not $fieldMap.ContainsKey($field)) {
          if ($unmapped -notcontains $field) { $unmapped += $field }
          continue
        }
        $token = $fieldMap[$field]
        if ($token -eq "") {
          if ($missing -notcontains $field) { $missing += $field }
          continue
        }
        if ($unit.Contains($token.ToLower())) { $checked++ }
        elseif ($missing -notcontains $field) { $missing += $field }
      }
    }

    $stats += [pscustomobject]@{
      Modul    = $m.modul
      Status   = $m.status
      Geprueft = $checked
      Fehlend  = $missing.Count
      Felder   = ($missing + ($unmapped | ForEach-Object { $_ + "?" })) -join ", "
    }

    # "nichts gefunden" und "alles gedeckt" duerfen nicht gleich aussehen.
    if (-not $foundTestFile) {
      $problems += ("{0} has a test unit but no C test file was found for c_datei '{1}' - nothing was compared" -f
        $m.modul, $m.c_datei)
    }

    if ($m.status -eq "done" -and ($missing.Count -gt 0 -or $unmapped.Count -gt 0)) {
      $problems += ("{0} is marked done but C asserts field(s) the test unit never checks: {1}" -f
        $m.modul, (($missing + $unmapped) -join ", "))
    }
  }

  if (-not $Quiet) {
    foreach ($row in (@($stats | Where-Object { $_.Felder -ne "" }) | Sort-Object Fehlend -Descending)) {
      Write-Host ("{0,-12} {1,-8} geprueft {2,3}   fehlend: {3}" -f
        $row.Modul, $row.Status, $row.Geprueft, $row.Felder)
    }
    Write-Host ""
  }

  $clean = @($stats | Where-Object { $_.Felder -eq "" }).Count
  Write-Host ("check-c-assertions: {0} of {1} test units check every field C asserts." -f $clean, $stats.Count)

  if ($problems.Count -gt 0) {
    Write-Host ""
    foreach ($p in $problems) { Write-Host ("  " + $p) }
    Write-Host ""
    throw ("{0} module(s) marked done drop assertions that exist in C. Port them, or record the reason in scripts/check-c-assertions-ignore.txt." -f $problems.Count)
  }
}
finally {
  Pop-Location
}

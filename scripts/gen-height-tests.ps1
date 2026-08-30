<#
.SYNOPSIS
  Erzeugt UnitTests/Test_Height.pas aus den Tabellen test_height und
  test_height_per_row in test_vector.c.

.DESCRIPTION
  test_vector.c ist upstream die einzige Stelle, an der die Hoehenlogik je
  Symbologie geprueft wird - die Modul-Testdateien (test_postal.c usw.) tun es
  fast gar nicht. Uebernommen werden symbology, output_options, input_mode,
  height, data, ret, height, rows und width; die beiden Vektor-Spalten der
  C-Tabellen bleiben weg, weil der Port keinen Vektor-Renderer prueft.

  Aufgenommen werden nur die Symbologien, deren Hoehenlogik portiert ist
  (Liste unten). Wird eine weitere Hoehenlogik portiert, gehoert die
  Symbologie hier dazu und die Datei wird neu erzeugt.

  Nach einem Wechsel der C-Referenz erneut laufen lassen.
#>
[CmdletBinding()]
param(
  [string]$CRoot,
  [string]$OutFile
)

$ErrorActionPreference = 'Stop'
$repoRoot = Split-Path -Parent $PSScriptRoot
if (-not $CRoot) { $CRoot = Join-Path $repoRoot 'Lib/zint-master-2026-03-13-b3a3c0d' }
if (-not $OutFile) { $OutFile = Join-Path $repoRoot 'UnitTests/Test_Height.pas' }

# Symbologien, deren Hoehenlogik im Port vorhanden ist.
$Wanted = @(
  'C25INTER', 'ITF14', 'DPLEIT', 'DPIDENT', 'CODE39', 'EXCODE39', 'LOGMARS',
  'HIBC_39', 'CODE93', 'PHARMA', 'PHARMA_TWO', 'CODE32', 'PZN', 'POSTNET',
  'PLANET', 'CEPNET', 'RM4SCC', 'KIX', 'DAFT', 'JAPANPOST', 'AUSPOST',
  'AUSREPLY', 'AUSROUTE', 'AUSREDIRECT', 'FIM', 'TELEPEN', 'TELEPEN_NUM',
  'GS1_128', 'EAN128', 'EAN14', 'NVE18'
)
# Upstream hat EAN128 in GS1_128 umbenannt; der Port fuehrt noch den alten Namen.
$Alias = @{ 'GS1_128' = 'EAN128' }

# Nur PHARMA_TWO erreicht im Port den HEIGHTPERROW_MODE-Zweig von set_height:
# zwei Zeilen ohne vorgegebene row_height. Alles andere in test_height_per_row
# sind mehrzeilige Symbologien, deren Hoehenlogik hier nicht portiert ist.
$WantedPerRow = @('PHARMA_TWO')

$RetMap = @{ '0' = '0'; 'ZINT_WARN_NONCOMPLIANT' = 'ZWARN_NONCOMPLIANT' }

$src = Get-Content -LiteralPath (Join-Path $CRoot 'backend/tests/test_vector.c') -Raw

function Get-Table {
  param([string]$Text, [string]$FuncName)
  $start = $Text.IndexOf("static void $FuncName(const testCtx", [StringComparison]::Ordinal)
  if ($start -lt 0) { throw "Funktion $FuncName nicht in test_vector.c gefunden" }
  $body = $Text.Substring($start)
  $dataAt = $body.IndexOf('struct item data[] = {', [StringComparison]::Ordinal)
  if ($dataAt -lt 0) { throw "Datentabelle von $FuncName nicht gefunden" }
  $body = $body.Substring($dataAt)
  $endAt = $body.IndexOf("`n    };", [StringComparison]::Ordinal)
  if ($endAt -lt 0) { throw "Ende der Datentabelle von $FuncName nicht gefunden" }
  return $body.Substring(0, $endAt)
}

function ConvertTo-PasFloat {
  param([string]$Value)
  if ($Value -match '[.eE]') { return $Value }
  return "$Value.0"
}

function ConvertTo-PasString {
  param([string]$Value)
  # In den beiden Tabellen kommen nur \\ und \" als Escapes vor.
  $v = $Value -replace '\\\\', '\' -replace '\\"', '"'
  return "'" + ($v -replace "'", "''") + "'"
}

# --- Tabelle test_height ----------------------------------------------------
$rowRe = '/\*\s*(?<i>\d+)\*/\s*\{\s*BARCODE_(?<sym>\w+)\s*,\s*(?<opts>[^,]+?)\s*,' +
         '\s*(?<h>[-\d.]+)\s*,\s*"(?<data>(?:[^"\\]|\\.)*)"\s*,' +
         '\s*"(?<cc>(?:[^"\\]|\\.)*)"\s*,\s*(?<ret>[A-Z_0-9]+)\s*,' +
         '\s*(?<eh>[-\d.]+)\s*,\s*(?<er>\d+)\s*,\s*(?<ew>\d+)\s*,'

$table = Get-Table -Text $src -FuncName 'test_height'
$rows = @()
$skippedComposite = 0
$skippedOther = @()
foreach ($m in [regex]::Matches($table, $rowRe)) {
  $sym = $m.Groups['sym'].Value
  if ($Wanted -notcontains $sym) { continue }
  if ($m.Groups['cc'].Value) { $skippedComposite++; continue }
  $ret = $m.Groups['ret'].Value
  $opts = $m.Groups['opts'].Value
  if (-not $RetMap.ContainsKey($ret)) { $skippedOther += "C#$($m.Groups['i'].Value) ret=$ret"; continue }
  if ($opts -ne '-1' -and $opts -ne 'COMPLIANT_HEIGHT') {
    $skippedOther += "C#$($m.Groups['i'].Value) opts=$opts"; continue
  }
  $name = $sym
  if ($Alias.ContainsKey($sym)) { $name = $Alias[$sym] }
  $rows += [pscustomobject]@{
    Index = [int]$m.Groups['i'].Value
    Sym   = $name
    Opts  = $opts
    H     = $m.Groups['h'].Value
    Data  = $m.Groups['data'].Value
    Ret   = $RetMap[$ret]
    ExpH  = $m.Groups['eh'].Value
    ExpR  = $m.Groups['er'].Value
    ExpW  = $m.Groups['ew'].Value
  }
}
if ($rows.Count -eq 0) { throw 'keine Zeilen aus test_height erkannt' }

# Gegenprobe: keine Zeile darf still durchfallen, weil der Ausdruck sie
# nicht trifft. Genau so entsteht sonst eine Luecke, die niemand sieht.
$matchedIdx = @{}
foreach ($r in $rows) { $matchedIdx[$r.Index] = $true }
foreach ($line in ($table -split "`n")) {
  if ($line -match '/\*\s*(?<i>\d+)\*/\s*\{\s*BARCODE_(?<sym>\w+)\s*,') {
    $i = [int]$Matches['i']
    if (($Wanted -contains $Matches['sym']) -and -not $matchedIdx.ContainsKey($i)) {
      $known = $skippedOther -join ', '
      if ($skippedComposite -eq 0 -and -not $known) {
        throw "test_height C#$i wurde vom Ausdruck nicht erfasst: $($line.Trim())"
      }
    }
  }
}

# --- Tabelle test_height_per_row -------------------------------------------
$prRe = '/\*\s*(?<i>\d+)\*/\s*\{\s*BARCODE_(?<sym>\w+)\s*,\s*(?<mode>-1|HEIGHTPERROW_MODE)\s*,' +
        '\s*(?<o1>-?\d+)\s*,\s*(?<o2>-?\d+)\s*,\s*(?<o3>-?\d+)\s*,' +
        '\s*(?<h>[-\d.]+)\s*,\s*(?<scale>[-\d.]+)\s*,\s*"(?<data>(?:[^"\\]|\\.)*)"\s*,' +
        '\s*"(?<cc>(?:[^"\\]|\\.)*)"\s*,\s*(?<ret>[A-Z_0-9]+)\s*,' +
        '\s*(?<eh>[-\d.]+)\s*,\s*(?<er>\d+)\s*,\s*(?<ew>\d+)\s*,'

$prTable = Get-Table -Text $src -FuncName 'test_height_per_row'
$prRows = @()
foreach ($m in [regex]::Matches($prTable, $prRe)) {
  $sym = $m.Groups['sym'].Value
  if ($WantedPerRow -notcontains $sym) { continue }
  if ($m.Groups['cc'].Value) { continue }
  if ($m.Groups['o1'].Value -ne '-1' -or $m.Groups['o2'].Value -ne '-1' -or
      $m.Groups['o3'].Value -ne '-1') {
    throw "test_height_per_row C#$($m.Groups['i'].Value) setzt option_1/2/3 - nicht abgebildet"
  }
  $ret = $m.Groups['ret'].Value
  if (-not $RetMap.ContainsKey($ret)) {
    throw "test_height_per_row C#$($m.Groups['i'].Value) ret=$ret - nicht abgebildet"
  }
  $prRows += [pscustomobject]@{
    Index = [int]$m.Groups['i'].Value
    Sym   = $sym
    Mode  = $m.Groups['mode'].Value
    H     = $m.Groups['h'].Value
    Data  = $m.Groups['data'].Value
    Ret   = $RetMap[$ret]
    ExpH  = $m.Groups['eh'].Value
    ExpR  = $m.Groups['er'].Value
    ExpW  = $m.Groups['ew'].Value
  }
}
if ($prRows.Count -eq 0) { throw 'keine Zeilen aus test_height_per_row erkannt' }

# --- Ausgabe ----------------------------------------------------------------
$order = @()
foreach ($r in $rows) { if ($order -notcontains $r.Sym) { $order += $r.Sym } }
$prOrder = @()
foreach ($r in $prRows) { if ($prOrder -notcontains $r.Sym) { $prOrder += $r.Sym } }

$out = New-Object System.Collections.Generic.List[string]
$out.Add(@'
unit Test_Height;

{$I zint_test.inc}

{
  DUnitX-Tests fuer die Hoehenlogik (z_set_height und die modulspezifischen
  Varianten).

  Testdaten aus test_height und test_height_per_row in test_vector.c
  (Zint commit b3a3c0d, 2026-03-13). Das ist upstream die einzige Stelle,
  an der die Hoehenlogik je Symbologie geprueft wird - die Modul-Testdateien
  tun es fast gar nicht.

  Uebernommen sind symbology, output_options, input_mode, height, data, ret,
  height, rows und width. Die beiden Vektor-Spalten der C-Tabellen bleiben
  weg, weil der Port keinen Vektor-Renderer prueft; show_hrt setzt C auf 0,
  der Port kennt das Feld nicht und auf die Hoehe wirkt es nicht.

  ERZEUGT von scripts/gen-height-tests.ps1 - Aenderungen von Hand gehen beim
  naechsten Lauf verloren.
}

interface

uses
  {$IFNDEF FPC}DUnitX.TestFramework,{$ENDIF}
  TestFramework_Zint,
  SysUtils,
  TestHelper_Zint,
  zint_helper,
  zint;

type
  [TestFixture]
  TTestHeight = class(TZintFixture)
  private
    procedure CheckCase(ACIndex, ASymbology, AOutputOptions, AInputMode: Integer;
      const AHeight: Single; const AData: String; ARet: Integer;
      const AExpHeight: Single; AExpRows, AExpWidth: Integer);
  published
'@)

foreach ($sym in $order) { $out.Add("    [Test] procedure Height_$sym;") }
$out.Add('')
$out.Add('    { test_height_per_row: HEIGHTPERROW_MODE }')
foreach ($sym in $prOrder) { $out.Add("    [Test] procedure HeightPerRow_$sym;") }

$out.Add(@'
  end;

implementation

procedure TTestHeight.CheckCase(ACIndex, ASymbology, AOutputOptions, AInputMode: Integer;
  const AHeight: Single; const AData: String; ARet: Integer;
  const AExpHeight: Single; AExpRows, AExpWidth: Integer);
var
  sym: TZintSymbol;
  ctx: String;
begin
  ctx := Format('C#%d', [ACIndex]);
  sym := TZintTestHelper.CreateSymbol(ASymbology);
  try
    sym.input_mode := UNICODE_MODE;
    if AInputMode >= 0 then
      sym.input_mode := sym.input_mode or AInputMode;
    if AOutputOptions >= 0 then
      sym.output_options := AOutputOptions;
    if AHeight >= 0 then
      sym.height := AHeight;
    ZAssert.AreEqual(ARet, TZintTestHelper.EncodeData(sym, AData),
      ctx + ' ret (' + TZintTestHelper.GetErrTxt(sym) + ')');
    ZAssert.AreEqual(AExpHeight, sym.height, ctx + ' height');
    ZAssert.AreEqual(AExpRows, sym.rows, ctx + ' rows');
    ZAssert.AreEqual(AExpWidth, sym.width, ctx + ' width');
  finally
    sym.Free;
  end;
end;
'@)

foreach ($sym in $order) {
  $out.Add("procedure TTestHeight.Height_$sym;")
  $out.Add('begin')
  foreach ($r in ($rows | Where-Object { $_.Sym -eq $sym })) {
    $opts = if ($r.Opts -eq '-1') { '-1' } else { 'COMPLIANT_HEIGHT' }
    $out.Add(("  CheckCase({0}, BARCODE_{1}, {2}, -1, {3}, {4}, {5}, {6}, {7}, {8});" -f
      $r.Index, $sym, $opts, (ConvertTo-PasFloat $r.H), (ConvertTo-PasString $r.Data),
      $r.Ret, (ConvertTo-PasFloat $r.ExpH), $r.ExpR, $r.ExpW))
  }
  $out.Add('end;')
  $out.Add('')
}

foreach ($sym in $prOrder) {
  $out.Add("procedure TTestHeight.HeightPerRow_$sym;")
  $out.Add('begin')
  foreach ($r in ($prRows | Where-Object { $_.Sym -eq $sym })) {
    $mode = if ($r.Mode -eq '-1') { '-1' } else { 'HEIGHTPERROW_MODE' }
    $out.Add(("  CheckCase({0}, BARCODE_{1}, -1, {2}, {3}, {4}, {5}, {6}, {7}, {8});" -f
      $r.Index, $sym, $mode, (ConvertTo-PasFloat $r.H), (ConvertTo-PasString $r.Data),
      $r.Ret, (ConvertTo-PasFloat $r.ExpH), $r.ExpR, $r.ExpW))
  }
  $out.Add('end;')
  $out.Add('')
}

$out.Add(@'
initialization
  ZRegisterFixture(TTestHeight);

end.
'@)

$text = ($out -join "`r`n")
if (-not $text.EndsWith("`r`n")) { $text += "`r`n" }
$outDir = (Resolve-Path -LiteralPath (Split-Path $OutFile -Parent)).Path
$outPath = Join-Path $outDir (Split-Path $OutFile -Leaf)
[System.IO.File]::WriteAllText($outPath, $text, (New-Object System.Text.UTF8Encoding($false)))

Write-Host ("gen-height-tests: {0} Faelle aus test_height in {1} Methoden, {2} aus test_height_per_row in {3}" -f
  $rows.Count, $order.Count, $prRows.Count, $prOrder.Count)
if ($skippedComposite -gt 0) {
  Write-Host "  uebersprungen (Composite-Faelle, im Port nicht abgebildet): $skippedComposite"
}
if ($skippedOther.Count -gt 0) {
  Write-Host ("  uebersprungen (sonstiges): {0}" -f ($skippedOther -join ', '))
}

<#
.SYNOPSIS
  Erzeugt docs/ports/README.md neu aus docs/ports/_modules.tsv.

.DESCRIPTION
  Die Uebersicht wird beim Merge auf develop gepflegt, nicht im Feature-Branch.
  Wenn ein Modul den Status wechselt, in _modules.tsv aendern und dieses Skript
  laufen lassen.

  Statuswerte: done | partial | legacy | missing
#>
param(
  [switch]$Check
)

$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
$tsv      = Join-Path $repoRoot "docs\ports\_modules.tsv"
$outFile  = Join-Path $repoRoot "docs\ports\README.md"

if (-not (Test-Path $tsv)) { throw "Nicht gefunden: $tsv" }

$modules = Import-Csv -Path $tsv -Delimiter "`t"

$groups = [ordered]@{
  "done"    = "## OK - portiert und getestet"
  "partial" = "## TEIL - Teilport mit dokumentierten Deltas"
  "legacy"  = "## ALT - Legacy-Port ohne b3a3c0d-Verifikation"
  "missing" = "## FEHLT - noch nicht portiert"
}

$sb = [System.Text.StringBuilder]::new()
[void]$sb.AppendLine("# Portierungsstand pro Modul")
[void]$sb.AppendLine()
[void]$sb.AppendLine("Eine Datei je Modul. So kollidieren parallele Aenderungen nicht mehr in einer")
[void]$sb.AppendLine("gemeinsamen Statusdatei. Ablauf und Regeln: [PORTING_WORKFLOW.md](../PORTING_WORKFLOW.md).")
[void]$sb.AppendLine()
[void]$sb.AppendLine("Diese Uebersicht wird **beim Merge auf develop** gepflegt, nicht im Feature-Branch.")
[void]$sb.AppendLine("Neu erzeugen mit ``scripts\gen-port-index.ps1``.")
[void]$sb.AppendLine()
[void]$sb.AppendLine("Legende: OK = portiert (b3a3c0d) + Tests gruen | TEIL = Teilport mit dokumentierten")
[void]$sb.AppendLine("Deltas | ALT = Legacy-Port, nicht gegen b3a3c0d verifiziert | FEHLT = nicht portiert")
[void]$sb.AppendLine()

foreach ($status in $groups.Keys) {
  [void]$sb.AppendLine($groups[$status])
  [void]$sb.AppendLine()
  [void]$sb.AppendLine("| Modul | Beschreibung | Delphi-Unit | Tests |")
  [void]$sb.AppendLine("|---|---|---|---|")

  foreach ($m in $modules | Where-Object { $_.status -eq $status }) {
    $unit = if ($m.delphi_unit -eq "-") { "-" } else { "``$($m.delphi_unit)``" }
    $test = if ($m.test_unit   -eq "-") { "-" } else { "``$($m.test_unit)``" }
    [void]$sb.AppendLine("| [$($m.modul)]($($m.modul).md) | $($m.beschreibung) | $unit | $test |")
  }
  [void]$sb.AppendLine()
}

$new = $sb.ToString()

if ($Check) {
  $old = if (Test-Path $outFile) { Get-Content -Raw -Path $outFile } else { "" }
  if ($old -ne $new) {
    Write-Error "docs/ports/README.md ist nicht aktuell. 'scripts\gen-port-index.ps1' ausfuehren."
    exit 1
  }
  Write-Host "docs/ports/README.md ist aktuell."
  exit 0
}

Set-Content -Path $outFile -Value $new -NoNewline -Encoding UTF8
Write-Host ("Geschrieben: {0} ({1} Module)" -f $outFile, $modules.Count)

# Fehlende Moduldateien melden
foreach ($m in $modules) {
  $f = Join-Path $repoRoot ("docs\ports\{0}.md" -f $m.modul)
  if (-not (Test-Path $f)) { Write-Warning ("Moduldatei fehlt: docs/ports/{0}.md" -f $m.modul) }
}

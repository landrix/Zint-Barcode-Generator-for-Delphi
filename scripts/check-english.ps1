<#
  Sucht deutschsprachige Stellen in allen versionierten Textdateien.

  Zweck: Vor dem Merge nach main muss alles Deutsche uebersetzt sein
  (docs/PORTING_WORKFLOW.md, Abschnitt 2a). Dieses Skript findet, was noch
  offen ist - es ersetzt keine Durchsicht, aber es verhindert, dass eine ganze
  Datei uebersehen wird.

  Exitcode 0 = nichts gefunden, 1 = Treffer (oder Fehler).

  Erkannt wird an zwei Signalen:
    1. Umlaute und Eszett - eindeutig, praktisch keine Fehltreffer.
    2. Deutsche Funktionswoerter als ganze Woerter. Bewusst konservativ
       gewaehlt: nur Woerter, die weder englisch noch Pascal-Schluesselwort
       noch plausibler Bezeichner sind.

  Fehltreffer sind moeglich (Barcode-Testdaten, Eigennamen). Dafuer gibt es
  scripts/check-english-ignore.txt: eine Glob-Zeile je Ausnahme.

  Dieses Skript prueft sich nicht selbst - seine Wortliste besteht
  naturgemaess aus deutschen Woertern.
#>
param(
  [string]$Path = "",
  [int]$MaxPerFile = 5,
  [switch]$All,
  [switch]$Quiet
)

$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
Push-Location $repoRoot
try {
  # Nur versionierte Dateien - Build-Ausgaben und die C-Referenz bleiben aussen vor.
  $tracked = & git ls-files
  if ($LASTEXITCODE -ne 0) {
    throw "git ls-files failed - is this a git repository?"
  }

  $textExtensions = @(
    ".md", ".txt", ".tsv", ".csv",
    ".pas", ".inc", ".dpr", ".lpr", ".dfm",
    ".ps1", ".psm1", ".bat", ".cmd", ".sh",
    ".yml", ".yaml", ".json", ".xml", ".lpi", ".gitignore", ".gitmodules"
  )

  # Fest ausgenommen: fremder Code und dieses Skript selbst.
  $builtinIgnore = @(
    "Lib/*",
    "scripts/check-english.ps1",
    "scripts/check-english-ignore.txt"
  )

  $ignoreFile = Join-Path $PSScriptRoot "check-english-ignore.txt"
  $userIgnore = @()
  if (Test-Path $ignoreFile) {
    $userIgnore = Get-Content $ignoreFile |
      Where-Object { $_.Trim() -ne "" -and -not $_.TrimStart().StartsWith("#") } |
      ForEach-Object { $_.Trim() }
  }
  $ignore = $builtinIgnore + $userIgnore

  # @() erzwingt ein Array: bei genau einem Treffer liefert die Pipeline sonst
  # ein Einzelobjekt, dessen .Count nicht die Anzahl der Dateien ist.
  $files = @($tracked | Where-Object {
    $f = $_
    $ext = [IO.Path]::GetExtension($f)
    if ($ext -eq "" ) { $ext = [IO.Path]::GetFileName($f) }
    if ($textExtensions -notcontains $ext) { return $false }
    foreach ($pattern in $ignore) { if ($f -like $pattern) { return $false } }
    return $true
  })

  if ($Path -ne "") {
    $files = @($files | Where-Object { $_ -like $Path })
  }

  # Ganze Woerter, die im Englischen nicht vorkommen und keine Bezeichner sind.
  $germanWords = @(
    "und", "oder", "nicht", "kein", "keine", "keinen", "keiner",
    "wurde", "wurden", "werden", "wird", "muss", "muessen", "soll", "sollen",
    "dann", "wenn", "weil", "damit", "dass", "ohne", "gegen", "zwischen",
    "jeder", "jede", "jeden", "alle", "allen", "alles",
    "eine", "einen", "einem", "eines", "einer",
    "dieser", "diese", "dieses", "diesem", "diesen",
    "durch", "beim", "vom", "zum", "zur", "nach", "unter", "ueber",
    "sich", "ihre", "seine", "noch", "immer", "bereits", "jetzt", "siehe",
    "hier", "dort", "sind", "haben", "wegen", "bevor", "waehrend",
    "gruen", "fehlt", "fehlen", "abweichung", "aenderung", "datei", "dateien",
    "zeile", "zeilen", "zeichen", "laenge", "gilt", "gelten", "erst", "erste"
  )
  $wordPattern = "(?i)\b(" + ($germanWords -join "|") + ")\b"
  $umlautPattern = "[äöüÄÖÜß]"

  $strictUtf8 = New-Object System.Text.UTF8Encoding($false, $true)
  $cp1252 = [System.Text.Encoding]::GetEncoding(1252)

  $hits = @()
  foreach ($file in $files) {
    $bytes = [IO.File]::ReadAllBytes((Join-Path $repoRoot $file))
    try {
      $text = $strictUtf8.GetString($bytes)
    } catch {
      # Nicht jede Quelldatei ist UTF-8; zint_qr_epc.pas ist cp1252.
      $text = $cp1252.GetString($bytes)
    }

    $lineNo = 0
    foreach ($line in ($text -split "\r?\n")) {
      $lineNo++
      $reason = ""
      if ($line -cmatch $umlautPattern) {
        $reason = "Umlaut"
      } elseif ($line -match $wordPattern) {
        $reason = "Wort '" + $Matches[1] + "'"
      }
      if ($reason -ne "") {
        $hits += [pscustomobject]@{
          File   = $file
          Line   = $lineNo
          Reason = $reason
          Text   = $line.Trim()
        }
      }
    }
  }

  if ($hits.Count -eq 0) {
    Write-Host "check-english: no German content found in $($files.Count) tracked text files."
    return
  }

  $byFile = @($hits | Group-Object File | Sort-Object Count -Descending)

  if (-not $Quiet) {
    foreach ($group in $byFile) {
      Write-Host ""
      Write-Host ("{0}  ({1} Zeile(n))" -f $group.Name, $group.Count)
      $shown = if ($All) { $group.Group } else { $group.Group | Select-Object -First $MaxPerFile }
      foreach ($hit in $shown) {
        $snippet = $hit.Text
        if ($snippet.Length -gt 90) { $snippet = $snippet.Substring(0, 90) + "..." }
        Write-Host ("  {0,5}: [{1}] {2}" -f $hit.Line, $hit.Reason, $snippet)
      }
      if (-not $All -and $group.Count -gt $MaxPerFile) {
        Write-Host ("        ... {0} weitere (-All zeigt alle)" -f ($group.Count - $MaxPerFile))
      }
    }
    Write-Host ""
  }

  Write-Host ("check-english: {0} German line(s) in {1} of {2} files." -f $hits.Count, $byFile.Count, $files.Count)
  Write-Host "Translate them before merging into main (docs/PORTING_WORKFLOW.md, section 2a)."
  exit 1
}
finally {
  Pop-Location
}

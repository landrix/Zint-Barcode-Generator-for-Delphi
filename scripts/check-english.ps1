<#
  Finds German text in all tracked text files.

  Purpose: before merging into main everything German must be translated
  (docs/PORTING_WORKFLOW.md, section 2a). This script finds what is left. It
  does not replace reading the diff - it prevents overlooking a whole file.

  Exit code 0 = nothing found, 1 = hits (or an error).

  Two signals:
    1. Umlauts and eszett - unambiguous, practically no false positives.
    2. German function and content words, whole words, from
       scripts/check-english-words.txt.

  The word list cannot be complete: a German line built only from unlisted
  words passes. Treat a clean run as "no whole file was forgotten", not as
  proof that the repository is English.

  This file is written in English and is ASCII only on purpose. It scans
  itself, and PowerShell 5.1 reads a BOM-less file as ANSI - a literal umlaut
  in the source would break parsing there. Umlauts are therefore built from
  character codes.

  Only scripts/check-english-words.txt is excluded from the scan; it consists
  of German words by definition. False positives elsewhere (test data, proper
  names) belong in scripts/check-english-ignore.txt with a reason.
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
  # Tracked files only - build output and the C reference stay out.
  $tracked = & git ls-files
  if ($LASTEXITCODE -ne 0) {
    throw "git ls-files failed - is this a git repository?"
  }

  # Every versioned format that can carry prose. Adding an extension is
  # cheap; a missing one makes the check silently blind to those files.
  $textExtensions = @(
    ".md", ".txt", ".tsv", ".csv", ".svg",
    ".pas", ".inc", ".dpr", ".lpr", ".dfm", ".fmx", ".lfm",
    ".dproj", ".groupproj", ".bdsproj", ".lpi", ".lps",
    ".ps1", ".psm1", ".bat", ".cmd", ".sh",
    ".yml", ".yaml", ".json", ".xml", ".gitignore", ".gitmodules"
  )

  # Foreign code, and the word list itself.
  $builtinIgnore = @(
    "Lib/*",
    "scripts/check-english-words.txt"
  )

  $ignoreFile = Join-Path $PSScriptRoot "check-english-ignore.txt"
  $userIgnore = @()
  if (Test-Path $ignoreFile) {
    $userIgnore = @(Get-Content $ignoreFile |
      Where-Object { $_.Trim() -ne "" -and -not $_.TrimStart().StartsWith("#") } |
      ForEach-Object { $_.Trim() })
  }
  $ignore = $builtinIgnore + $userIgnore

  # @() forces an array: with a single match the pipeline yields one object,
  # whose .Count is not the number of files.
  $files = @($tracked | Where-Object {
    $f = $_
    $ext = [IO.Path]::GetExtension($f)
    # No extension (LICENSE, Makefile, Dockerfile) means "scan it": such files
    # carry prose more often than not, and a binary one is caught below.
    if ($ext -ne "" -and $textExtensions -notcontains $ext) { return $false }
    foreach ($pattern in $ignore) { if ($f -like $pattern) { return $false } }
    return $true
  })

  if ($Path -ne "") {
    $filtered = @($files | Where-Object { $_ -like $Path })
    if ($filtered.Count -eq 0) {
      # Silently reporting success for a typo would be the worst outcome.
      throw ("-Path '{0}' matches none of the {1} scanned files." -f $Path, $files.Count)
    }
    $files = $filtered
  }

  $wordFile = Join-Path $PSScriptRoot "check-english-words.txt"
  if (-not (Test-Path $wordFile)) {
    throw "Word list not found: $wordFile"
  }
  $germanWords = @(Get-Content $wordFile |
    ForEach-Object { $_.Trim() } |
    Where-Object { $_ -ne "" -and -not $_.StartsWith("#") })
  if ($germanWords.Count -eq 0) {
    throw "Word list $wordFile is empty - the check would pass on anything."
  }
  $wordPattern = "(?i)\b(" + ($germanWords -join "|") + ")\b"

  # Built from code points so this file stays pure ASCII (see header).
  $umlautChars = @(0xE4, 0xF6, 0xFC, 0xC4, 0xD6, 0xDC, 0xDF) |
    ForEach-Object { [char]$_ }
  $umlautPattern = "[" + ($umlautChars -join "") + "]"

  $strictUtf8 = New-Object System.Text.UTF8Encoding($false, $true)
  $cp1252 = [System.Text.Encoding]::GetEncoding(1252)

  $hits = @()
  $binary = @()
  foreach ($file in $files) {
    $bytes = [IO.File]::ReadAllBytes((Join-Path $repoRoot $file))

    # A NUL byte outside UTF-16 means binary (Delphi .dfm can be binary, and an
    # extensionless file can be anything). Skipping is fine - reporting the
    # skip is not optional, or the "no file forgotten" claim would be hollow.
    $hasBom16 = $bytes.Length -ge 2 -and (
      ($bytes[0] -eq 0xFF -and $bytes[1] -eq 0xFE) -or
      ($bytes[0] -eq 0xFE -and $bytes[1] -eq 0xFF))
    if (-not $hasBom16) {
      $probe = [Math]::Min($bytes.Length, 8192)
      $isBinary = $false
      for ($i = 0; $i -lt $probe; $i++) {
        if ($bytes[$i] -eq 0) { $isBinary = $true; break }
      }
      if ($isBinary) {
        $binary += $file
        continue
      }
    }

    if ($bytes.Length -ge 2 -and $bytes[0] -eq 0xFF -and $bytes[1] -eq 0xFE) {
      $text = [System.Text.Encoding]::Unicode.GetString($bytes, 2, $bytes.Length - 2)
    } elseif ($bytes.Length -ge 2 -and $bytes[0] -eq 0xFE -and $bytes[1] -eq 0xFF) {
      $text = [System.Text.Encoding]::BigEndianUnicode.GetString($bytes, 2, $bytes.Length - 2)
    } else {
      try {
        # Strict: invalid UTF-8 throws instead of yielding replacement chars.
        $text = $strictUtf8.GetString($bytes)
      } catch {
        # Not every source file is UTF-8; zint_qr_epc.pas is cp1252.
        $text = $cp1252.GetString($bytes)
      }
    }

    $lineNo = 0
    foreach ($line in ($text -split "\r?\n")) {
      $lineNo++
      $reason = ""
      if ($line -cmatch $umlautPattern) {
        $reason = "umlaut"
      } elseif ($line -match $wordPattern) {
        $reason = "word '" + $Matches[1] + "'"
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

  if ($binary.Count -gt 0) {
    Write-Host ("check-english: skipped {0} binary file(s): {1}" -f $binary.Count, ($binary -join ", "))
  }

  if ($hits.Count -eq 0) {
    Write-Host ("check-english: no German content found in {0} tracked text files." -f ($files.Count - $binary.Count))
    return
  }

  $byFile = @($hits | Group-Object File | Sort-Object Count -Descending)

  if (-not $Quiet) {
    foreach ($group in $byFile) {
      Write-Host ""
      Write-Host ("{0}  ({1} line(s))" -f $group.Name, $group.Count)
      $shown = if ($All) { $group.Group } else { $group.Group | Select-Object -First $MaxPerFile }
      foreach ($hit in $shown) {
        $snippet = $hit.Text
        if ($snippet.Length -gt 90) { $snippet = $snippet.Substring(0, 90) + "..." }
        Write-Host ("  {0,5}: [{1}] {2}" -f $hit.Line, $hit.Reason, $snippet)
      }
      if (-not $All -and $group.Count -gt $MaxPerFile) {
        Write-Host ("        ... {0} more (-All shows every line)" -f ($group.Count - $MaxPerFile))
      }
    }
    Write-Host ""
  }

  Write-Host ("check-english: {0} German line(s) in {1} of {2} files." -f $hits.Count, $byFile.Count, ($files.Count - $binary.Count))
  Write-Host "Translate them before merging into main (docs/PORTING_WORKFLOW.md, section 2a)."
  exit 1
}
finally {
  Pop-Location
}

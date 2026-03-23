unit TestHelper_Zint;

{
  Test-Infrastruktur fuer Zint-Barcode-Generator Unit-Tests.
  Stellt Hilfsfunktionen bereit, die das Testen der TZintSymbol-Klasse
  vereinfachen und an das C-Testframework (testcommon.c) angelehnt sind.
}

interface

uses
  System.SysUtils, zint, zint_common, zint_helper;

type
  TEncodeTestItem = record
    Index: Integer;
    Symbology: Integer;
    InputMode: Integer;
    Option1: Integer;
    Option2: Integer;
    Option3: Integer;
    OutputOptions: Integer;
    Data: String;
    DataLen: Integer;       // -1 = verwende Length(Data)
    ExpectedResult: Integer; // 0 = OK, 2 = Warn, >=5 = Error
    ExpectedRows: Integer;   // -1 = nicht pruefen
    ExpectedWidth: Integer;  // -1 = nicht pruefen
    ExpectedErrTxt: String;  // '' = nicht pruefen
    Comment: String;
  end;

  TZintTestHelper = class
  public
    /// <summary>Erzeugt ein neues TZintSymbol mit dem angegebenen Symbology-Typ</summary>
    class function CreateSymbol(ASymbology: Integer): TZintSymbol;

    /// <summary>Konfiguriert ein Symbol analog zu testUtilSetSymbol() in C</summary>
    class procedure SetupSymbol(ASymbol: TZintSymbol;
      ASymbology, AInputMode, AOption1, AOption2, AOption3, AOutputOptions: Integer);

    /// <summary>Fuehrt Encode aus und gibt den Fehlercode zurueck (ohne Exception)</summary>
    class function EncodeData(ASymbol: TZintSymbol; const AData: String): Integer; overload;
    class function EncodeData(ASymbol: TZintSymbol; const AData: TArrayOfByte; ALength: Integer): Integer; overload;

    /// <summary>Liest errtxt als Delphi-String</summary>
    class function GetErrTxt(ASymbol: TZintSymbol): String;

    /// <summary>Liest text (HRT) als Delphi-String</summary>
    class function GetText(ASymbol: TZintSymbol): String;

    /// <summary>Erzeugt einen String aus den Modulen einer Zeile (0/1)</summary>
    class function ModulesDumpRow(ASymbol: TZintSymbol; ARow: Integer): String;

    /// <summary>Erzeugt einen String aus allen Modulen (Zeilen durch LF getrennt)</summary>
    class function ModulesDump(ASymbol: TZintSymbol): String;

    /// <summary>Wiederholt einen Pattern-String bis zur angegebenen Laenge</summary>
    class function StrRepeat(const APattern: String; ATargetLen: Integer): String;

    /// <summary>Konvertiert einen Unicode-String zu TZintSegment (UTF-8 bytes mit ECI)</summary>
    /// <param name="AData">Unicode-String zu konvertieren</param>
    /// <param name="AEci">ECI-Wert (-1 = Auto, 0 = Latin-1, etc.)</param>
    /// <returns>TZintSegment mit UTF-8 bytes und ECI</returns>
    class function StringToSegment(const AData: String; AEci: Integer = -1): TZintSegment;

    /// <summary>Encodiert mit Segs mittels ZBarcode_Encode_Segs</summary>
    class function EncodeDataSegs(ASymbol: TZintSymbol; const ASegments: TZintSegments): Integer;
  end;

const
  // Ergebnis-Konstanten (Spiegel der Zint-Konstanten, fuer Lesbarkeit in Tests)
  ZINT_WARN_HRT_TRUNCATED    = ZWARN_HRT_TRUNCATED; // = 1
  ZINT_OK                   = 0;
  ZINT_WARN_INVALID_OPTION  = ZWARN_INVALID_OPTION; // = 2
  ZINT_WARN_USES_ECI        = ZWARN_USES_ECI;      // = 3
  ZINT_WARN_NONCOMPLIANT    = ZWARN_NONCOMPLIANT;  // = 4
  ZINT_ERROR_TOO_LONG       = ZERROR_TOO_LONG;      // = 5
  ZINT_ERROR_INVALID_DATA   = ZERROR_INVALID_DATA;   // = 6
  ZINT_ERROR_INVALID_CHECK  = ZERROR_INVALID_CHECK;   // = 7
  ZINT_ERROR_INVALID_OPTION = ZERROR_INVALID_OPTION;  // = 8
  ZINT_ERROR_ENCODING       = ZERROR_ENCODING_PROBLEM; // = 9
  ZINT_ERROR_FILE_ACCESS    = ZERROR_FILE_ACCESS;     // = 10
  ZINT_ERROR_MEMORY         = ZERROR_MEMORY;          // = 11
  ZINT_ERROR_FILE_WRITE     = ZERROR_FILE_WRITE;      // = 12
  ZINT_ERROR_USES_ECI       = ZERROR_USES_ECI;        // = 13
  ZINT_ERROR_NONCOMPLIANT   = ZERROR_NONCOMPLIANT;    // = 14
  ZINT_ERROR_HRT_TRUNCATED  = ZERROR_HRT_TRUNCATED;   // = 15

  // Schwelle ab der ein Fehler vorliegt (< ZINT_ERROR = Warning/OK)
  ZINT_ERROR = ZERROR_TOO_LONG; // = 5

implementation

{ TZintTestHelper }

class function TZintTestHelper.CreateSymbol(ASymbology: Integer): TZintSymbol;
begin
  Result := TZintSymbol.Create(nil);
  Result.symbology := ASymbology;
end;

class procedure TZintTestHelper.SetupSymbol(ASymbol: TZintSymbol;
  ASymbology, AInputMode, AOption1, AOption2, AOption3, AOutputOptions: Integer);
begin
  if ASymbology >= 0 then
    ASymbol.symbology := ASymbology;
  if AInputMode >= 0 then
    ASymbol.input_mode := AInputMode;
  if AOption1 >= 0 then
    ASymbol.option_1 := AOption1;
  if AOption2 >= 0 then
    ASymbol.option_2 := AOption2;
  if AOption3 >= 0 then
    ASymbol.option_3 := AOption3;
  if AOutputOptions >= 0 then
    ASymbol.output_options := AOutputOptions;
end;

class function TZintTestHelper.EncodeData(ASymbol: TZintSymbol; const AData: String): Integer;
var
  b: TArrayOfByte;
begin
  if AData = '' then
  begin
    SetLength(b, 1);
    b[0] := 0;
    Result := ZBarcode_Encode(ASymbol, b, 0);
  end
  else
  begin
    if (ASymbol.input_mode and UNICODE_MODE) <> 0 then
      b := TEncoding.UTF8.GetBytes(AData)
    else
      b := StrToArrayOfByte(AData);

    SetLength(b, Length(b) + 1);
    b[High(b)] := 0;
    Result := ZBarcode_Encode(ASymbol, b, ustrlen(b));
  end;
end;

class function TZintTestHelper.EncodeData(ASymbol: TZintSymbol; const AData: TArrayOfByte; ALength: Integer): Integer;
begin
  Result := ZBarcode_Encode(ASymbol, AData, ALength);
end;

class function TZintTestHelper.GetErrTxt(ASymbol: TZintSymbol): String;
const
  PrefixError = 'error: ';
  PrefixWarning = 'warning: ';
begin
  Result := String(PChar(@ASymbol.errtxt[0]));
  // Strip prefix added by error_tag() so tests match C expectations
  if Result.StartsWith(PrefixError) then
    Result := Result.Substring(Length(PrefixError))
  else if Result.StartsWith(PrefixWarning) then
    Result := Result.Substring(Length(PrefixWarning));
end;

class function TZintTestHelper.GetText(ASymbol: TZintSymbol): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(ASymbol.text) do
  begin
    if ASymbol.text[i] = 0 then Break;
    Result := Result + Chr(ASymbol.text[i]);
  end;
end;

class function TZintTestHelper.ModulesDumpRow(ASymbol: TZintSymbol; ARow: Integer): String;
var
  j: Integer;
begin
  Result := '';
  for j := 0 to ASymbol.width - 1 do
  begin
    if module_is_set(ASymbol, ARow, j) <> 0 then
      Result := Result + '1'
    else
      Result := Result + '0';
  end;
end;

class function TZintTestHelper.ModulesDump(ASymbol: TZintSymbol): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to ASymbol.rows - 1 do
  begin
    if i > 0 then
      Result := Result + #10;
    Result := Result + ModulesDumpRow(ASymbol, i);
  end;
end;

class function TZintTestHelper.StrRepeat(const APattern: String; ATargetLen: Integer): String;
var
  PatLen, i: Integer;
begin
  PatLen := Length(APattern);
  if PatLen = 0 then
    Exit('');

  SetLength(Result, ATargetLen);
  for i := 1 to ATargetLen do
    Result[i] := APattern[((i - 1) mod PatLen) + 1];
end;

class function TZintTestHelper.StringToSegment(const AData: String; AEci: Integer = -1): TZintSegment;
begin
  { Convert Unicode string to UTF-8 bytes for segment }
  Result.Source := TEncoding.UTF8.GetBytes(AData);
  if Length(Result.Source) = 0 then
    Result.Length := 0  { Empty segment: length must be 0, not -1 }
   else
   begin
    { Set actual byte count - ustrlen() will be used by encoder if needed }
    Result.Length := Length(Result.Source);
   end;
  Result.ECI := AEci;
  Result.SourceMode := -1; { Use symbol.input_mode }
end;

class function TZintTestHelper.EncodeDataSegs(ASymbol: TZintSymbol; const ASegments: TZintSegments): Integer;
begin
  Result := ZBarcode_Encode_Segs(ASymbol, ASegments);
end;

end.

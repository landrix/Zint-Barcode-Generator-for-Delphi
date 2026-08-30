unit Test_CDiff;

{$I zint_test.inc}

{
  Differenztest gegen die echte Zint-C-Bibliothek.

  Anders als die uebrigen Testunits ist das hier keine Portierung einer
  C-Testdatei, sondern ein Vergleich von Verhalten: jeder Fall aus
  data/cdiff-corpus.tsv wurde einmal durch die C-Bibliothek geschickt
  (scripts/gen-cdiff-golden.ps1), das Ergebnis steht in data/cdiff-golden.tsv.
  Diese Unit schickt dieselben Faelle durch den Port und vergleicht.

  Was das leistet, was die portierten C-Testfaelle nicht leisten: es findet
  Verhalten, das in C existiert und beim Portieren uebersehen wurde, ohne dass
  jemand die betreffende Assertion geschrieben haben muss. Was es nicht
  leistet: es sagt nichts ueber die Absicht eines Falles - dafuer sind die
  handportierten Faelle mit ihren C#<n>-Verweisen da. Beide werden gebraucht.

  Die Referenzdatei wird nie neu erzeugt, um einen roten Test gruen zu machen
  (docs/PORTING_WORKFLOW.md, Abschnitt 3b).
}

interface

uses
  {$IFNDEF FPC}DUnitX.TestFramework,{$ENDIF}
  TestFramework_Zint,
  SysUtils,
  Classes,
  TestHelper_Zint,
  zint_helper,
  zint;

type
  TCDiffCase = record
    Id: String;
    Symbology: Integer;
    InputMode: Integer;
    Option1, Option2, Option3: Integer;
    OutputOptions: Integer;
    Data: TArrayOfByte;
    DataLen: Integer;
    { Erwartungswerte aus der C-Bibliothek }
    Ret: Integer;
    ErrTxt: String;
    Rows, Width: Integer;
    Height: Single;
    ExpOption1, ExpOption2, ExpOption3: Integer;
    Text: String;
    Modules: String;
  end;

  [TestFixture]
  TTestCDiff = class(TZintFixture)
  private
    procedure RunModule(const AModule: String);
  published
    [Test] procedure CDiff_2of5;
    [Test] procedure CDiff_Auspost;
    [Test] procedure CDiff_Code;
    [Test] procedure CDiff_Code128;
    [Test] procedure CDiff_Medical;
    [Test] procedure CDiff_Plessey;
    [Test] procedure CDiff_Postal;
    [Test] procedure CDiff_Telepen;

    { Der Korpus darf keine Symbologie enthalten, die weder zugeordnet noch
      ausdruecklich ausgenommen ist. Genau diese Pruefung haette zint_dpd und
      zint_upu_s10 gemeldet. }
    [Test] procedure CDiff_KorpusVollstaendigZugeordnet;
    { Eine Ausnahme, die keine mehr noetig ist, muss weg - sonst verrottet die
      Liste unbemerkt und deckt spaeter eine echte Abweichung zu. }
    [Test] procedure CDiff_AusnahmenAlleNochNoetig;
  end;

implementation

{ Zuordnung Symbologie -> Modul. Der Modulname entspricht docs/ports/_modules.tsv.
  Nicht portierte Symbologien gehoeren nicht hierher, sondern in den Korpus
  ueberhaupt nicht - solange es keinen Grund gibt, sie aufzunehmen. }
type
  TSymModule = record
    Symbology: Integer;
    Modul: String;
  end;

const
  SYM_MODULES: array[0..42] of TSymModule = (
    (Symbology: BARCODE_CODE11;      Modul: 'code'),
    (Symbology: BARCODE_C25MATRIX;   Modul: '2of5'),
    (Symbology: BARCODE_C25INTER;    Modul: '2of5'),
    (Symbology: BARCODE_C25IATA;     Modul: '2of5'),
    (Symbology: BARCODE_C25LOGIC;    Modul: '2of5'),
    (Symbology: BARCODE_C25IND;      Modul: '2of5'),
    (Symbology: BARCODE_CODE39;      Modul: 'code'),
    (Symbology: BARCODE_EXCODE39;    Modul: 'code'),
    (Symbology: BARCODE_EAN128;      Modul: 'code128'),
    (Symbology: BARCODE_CODE128;     Modul: 'code128'),
    (Symbology: BARCODE_DPLEIT;      Modul: '2of5'),
    (Symbology: BARCODE_DPIDENT;     Modul: '2of5'),
    (Symbology: BARCODE_CODE93;      Modul: 'code'),
    (Symbology: BARCODE_FLAT;        Modul: 'postal'),
    (Symbology: BARCODE_TELEPEN;     Modul: 'telepen'),
    (Symbology: BARCODE_POSTNET;     Modul: 'postal'),
    (Symbology: BARCODE_MSI_PLESSEY; Modul: 'plessey'),
    (Symbology: BARCODE_FIM;         Modul: 'postal'),
    (Symbology: BARCODE_LOGMARS;     Modul: 'code'),
    (Symbology: BARCODE_PHARMA;      Modul: 'medical'),
    (Symbology: BARCODE_PZN;         Modul: 'medical'),
    (Symbology: BARCODE_PHARMA_TWO;  Modul: 'medical'),
    (Symbology: BARCODE_CEPNET;      Modul: 'postal'),
    (Symbology: BARCODE_CODE128B;    Modul: 'code128'),
    (Symbology: BARCODE_AUSPOST;     Modul: 'auspost'),
    (Symbology: BARCODE_AUSREPLY;    Modul: 'auspost'),
    (Symbology: BARCODE_AUSROUTE;    Modul: 'auspost'),
    (Symbology: BARCODE_AUSREDIRECT; Modul: 'auspost'),
    (Symbology: BARCODE_RM4SCC;      Modul: 'postal'),
    (Symbology: BARCODE_EAN14;       Modul: 'code128'),
    (Symbology: BARCODE_VIN;         Modul: 'code'),
    (Symbology: BARCODE_NVE18;       Modul: 'code128'),
    (Symbology: BARCODE_JAPANPOST;   Modul: 'postal'),
    (Symbology: BARCODE_KOREAPOST;   Modul: 'postal'),
    (Symbology: BARCODE_PLANET;      Modul: 'postal'),
    (Symbology: BARCODE_PLESSEY;     Modul: 'plessey'),
    (Symbology: BARCODE_TELEPEN_NUM; Modul: 'telepen'),
    (Symbology: BARCODE_ITF14;       Modul: '2of5'),
    (Symbology: BARCODE_KIX;         Modul: 'postal'),
    (Symbology: BARCODE_DAFT;        Modul: 'postal'),
    (Symbology: BARCODE_HIBC_128;    Modul: 'code128'),
    (Symbology: BARCODE_HIBC_39;     Modul: 'code'),
    (Symbology: BARCODE_CODE32;      Modul: 'medical')
  );

var
  FCases: array of TCDiffCase;
  FLoaded: Boolean = False;
  { "fall-id:feld" aus data/cdiff-ignore.txt }
  FIgnored: TStringList = nil;
  { Ausnahmen, die im Lauf tatsaechlich gebraucht wurden }
  FIgnoreUsed: TStringList = nil;
  { Ob ueberhaupt schon verglichen wurde - sonst ist ueber tote Ausnahmen
    nichts zu sagen. }
  FCompared: Boolean = False;

{ ---------------------------------------------------------------- Hilfen -- }

{ \xNN und \\ aufloesen - dieselbe Kodierung wie in scripts/tools/cdump.c }
function Unescape(const S: String): TArrayOfByte;
var
  i, n: Integer;
  b: Byte;
begin
  SetLength(Result, Length(S));
  i := 1;
  n := 0;
  while i <= Length(S) do
  begin
    if (S[i] = '\') and (i + 3 <= Length(S)) and ((S[i + 1] = 'x') or (S[i + 1] = 'X')) then
    begin
      b := StrToInt('$' + S[i + 2] + S[i + 3]);
      Result[n] := b;
      Inc(n);
      Inc(i, 4);
    end
    else if (S[i] = '\') and (i + 1 <= Length(S)) and (S[i + 1] = '\') then
    begin
      Result[n] := Ord('\');
      Inc(n);
      Inc(i, 2);
    end
    else
    begin
      Result[n] := Byte(Ord(S[i]));
      Inc(n);
      Inc(i);
    end;
  end;
  SetLength(Result, n);
end;

function UnescapeStr(const S: String): String;
var
  b: TArrayOfByte;
  i: Integer;
begin
  b := Unescape(S);
  SetLength(Result, Length(b));
  for i := 0 to High(b) do
    Result[i + 1] := Chr(b[i]);
end;

{ Sucht eine Datei relativ zum Programm und zum Arbeitsverzeichnis aufwaerts.
  Noetig, weil das Delphi-Gate mit cwd = UnitTests laeuft und das FPC-Gate mit
  cwd = Repositoriumswurzel. }
function FindDataFile(const ARelative: String): String;

  function SearchUp(const AStart: String): String;
  var
    dir, cand: String;
    i: Integer;
  begin
    Result := '';
    dir := ExcludeTrailingPathDelimiter(AStart);
    for i := 0 to 6 do
    begin
      cand := IncludeTrailingPathDelimiter(dir) + ARelative;
      if FileExists(cand) then
      begin
        Result := cand;
        Exit;
      end;
      if ExtractFileDir(dir) = dir then
        Exit;
      dir := ExtractFileDir(dir);
    end;
  end;

begin
  Result := SearchUp(ExtractFilePath(ParamStr(0)));
  if Result = '' then
    Result := SearchUp(GetCurrentDir);
end;

function SplitTabs(const S: String): TStringList;
var
  i, start: Integer;
begin
  Result := TStringList.Create;
  start := 1;
  for i := 1 to Length(S) do
    if S[i] = #9 then
    begin
      Result.Add(Copy(S, start, i - start));
      start := i + 1;
    end;
  Result.Add(Copy(S, start, Length(S) - start + 1));
end;

var
  CDiffFmt: TFormatSettings;

procedure LoadCases;
var
  corpusPath, goldenPath: String;
  corpus, golden: TStringList;
  i, n: Integer;
  cf, gf: TStringList;
  byId: TStringList;
  idx: Integer;
begin
  if FLoaded then
    Exit;

  corpusPath := FindDataFile('UnitTests' + PathDelim + 'data' + PathDelim + 'cdiff-corpus.tsv');
  goldenPath := FindDataFile('UnitTests' + PathDelim + 'data' + PathDelim + 'cdiff-golden.tsv');
  { Eine fehlende Datei ist ein harter Fehler. "0 Faelle geprueft, gruen" waere
    die schlimmste aller Antworten. }
  if corpusPath = '' then
    raise Exception.Create('Test_CDiff: cdiff-corpus.tsv nicht gefunden (gesucht ab ' +
      ExtractFilePath(ParamStr(0)) + ' und ' + GetCurrentDir + ')');
  if goldenPath = '' then
    raise Exception.Create('Test_CDiff: cdiff-golden.tsv nicht gefunden; erzeugen mit ' +
      'scripts/gen-cdiff-golden.ps1');

  corpus := TStringList.Create;
  golden := TStringList.Create;
  byId := TStringList.Create;
  try
    corpus.LoadFromFile(corpusPath);
    golden.LoadFromFile(goldenPath);

    byId.Sorted := False;
    for i := 0 to golden.Count - 1 do
    begin
      if (golden[i] = '') or (golden[i][1] = '#') then
        Continue;
      gf := SplitTabs(golden[i]);
      try
        if (gf.Count > 0) and (gf[0] <> 'id') then
          { NativeInt statt Integer: unter FPC/aarch64 ist ein Zeiger 8 Byte
            und die Umwandlung aus LongInt waere unzulaessig. }
          byId.AddObject(gf[0], TObject(NativeInt(i)));
      finally
        gf.Free;
      end;
    end;
    byId.Sorted := True;

    n := 0;
    SetLength(FCases, corpus.Count);
    for i := 0 to corpus.Count - 1 do
    begin
      if (corpus[i] = '') or (corpus[i][1] = '#') then
        Continue;
      cf := SplitTabs(corpus[i]);
      try
        if (cf.Count < 8) or (cf[0] = 'id') then
          Continue;
        idx := byId.IndexOf(cf[0]);
        if idx < 0 then
          raise Exception.Create('Test_CDiff: Fall "' + cf[0] +
            '" steht im Korpus, aber nicht in der Referenzdatei. ' +
            'Referenzdatei mit scripts/gen-cdiff-golden.ps1 neu erzeugen - ' +
            'als eigener Commit, siehe PORTING_WORKFLOW Abschnitt 3b.');

        FCases[n].Id := cf[0];
        FCases[n].Symbology := StrToInt(cf[1]);
        FCases[n].InputMode := StrToInt(cf[2]);
        FCases[n].Option1 := StrToInt(cf[3]);
        FCases[n].Option2 := StrToInt(cf[4]);
        FCases[n].Option3 := StrToInt(cf[5]);
        FCases[n].OutputOptions := StrToInt(cf[6]);
        FCases[n].Data := Unescape(cf[7]);
        FCases[n].DataLen := Length(FCases[n].Data);

        gf := SplitTabs(golden[NativeInt(byId.Objects[idx])]);
        try
          FCases[n].Ret := StrToInt(gf[1]);
          FCases[n].ErrTxt := UnescapeStr(gf[2]);
          FCases[n].Rows := StrToInt(gf[3]);
          FCases[n].Width := StrToInt(gf[4]);
          FCases[n].Height := StrToFloat(gf[5], CDiffFmt);
          FCases[n].ExpOption1 := StrToInt(gf[6]);
          FCases[n].ExpOption2 := StrToInt(gf[7]);
          FCases[n].ExpOption3 := StrToInt(gf[8]);
          FCases[n].Text := UnescapeStr(gf[9]);
          FCases[n].Modules := UnescapeStr(gf[10]);
        finally
          gf.Free;
        end;
        Inc(n);
      finally
        cf.Free;
      end;
    end;
    SetLength(FCases, n);
    if n = 0 then
      raise Exception.Create('Test_CDiff: Korpus enthaelt keine Faelle');
  finally
    corpus.Free;
    golden.Free;
    byId.Free;
  end;
  FLoaded := True;
end;

procedure LoadIgnores;
var
  path, line: String;
  raw: TStringList;
  i: Integer;
begin
  if FIgnored <> nil then
    Exit;
  FIgnored := TStringList.Create;
  FIgnoreUsed := TStringList.Create;
  FIgnored.Sorted := True;
  FIgnoreUsed.Sorted := True;
  FIgnoreUsed.Duplicates := dupIgnore;

  path := FindDataFile('UnitTests' + PathDelim + 'data' + PathDelim + 'cdiff-ignore.txt');
  if path = '' then
    raise Exception.Create('Test_CDiff: cdiff-ignore.txt nicht gefunden');

  raw := TStringList.Create;
  try
    raw.LoadFromFile(path);
    for i := 0 to raw.Count - 1 do
    begin
      line := Trim(raw[i]);
      if (line = '') or (line[1] = '#') then
        Continue;
      FIgnored.Add(line);
    end;
  finally
    raw.Free;
  end;
end;

{ Vergleicht, sofern nicht ausgenommen. ANote landet nur in der Meldung, nicht
  im Vergleich - der Fehlertext hilft beim Lesen eines roten ret.

  Eine Ausnahme wird nur dann als noetig vermerkt, wenn die Werte auch
  wirklich auseinandergehen. "Nachgeschlagen" reicht nicht: sonst gilt jede
  Ausnahme als gebraucht, sobald ihr Feld ueberhaupt geprueft wird, und
  CDiff_AusnahmenAlleNochNoetig findet nie einen toten Eintrag. }
procedure CheckField(const AId, AField: String; const AExpected, AActual: String;
  const ANote: String = '');
var
  msg, key: String;
begin
  if AExpected = AActual then
    Exit;
  key := AId + ':' + AField;
  if FIgnored.IndexOf(key) >= 0 then
  begin
    FIgnoreUsed.Add(key);
    Exit;
  end;
  msg := AId + ' ' + AField;
  if ANote <> '' then
    msg := msg + ' (' + ANote + ')';
  ZAssert.AreEqual(AExpected, AActual, msg);
end;

function ModuleOf(ASymbology: Integer): String;
var
  i: Integer;
begin
  Result := '';
  for i := Low(SYM_MODULES) to High(SYM_MODULES) do
    if SYM_MODULES[i].Symbology = ASymbology then
    begin
      Result := SYM_MODULES[i].Modul;
      Exit;
    end;
end;

{ ------------------------------------------------------------------ Tests - }

procedure TTestCDiff.RunModule(const AModule: String);
var
  i, checked: Integer;
  c: TCDiffCase;
  sym: TZintSymbol;
  ret: Integer;
begin
  LoadCases;
  LoadIgnores;
  checked := 0;
  for i := 0 to High(FCases) do
  begin
    c := FCases[i];
    if ModuleOf(c.Symbology) <> AModule then
      Continue;

    sym := TZintTestHelper.CreateSymbol(c.Symbology);
    try
      if c.InputMode >= 0 then
        sym.input_mode := c.InputMode;
      if c.Option1 >= 0 then
        sym.option_1 := c.Option1;
      if c.Option2 >= 0 then
        sym.option_2 := c.Option2;
      if c.Option3 >= 0 then
        sym.option_3 := c.Option3;
      if c.OutputOptions >= 0 then
        sym.output_options := c.OutputOptions;

      ret := TZintTestHelper.EncodeData(sym, c.Data, c.DataLen);
      FCompared := True;

      CheckField(c.Id, 'ret', IntToStr(c.Ret), IntToStr(ret),
        TZintTestHelper.GetErrTxt(sym));
      CheckField(c.Id, 'errtxt', c.ErrTxt, TZintTestHelper.GetErrTxt(sym));
      if ret < ZERROR_TOO_LONG then
      begin
        CheckField(c.Id, 'rows', IntToStr(c.Rows), IntToStr(sym.rows));
        CheckField(c.Id, 'width', IntToStr(c.Width), IntToStr(sym.width));
        CheckField(c.Id, 'height', FloatToStr(c.Height, CDiffFmt), FloatToStr(sym.height, CDiffFmt));
        CheckField(c.Id, 'option_2', IntToStr(c.ExpOption2), IntToStr(sym.option_2));
        CheckField(c.Id, 'text', c.Text, TZintTestHelper.GetText(sym));
        CheckField(c.Id, 'modules', c.Modules, TZintTestHelper.ModulesDump(sym));
      end;
      Inc(checked);
    finally
      sym.Free;
    end;
  end;

  { Ein Modul ohne Faelle heisst, dass die Zuordnung oder der Korpus kaputt
    ist - nicht, dass alles in Ordnung waere. }
  if checked = 0 then
    ZAssert.Fail('Test_CDiff: kein Fall fuer Modul "' + AModule + '" im Korpus');
end;

procedure TTestCDiff.CDiff_2of5;    begin RunModule('2of5');    end;
procedure TTestCDiff.CDiff_Auspost; begin RunModule('auspost'); end;
procedure TTestCDiff.CDiff_Code;    begin RunModule('code');    end;
procedure TTestCDiff.CDiff_Code128; begin RunModule('code128'); end;
procedure TTestCDiff.CDiff_Medical; begin RunModule('medical'); end;
procedure TTestCDiff.CDiff_Plessey; begin RunModule('plessey'); end;
procedure TTestCDiff.CDiff_Postal;  begin RunModule('postal');  end;
procedure TTestCDiff.CDiff_Telepen; begin RunModule('telepen'); end;

procedure TTestCDiff.CDiff_KorpusVollstaendigZugeordnet;
var
  i: Integer;
  unassigned: String;
begin
  LoadCases;
  unassigned := '';
  for i := 0 to High(FCases) do
    if ModuleOf(FCases[i].Symbology) = '' then
      if Pos('(' + IntToStr(FCases[i].Symbology) + ')', unassigned) = 0 then
        unassigned := unassigned + FCases[i].Id + '(' + IntToStr(FCases[i].Symbology) + ') ';

  if unassigned <> '' then
    ZAssert.Fail('Test_CDiff: Symbologien im Korpus ohne Modulzuordnung: ' + unassigned +
      '- entweder in SYM_MODULES eintragen oder mit Begruendung aus dem Korpus nehmen');
end;

procedure TTestCDiff.CDiff_AusnahmenAlleNochNoetig;
var
  i: Integer;
  dead: String;
begin
  { Setzt voraus, dass die Modultests vorher liefen - DUnitX und FPC fuehren
    die Methoden einer Fixture in Deklarationsreihenfolge aus, und diese steht
    zuletzt. Laeuft sie allein, hat noch nichts verglichen; dann ist die
    Aussage nicht zu treffen und der Test haelt sich zurueck. }
  LoadCases;
  LoadIgnores;
  if not FCompared then
    Exit;

  dead := '';
  for i := 0 to FIgnored.Count - 1 do
    if FIgnoreUsed.IndexOf(FIgnored[i]) < 0 then
      dead := dead + FIgnored[i] + ' ';

  if dead <> '' then
    ZAssert.Fail('Test_CDiff: Ausnahmen in cdiff-ignore.txt, die nicht mehr ' +
      'gebraucht werden: ' + dead + '- ersatzlos streichen');
end;

initialization
  CDiffFmt := {$IFDEF FPC}DefaultFormatSettings{$ELSE}SysUtils.FormatSettings{$ENDIF};
  CDiffFmt.DecimalSeparator := '.';
  ZRegisterFixture(TTestCDiff);

end.

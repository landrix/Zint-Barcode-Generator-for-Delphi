unit Test_Code1;

interface

uses
  DUnitX.TestFramework,
  TestHelper_Zint,
  zint,
  zint_common;

type
  TCodeOneCase = record
    Index: Integer;
    Option2: Integer;
    InputMode: Integer;
    Eci: Integer;
    StructAppCount: Integer;
    StructAppIndex: Integer;
    StructAppId: String;
    Data: String;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    ExpectedOption2: Integer;
    ExpectedErrTxt: String;
  end;

  [TestFixture]
  TTestCodeOneInputFromC = class(TObject)
  published
    [Test]
    procedure TestInputAndLargeSubset;
    [Test]
    procedure TestInputDeeperLegacyDeltaSubset;
  end;

implementation

uses
  System.SysUtils;

procedure TTestCodeOneInputFromC.TestInputAndLargeSubset;
const
  GS1PARENS = GS1PARENS_MODE;
  ESCAPE = ESCAPE_MODE;
var
  Symbol: TZintSymbol;
  I: Integer;
  Ret: Integer;
  Cases: array[0..16] of TCodeOneCase;
begin
  Cases[0] := Default(TCodeOneCase);
  Cases[0].Index := 0;
  Cases[0].Option2 := 0;
  Cases[0].Data := '123456789012ABCDEFGHI';
  Cases[0].ExpectedRet := 0;
  Cases[0].ExpectedRows := 22;
  Cases[0].ExpectedWidth := 22;
  Cases[0].ExpectedOption2 := 2;

  Cases[1] := Default(TCodeOneCase);
  Cases[1].Index := 2;
  Cases[1].Option2 := 1;
  Cases[1].Data := '1';
  Cases[1].ExpectedRet := 0;
  Cases[1].ExpectedRows := 16;
  Cases[1].ExpectedWidth := 18;
  Cases[1].ExpectedOption2 := 1;

  Cases[2] := Default(TCodeOneCase);
  Cases[2].Index := 16;
  Cases[2].Option2 := 9;
  Cases[2].Eci := 3;
  Cases[2].Data := '1';
  Cases[2].ExpectedRet := ZWARN_INVALID_OPTION;
  Cases[2].ExpectedRows := 8;
  Cases[2].ExpectedWidth := 11;
  Cases[2].ExpectedOption2 := 9;
  Cases[2].ExpectedErrTxt := 'Warning 511: ECI ignored for Version S';

  Cases[3] := Default(TCodeOneCase);
  Cases[3].Index := 104;
  Cases[3].Option2 := 9;
  Cases[3].InputMode := 0;
  Cases[3].Data := TZintTestHelper.StrRepeat('1', 12);
  Cases[3].ExpectedRet := 0;
  Cases[3].ExpectedRows := 8;
  Cases[3].ExpectedWidth := 21;
  Cases[3].ExpectedOption2 := 9;

  Cases[4] := Default(TCodeOneCase);
  Cases[4].Index := 17;
  Cases[4].Option2 := 9;
  Cases[4].InputMode := GS1_MODE;
  Cases[4].Eci := 3;
  Cases[4].Data := '[01]12345678901231';
  Cases[4].ExpectedRet := ZWARN_INVALID_OPTION;
  Cases[4].ExpectedRows := 8;
  Cases[4].ExpectedWidth := 31;
  Cases[4].ExpectedOption2 := 9;
  Cases[4].ExpectedErrTxt := 'Warning 511: ECI and GS1 mode ignored for Version S';

  Cases[5] := Default(TCodeOneCase);
  Cases[5].Index := 12;
  Cases[5].Option2 := 9;
  Cases[5].Data := TZintTestHelper.StrRepeat('1', 18);
  Cases[5].ExpectedRet := 0;
  Cases[5].ExpectedRows := 8;
  Cases[5].ExpectedWidth := 31;
  Cases[5].ExpectedOption2 := 9;

  Cases[6] := Default(TCodeOneCase);
  Cases[6].Index := 13;
  Cases[6].Option2 := 9;
  Cases[6].Data := '123A';
  Cases[6].ExpectedRet := ZERROR_INVALID_DATA;
  Cases[6].ExpectedOption2 := 9;
  Cases[6].ExpectedErrTxt := 'Error 515: Invalid character at position 4 in input (Version S encodes digits only)';

  Cases[7] := Default(TCodeOneCase);
  Cases[7].Index := 14;
  Cases[7].Option2 := 9;
  Cases[7].Data := TZintTestHelper.StrRepeat('1', 19);
  Cases[7].ExpectedRet := ZERROR_TOO_LONG;
  Cases[7].ExpectedOption2 := 9;
  Cases[7].ExpectedErrTxt := 'Error 514: Input length 19 too long for Version S (maximum 18)';

  Cases[8] := Default(TCodeOneCase);
  Cases[8].Index := 33;
  Cases[8].Option2 := 0;
  Cases[8].StructAppCount := 129;
  Cases[8].StructAppIndex := 1;
  Cases[8].Data := '123456789012ABCDEFGHI';
  Cases[8].ExpectedRet := ZERROR_INVALID_OPTION;
  Cases[8].ExpectedOption2 := 0;
  Cases[8].ExpectedErrTxt := 'Error 711: Structured Append count ''129'' out of range (2 to 128)';

  Cases[9] := Default(TCodeOneCase);
  Cases[9].Index := 19;
  Cases[9].Option2 := 10;
  Cases[9].Data := TZintTestHelper.StrRepeat('1', 91);
  Cases[9].ExpectedRet := ZERROR_TOO_LONG;
  Cases[9].ExpectedOption2 := 10;
  Cases[9].ExpectedErrTxt := 'Error 519: Input length 91 too long for Version T (maximum 90)';

  Cases[10] := Default(TCodeOneCase);
  Cases[10].Index := 28;
  Cases[10].Option2 := 11;
  Cases[10].Data := '123';
  Cases[10].ExpectedRet := ZERROR_INVALID_OPTION;
  Cases[10].ExpectedOption2 := 11;
  Cases[10].ExpectedErrTxt := 'Error 513: Version ''11'' out of range (1 to 10)';

  Cases[11] := Default(TCodeOneCase);
  Cases[11].Index := 35;
  Cases[11].Option2 := 0;
  Cases[11].InputMode := 0;
  Cases[11].StructAppCount := 2;
  Cases[11].StructAppIndex := 3;
  Cases[11].Data := '123456789012ABCDEFGHI';
  Cases[11].ExpectedRet := ZERROR_INVALID_OPTION;
  Cases[11].ExpectedOption2 := 0;
  Cases[11].ExpectedErrTxt := 'Error 712: Structured Append index ''3'' out of range (1 to count 2)';

  Cases[12] := Default(TCodeOneCase);
  Cases[12].Index := 31;
  Cases[12].Option2 := 1;
  Cases[12].StructAppCount := 1;
  Cases[12].StructAppIndex := 1;
  Cases[12].Data := '123';
  Cases[12].ExpectedRet := ZERROR_INVALID_OPTION;
  Cases[12].ExpectedOption2 := 1;
  Cases[12].ExpectedErrTxt := 'Error 711: Structured Append count ''1'' out of range (2 to 128)';

  Cases[13] := Default(TCodeOneCase);
  Cases[13].Index := 34;
  Cases[13].Option2 := 1;
  Cases[13].StructAppCount := 2;
  Cases[13].StructAppIndex := 0;
  Cases[13].Data := '123';
  Cases[13].ExpectedRet := ZERROR_INVALID_OPTION;
  Cases[13].ExpectedOption2 := 1;
  Cases[13].ExpectedErrTxt := 'Error 712: Structured Append index ''0'' out of range (1 to count 2)';

  Cases[14] := Default(TCodeOneCase);
  Cases[14].Index := 36;
  Cases[14].Option2 := 1;
  Cases[14].StructAppCount := 2;
  Cases[14].StructAppIndex := 1;
  Cases[14].StructAppId := '1';
  Cases[14].Data := '123';
  Cases[14].ExpectedRet := ZERROR_INVALID_OPTION;
  Cases[14].ExpectedOption2 := 1;
  Cases[14].ExpectedErrTxt := 'Error 713: Structured Append ID not available for Code One';

  Cases[15] := Default(TCodeOneCase);
  Cases[15].Index := 37;
  Cases[15].Option2 := 9;
  Cases[15].StructAppCount := 2;
  Cases[15].StructAppIndex := 1;
  Cases[15].Data := '123';
  Cases[15].ExpectedRet := ZERROR_INVALID_OPTION;
  Cases[15].ExpectedOption2 := 9;
  Cases[15].ExpectedErrTxt := 'Error 714: Structured Append not available for Version S';

  Cases[16] := Default(TCodeOneCase);
  Cases[16].Index := 29;
  Cases[16].Option2 := -2;
  Cases[16].Data := '1';
  Cases[16].ExpectedRet := ZERROR_INVALID_OPTION;
  Cases[16].ExpectedOption2 := -2;
  Cases[16].ExpectedErrTxt := 'Error 513: Version ''-2'' out of range (1 to 10)';

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(BARCODE_CODEONE);
    try
      TZintTestHelper.SetupSymbol(Symbol, BARCODE_CODEONE, Cases[I].InputMode, -1, Cases[I].Option2, -1, -1);
      if Cases[I].Option2 < 0 then
        Symbol.option_2 := Cases[I].Option2;
      Symbol.eci := Cases[I].Eci;
      Symbol.structapp.count := Cases[I].StructAppCount;
      Symbol.structapp.index := Cases[I].StructAppIndex;
      Symbol.structapp.id := Cases[I].StructAppId;

      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret, Format('C#%d ret', [Cases[I].Index]));
      Assert.AreEqual<Integer>(Cases[I].ExpectedOption2, Symbol.option_2, Format('C#%d option_2', [Cases[I].Index]));
      if Cases[I].ExpectedRows <> 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows, Format('C#%d rows', [Cases[I].Index]));
      if Cases[I].ExpectedWidth <> 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width, Format('C#%d width', [Cases[I].Index]));
      Assert.AreEqual(Cases[I].ExpectedErrTxt, TZintTestHelper.GetErrTxt(Symbol), Format('C#%d errtxt', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestCodeOneInputFromC.TestInputDeeperLegacyDeltaSubset;
type
  TDeepCase = record
    Index: Integer;
    InputMode: Integer;
    Option2: Integer;
    Eci: Integer;
    Data: String;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    ExpectedOption2: Integer;
    ExpectedErrTxt: String;
  end;
const
  GS1PARENS = GS1PARENS_MODE;
  ESCAPE = ESCAPE_MODE;
var
  Symbol: TZintSymbol;
  Ret: Integer;
  I: Integer;
  Cases: array[0..5] of TDeepCase;
begin
  { C#6: GS1_MODE + Version A success }
  Cases[0] := Default(TDeepCase);
  Cases[0].Index := 6;
  Cases[0].InputMode := GS1_MODE;
  Cases[0].Option2 := 1;
  Cases[0].Data := '[01]12345678901231';
  Cases[0].ExpectedRet := 0;
  Cases[0].ExpectedRows := 16;
  Cases[0].ExpectedWidth := 18;
  Cases[0].ExpectedOption2 := 1;

  { C#7: GS1PARENS_MODE + Version A success }
  Cases[1] := Default(TDeepCase);
  Cases[1].Index := 7;
  Cases[1].InputMode := GS1_MODE or GS1PARENS;
  Cases[1].Option2 := 1;
  Cases[1].Data := '(01)12345678901231';
  Cases[1].ExpectedRet := 0;
  Cases[1].ExpectedRows := 16;
  Cases[1].ExpectedWidth := 18;
  Cases[1].ExpectedOption2 := 1;

  { C#8: ESCAPE_MODE + GS1PARENS_MODE + Version A success }
  Cases[2] := Default(TDeepCase);
  Cases[2].Index := 8;
  Cases[2].InputMode := GS1_MODE or ESCAPE or GS1PARENS;
  Cases[2].Option2 := 1;
  Cases[2].Data := '(21)\(12\)4567890123';
  Cases[2].ExpectedRet := 0;
  Cases[2].ExpectedRows := 16;
  Cases[2].ExpectedWidth := 18;
  Cases[2].ExpectedOption2 := 1;

  { C#11: GS1_MODE + ECI=3 warns (ECI ignored for GS1) but encodes Version A }
  Cases[3] := Default(TDeepCase);
  Cases[3].Index := 11;
  Cases[3].InputMode := GS1_MODE;
  Cases[3].Eci := 3;
  Cases[3].Option2 := 1;
  Cases[3].Data := '[01]12345678901231';
  Cases[3].ExpectedRet := ZWARN_INVALID_OPTION;
  Cases[3].ExpectedRows := 16;
  Cases[3].ExpectedWidth := 18;
  Cases[3].ExpectedOption2 := 1;
  Cases[3].ExpectedErrTxt := 'Warning 512: ECI ignored for GS1 mode';

  { C#18: Version T-48 with 90 digits succeeds (decimal mode, 38 codewords) }
  Cases[4] := Default(TDeepCase);
  Cases[4].Index := 18;
  Cases[4].InputMode := 0;
  Cases[4].Option2 := 10;
  Cases[4].Data := '123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890';
  Cases[4].ExpectedRet := 0;
  Cases[4].ExpectedRows := 16;
  Cases[4].ExpectedWidth := 49;
  Cases[4].ExpectedOption2 := 10;

  { C#15: GS1_MODE + Version S: GS1 mode ignored warning }
  Cases[5] := Default(TDeepCase);
  Cases[5].Index := 15;
  Cases[5].InputMode := GS1_MODE;
  Cases[5].Option2 := 9;
  Cases[5].Data := '[01]12345678901231';
  Cases[5].ExpectedRet := ZWARN_INVALID_OPTION;
  Cases[5].ExpectedRows := 8;
  Cases[5].ExpectedWidth := 31;
  Cases[5].ExpectedOption2 := 9;
  Cases[5].ExpectedErrTxt := 'Warning 511: GS1 mode ignored for Version S';

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(BARCODE_CODEONE);
    try
      TZintTestHelper.SetupSymbol(Symbol, BARCODE_CODEONE, Cases[I].InputMode, -1, Cases[I].Option2, -1, -1);
      Symbol.eci := Cases[I].Eci;

      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret, Format('C#%d ret', [Cases[I].Index]));
      Assert.AreEqual<Integer>(Cases[I].ExpectedOption2, Symbol.option_2, Format('C#%d option_2', [Cases[I].Index]));
      if Cases[I].ExpectedRows <> 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows, Format('C#%d rows', [Cases[I].Index]));
      if Cases[I].ExpectedWidth <> 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width, Format('C#%d width', [Cases[I].Index]));
      if Cases[I].ExpectedErrTxt <> '' then
        Assert.AreEqual(Cases[I].ExpectedErrTxt, TZintTestHelper.GetErrTxt(Symbol), Format('C#%d errtxt', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestCodeOneInputFromC);

end.
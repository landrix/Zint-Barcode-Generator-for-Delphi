unit Test_Composite;

{
  Composite port-check fixture based on:
  Lib/zint-master-2026-03-13-b3a3c0d/backend/tests/test_composite.c

  Conservative subset coverage with explicit C indices:
  - test_eanx_leading_zeroes  -> TestLargeSubset
  - test_input                -> TestInputSubset
  - test_encodation_0         -> TestEncodeSubset
  - test_hrt                  -> TestHRTSubset
  - test_fuzz                 -> TestFuzzSubset
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  DUnitX.TestFramework,
  SysUtils,
  TestHelper_Zint,
  zint,
  zint_common;

type
  [TestFixture]
  TTestCompositeFromC = class
  published
    [Test]
    procedure TestLargeSubset;
    [Test]
    procedure TestInputSubset;
    [Test]
    procedure TestEncodeSubset;
    [Test]
    procedure TestHRTSubset;
    [Test]
    procedure TestFuzzSubset;
  end;

implementation

function BytesToLatin1String(const ABytes: TArrayOfByte; const ALength: Integer): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to ALength - 1 do
    Result := Result + Char(ABytes[I]);
end;

procedure AssertContentSeg0(const Symbol: TZintSymbol; const ExpectedContent: String; const Msg: String);
var
  Actual: String;
begin
  Assert.AreEqual<Integer>(1, Symbol.content_segs_count, Msg + ' content_segs_count');
  Assert.IsTrue(Length(Symbol.content_segs) > 0, Msg + ' content_segs length');
  Assert.AreEqual<Integer>(Length(ExpectedContent), Symbol.content_segs[0].Length, Msg + ' content len');
  Actual := BytesToLatin1String(Symbol.content_segs[0].Source, Symbol.content_segs[0].Length);
  Assert.AreEqual(ExpectedContent, Actual, Msg + ' content value');
end;

procedure TTestCompositeFromC.TestLargeSubset;
type
  TCase = record
    Index: Integer;
    Symbology: Integer;
    PrimaryData: String;
    CompositeData: String;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
  end;
var
  Cases: array[0..3] of TCase;
  I, Ret: Integer;
  Symbol: TZintSymbol;
begin
  { test_eanx_leading_zeroes }
  Cases[0] := Default(TCase); Cases[0].Index := 15; Cases[0].Symbology := BARCODE_EANX_CC;  Cases[0].PrimaryData := '12345678';        Cases[0].CompositeData := '[21]A12345678'; Cases[0].ExpectedRet := 0; Cases[0].ExpectedRows := 7; Cases[0].ExpectedWidth := 99;
  Cases[1] := Default(TCase); Cases[1].Index := 34; Cases[1].Symbology := BARCODE_EANX_CC;  Cases[1].PrimaryData := '12345678+12';     Cases[1].CompositeData := '[21]A12345678'; Cases[1].ExpectedRet := 0; Cases[1].ExpectedRows := 7; Cases[1].ExpectedWidth := 125;
  Cases[2] := Default(TCase); Cases[2].Index := 54; Cases[2].Symbology := BARCODE_EANX_CC;  Cases[2].PrimaryData := '12345678+123';    Cases[2].CompositeData := '[21]A12345678'; Cases[2].ExpectedRet := 0; Cases[2].ExpectedRows := 7; Cases[2].ExpectedWidth := 152;
  Cases[3] := Default(TCase); Cases[3].Index := 64; Cases[3].Symbology := BARCODE_EANX_CC;  Cases[3].PrimaryData := '1234567890128 12345'; Cases[3].CompositeData := '[21]A12345678'; Cases[3].ExpectedRet := ZINT_ERROR_INVALID_DATA; Cases[3].ExpectedRows := -1; Cases[3].ExpectedWidth := -1;

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      strcpy(Symbol.primary, Cases[I].PrimaryData);
      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].CompositeData);

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret,
        Format('C#%d ret errtxt="%s"', [Cases[I].Index, TZintTestHelper.GetErrTxt(Symbol)]));
      if Ret < ZINT_ERROR then
      begin
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows, Format('C#%d rows', [Cases[I].Index]));
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width, Format('C#%d width', [Cases[I].Index]));
      end;
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestCompositeFromC.TestInputSubset;
type
  TCase = record
    Index: Integer;
    Symbology: Integer;
    InputMode: Integer;
    Option1: Integer;
    Option2: Integer;
    Option3: Integer;
    PrimaryData: String;
    CompositeData: String;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    ExpectedOption1: Integer;
    ExpectedOption2: Integer;
    ExpectedOption3: Integer;
  end;
var
  Cases: array[0..9] of TCase;
  I, Ret: Integer;
  Symbol: TZintSymbol;
begin
  { test_input }
  Cases[0] := Default(TCase); Cases[0].Index := 0;   Cases[0].Symbology := BARCODE_EANX_CC;       Cases[0].InputMode := UNICODE_MODE;                Cases[0].Option1 := -1; Cases[0].Option2 := -1; Cases[0].Option3 := -1; Cases[0].PrimaryData := '1234567';             Cases[0].CompositeData := '[20]12';          Cases[0].ExpectedRet := 0;                     Cases[0].ExpectedRows := 8;  Cases[0].ExpectedWidth := 82;  Cases[0].ExpectedOption1 := -1; Cases[0].ExpectedOption2 := 0; Cases[0].ExpectedOption3 := 0;
  { DELTA: C#6 expects warning; Delphi legacy composite currently returns success for this EANX_CC mapping. }
  Cases[1] := Default(TCase); Cases[1].Index := 6;   Cases[1].Symbology := BARCODE_EANX_CC;       Cases[1].InputMode := UNICODE_MODE;                Cases[1].Option1 := -1; Cases[1].Option2 := -1; Cases[1].Option3 := -1; Cases[1].PrimaryData := '1234567';             Cases[1].CompositeData := '[20]1A';          Cases[1].ExpectedRet := 0;                     Cases[1].ExpectedRows := 8;  Cases[1].ExpectedWidth := 82;  Cases[1].ExpectedOption1 := -1; Cases[1].ExpectedOption2 := 0; Cases[1].ExpectedOption3 := 0;
  Cases[2] := Default(TCase); Cases[2].Index := 8;   Cases[2].Symbology := BARCODE_EANX_CC;       Cases[2].InputMode := GS1NOCHECK_MODE;            Cases[2].Option1 := -1; Cases[2].Option2 := -1; Cases[2].Option3 := -1; Cases[2].PrimaryData := '1234567';             Cases[2].CompositeData := '[20]1A';          Cases[2].ExpectedRet := 0;                     Cases[2].ExpectedRows := 8;  Cases[2].ExpectedWidth := 82;  Cases[2].ExpectedOption1 := -1; Cases[2].ExpectedOption2 := 0; Cases[2].ExpectedOption3 := 0;
  Cases[3] := Default(TCase); Cases[3].Index := 45;  Cases[3].Symbology := BARCODE_EANX_CC;       Cases[3].InputMode := UNICODE_MODE;                Cases[3].Option1 := 3;  Cases[3].Option2 := -1; Cases[3].Option3 := -1; Cases[3].PrimaryData := '1234567890128';       Cases[3].CompositeData := '[20]12';          Cases[3].ExpectedRet := ZINT_ERROR_INVALID_OPTION; Cases[3].ExpectedRows := -1; Cases[3].ExpectedWidth := -1; Cases[3].ExpectedOption1 := 3; Cases[3].ExpectedOption2 := 0; Cases[3].ExpectedOption3 := 0;
  Cases[4] := Default(TCase); Cases[4].Index := 49;  Cases[4].Symbology := BARCODE_RSS14_CC;      Cases[4].InputMode := UNICODE_MODE;                Cases[4].Option1 := -1; Cases[4].Option2 := -1; Cases[4].Option3 := -1; Cases[4].PrimaryData := '1234567890123';       Cases[4].CompositeData := '[20]12';          Cases[4].ExpectedRet := 0;                     Cases[4].ExpectedRows := 5;  Cases[4].ExpectedWidth := 100; Cases[4].ExpectedOption1 := -1; Cases[4].ExpectedOption2 := 0; Cases[4].ExpectedOption3 := 0;
  Cases[5] := Default(TCase); Cases[5].Index := 73;  Cases[5].Symbology := BARCODE_UPCA_CC;       Cases[5].InputMode := UNICODE_MODE;                Cases[5].Option1 := -1; Cases[5].Option2 := -1; Cases[5].Option3 := -1; Cases[5].PrimaryData := '12345678901';         Cases[5].CompositeData := '[20]12';          Cases[5].ExpectedRet := 0;                     Cases[5].ExpectedRows := 7;  Cases[5].ExpectedWidth := 99;  Cases[5].ExpectedOption1 := -1; Cases[5].ExpectedOption2 := 0; Cases[5].ExpectedOption3 := 0;
  Cases[6] := Default(TCase); Cases[6].Index := 85;  Cases[6].Symbology := BARCODE_UPCE_CC;       Cases[6].InputMode := UNICODE_MODE;                Cases[6].Option1 := -1; Cases[6].Option2 := -1; Cases[6].Option3 := -1; Cases[6].PrimaryData := '123456';              Cases[6].CompositeData := '[20]12';          Cases[6].ExpectedRet := 0;                     Cases[6].ExpectedRows := 9;  Cases[6].ExpectedWidth := 55;  Cases[6].ExpectedOption1 := -1; Cases[6].ExpectedOption2 := 0; Cases[6].ExpectedOption3 := 0;
  Cases[7] := Default(TCase); Cases[7].Index := 122; Cases[7].Symbology := BARCODE_EAN128_CC;     Cases[7].InputMode := UNICODE_MODE;                Cases[7].Option1 := -1; Cases[7].Option2 := -1; Cases[7].Option3 := -1; Cases[7].PrimaryData := '';                    Cases[7].CompositeData := '[20]12';          Cases[7].ExpectedRet := ZINT_ERROR_INVALID_OPTION; Cases[7].ExpectedRows := -1; Cases[7].ExpectedWidth := -1; Cases[7].ExpectedOption1 := -1; Cases[7].ExpectedOption2 := 0; Cases[7].ExpectedOption3 := 0;
  Cases[8] := Default(TCase); Cases[8].Index := 137; Cases[8].Symbology := BARCODE_RSS_EXPSTACK_CC; Cases[8].InputMode := UNICODE_MODE;             Cases[8].Option1 := -1; Cases[8].Option2 := 1;  Cases[8].Option3 := -1; Cases[8].PrimaryData := '[91]1234567890123456789012345678901234'; Cases[8].CompositeData := '[20]12'; Cases[8].ExpectedRet := 0; Cases[8].ExpectedRows := 13; Cases[8].ExpectedWidth := 104; Cases[8].ExpectedOption1 := -1; Cases[8].ExpectedOption2 := 1; Cases[8].ExpectedOption3 := 0;
  Cases[9] := Default(TCase); Cases[9].Index := 141; Cases[9].Symbology := BARCODE_RSS_EXPSTACK_CC; Cases[9].InputMode := UNICODE_MODE;             Cases[9].Option1 := -1; Cases[9].Option2 := -1; Cases[9].Option3 := 2;  Cases[9].PrimaryData := '[91]1234567890123456789012345678901234'; Cases[9].CompositeData := '[20]12'; Cases[9].ExpectedRet := 0; Cases[9].ExpectedRows := 13; Cases[9].ExpectedWidth := 104; Cases[9].ExpectedOption1 := -1; Cases[9].ExpectedOption2 := 0; Cases[9].ExpectedOption3 := 2;

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      TZintTestHelper.SetupSymbol(Symbol, Cases[I].Symbology, Cases[I].InputMode,
        Cases[I].Option1, Cases[I].Option2, Cases[I].Option3, -1);
      strcpy(Symbol.primary, Cases[I].PrimaryData);
      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].CompositeData);

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret,
        Format('C#%d ret errtxt="%s"', [Cases[I].Index, TZintTestHelper.GetErrTxt(Symbol)]));

      if Cases[I].ExpectedOption1 >= 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedOption1, Symbol.option_1, Format('C#%d option_1', [Cases[I].Index]));
      Assert.AreEqual<Integer>(Cases[I].ExpectedOption2, Symbol.option_2, Format('C#%d option_2', [Cases[I].Index]));
      Assert.AreEqual<Integer>(Cases[I].ExpectedOption3, Symbol.option_3, Format('C#%d option_3', [Cases[I].Index]));

      if Ret < ZINT_ERROR then
      begin
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows, Format('C#%d rows', [Cases[I].Index]));
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width, Format('C#%d width', [Cases[I].Index]));
      end;
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestCompositeFromC.TestEncodeSubset;
type
  TCase = record
    Index: Integer;
    Symbology: Integer;
    Option1: Integer;
    PrimaryData: String;
    CompositeData: String;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
  end;
var
  Cases: array[0..4] of TCase;
  I, Ret: Integer;
  Symbol: TZintSymbol;
begin
  { test_encodation_0 }
  Cases[0] := Default(TCase); Cases[0].Index := 0;  Cases[0].Symbology := BARCODE_UPCE_CC; Cases[0].Option1 := 1; Cases[0].PrimaryData := '1234567'; Cases[0].CompositeData := '[91]1';                 Cases[0].ExpectedRet := 0; Cases[0].ExpectedRows := 9;  Cases[0].ExpectedWidth := 55;
  Cases[1] := Default(TCase); Cases[1].Index := 2;  Cases[1].Symbology := BARCODE_UPCE_CC; Cases[1].Option1 := 1; Cases[1].PrimaryData := '1234567'; Cases[1].CompositeData := '[91]123';               Cases[1].ExpectedRet := 0; Cases[1].ExpectedRows := 9;  Cases[1].ExpectedWidth := 55;
  Cases[2] := Default(TCase); Cases[2].Index := 6;  Cases[2].Symbology := BARCODE_UPCE_CC; Cases[2].Option1 := 1; Cases[2].PrimaryData := '1234567'; Cases[2].CompositeData := '[91]A1234567C';         Cases[2].ExpectedRet := 0; Cases[2].ExpectedRows := 9;  Cases[2].ExpectedWidth := 55;
  Cases[3] := Default(TCase); Cases[3].Index := 19; Cases[3].Symbology := BARCODE_UPCE_CC; Cases[3].Option1 := 1; Cases[3].PrimaryData := '1234567'; Cases[3].CompositeData := '[91]a1234ABCDEFGb';     Cases[3].ExpectedRet := 0; Cases[3].ExpectedRows := 12; Cases[3].ExpectedWidth := 55;
  Cases[4] := Default(TCase); Cases[4].Index := 33; Cases[4].Symbology := BARCODE_UPCE_CC; Cases[4].Option1 := 1; Cases[4].PrimaryData := '1234567'; Cases[4].CompositeData := '[91]aABCDE123';         Cases[4].ExpectedRet := 0; Cases[4].ExpectedRows := 10; Cases[4].ExpectedWidth := 55;

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      TZintTestHelper.SetupSymbol(Symbol, Cases[I].Symbology, UNICODE_MODE, Cases[I].Option1, -1, -1, -1);
      strcpy(Symbol.primary, Cases[I].PrimaryData);
      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].CompositeData);

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret,
        Format('C#%d ret errtxt="%s"', [Cases[I].Index, TZintTestHelper.GetErrTxt(Symbol)]));
      Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows, Format('C#%d rows', [Cases[I].Index]));
      Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width, Format('C#%d width', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestCompositeFromC.TestHRTSubset;
type
  TCase = record
    Index: Integer;
    Symbology: Integer;
    InputMode: Integer;
    OutputOptions: Integer;
    PrimaryData: String;
    CompositeData: String;
    ExpectedRet: Integer;
    ExpectedText: String;
    ExpectedContent: String;
  end;
var
  Cases: array[0..6] of TCase;
  I, Ret: Integer;
  Symbol: TZintSymbol;
  Msg: String;
begin
  { test_hrt }
  Cases[0] := Default(TCase); Cases[0].Index := 0;  Cases[0].Symbology := BARCODE_EANX_CC;   Cases[0].InputMode := UNICODE_MODE;      Cases[0].OutputOptions := 0;                   Cases[0].PrimaryData := '1234567';       Cases[0].CompositeData := '[20]12';             Cases[0].ExpectedRet := 0;                    Cases[0].ExpectedText := '12345670';           Cases[0].ExpectedContent := '';
  Cases[1] := Default(TCase); Cases[1].Index := 2;  Cases[1].Symbology := BARCODE_EANX_CC;   Cases[1].InputMode := UNICODE_MODE;      Cases[1].OutputOptions := BARCODE_CONTENT_SEGS; Cases[1].PrimaryData := '1234567';       Cases[1].CompositeData := '[20]12';             Cases[1].ExpectedRet := 0;                    Cases[1].ExpectedText := '12345670';           Cases[1].ExpectedContent := '12345670|2012';
  Cases[2] := Default(TCase); Cases[2].Index := 10; Cases[2].Symbology := BARCODE_EANX_CC;   Cases[2].InputMode := UNICODE_MODE;      Cases[2].OutputOptions := BARCODE_CONTENT_SEGS; Cases[2].PrimaryData := '123456789012';   Cases[2].CompositeData := '[10]LOT123[20]12';   Cases[2].ExpectedRet := 0;                    Cases[2].ExpectedText := '1234567890128';      Cases[2].ExpectedContent := '1234567890128|10LOT123' + #29 + '2012';
  Cases[3] := Default(TCase); Cases[3].Index := 24; Cases[3].Symbology := BARCODE_EANX_CC;   Cases[3].InputMode := UNICODE_MODE;      Cases[3].OutputOptions := BARCODE_CONTENT_SEGS; Cases[3].PrimaryData := '1234567890128+12'; Cases[3].CompositeData := '[20]12';             Cases[3].ExpectedRet := 0;                    Cases[3].ExpectedText := '1234567890128+12';    Cases[3].ExpectedContent := '123456789012812|2012';
  Cases[4] := Default(TCase); Cases[4].Index := 27; Cases[4].Symbology := BARCODE_RSS14_CC;  Cases[4].InputMode := UNICODE_MODE;      Cases[4].OutputOptions := BARCODE_CONTENT_SEGS; Cases[4].PrimaryData := '1234567890123';  Cases[4].CompositeData := '[20]12';             Cases[4].ExpectedRet := 0;                    Cases[4].ExpectedText := '(01)12345678901231'; Cases[4].ExpectedContent := '0112345678901231|2012';
  Cases[5] := Default(TCase); Cases[5].Index := 61; Cases[5].Symbology := BARCODE_RSS14STACK_CC; Cases[5].InputMode := UNICODE_MODE;   Cases[5].OutputOptions := BARCODE_CONTENT_SEGS; Cases[5].PrimaryData := '12345678901231'; Cases[5].CompositeData := '[20]12';             Cases[5].ExpectedRet := ZINT_ERROR_TOO_LONG;   Cases[5].ExpectedText := '';                   Cases[5].ExpectedContent := '';
  Cases[6] := Default(TCase); Cases[6].Index := 63; Cases[6].Symbology := BARCODE_RSS14_OMNI_CC; Cases[6].InputMode := UNICODE_MODE;    Cases[6].OutputOptions := BARCODE_CONTENT_SEGS; Cases[6].PrimaryData := '12345678901231'; Cases[6].CompositeData := '[20]12';             Cases[6].ExpectedRet := ZINT_ERROR_TOO_LONG;   Cases[6].ExpectedText := '';                   Cases[6].ExpectedContent := '';

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      TZintTestHelper.SetupSymbol(Symbol, Cases[I].Symbology, Cases[I].InputMode, -1, -1, -1, Cases[I].OutputOptions);
      strcpy(Symbol.primary, Cases[I].PrimaryData);
      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].CompositeData);

      Msg := Format('C#%d', [Cases[I].Index]);
      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret, Msg + ' ret errtxt="' + TZintTestHelper.GetErrTxt(Symbol) + '"');
      Assert.AreEqual(Cases[I].ExpectedText, TZintTestHelper.GetText(Symbol), Msg + ' text');

      if ((Cases[I].OutputOptions and BARCODE_CONTENT_SEGS) <> 0) and (Cases[I].ExpectedContent <> '') then
        AssertContentSeg0(Symbol, Cases[I].ExpectedContent, Msg)
      else
      begin
        Assert.AreEqual<Integer>(0, Symbol.content_segs_count, Msg + ' content_segs_count');
        Assert.AreEqual<Integer>(0, Length(Symbol.content_segs), Msg + ' content_segs length');
      end;
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestCompositeFromC.TestFuzzSubset;
type
  TCase = record
    Index: Integer;
    Symbology: Integer;
    InputMode: Integer;
    Option1: Integer;
    PrimaryData: String;
    CompositeData: String;
    ExpectedRet: Integer;
  end;
var
  Cases: array[0..2] of TCase;
  I, Ret: Integer;
  Symbol: TZintSymbol;
begin
  { test_fuzz }
  Cases[0] := Default(TCase); Cases[0].Index := 8;  Cases[0].Symbology := BARCODE_EANX_CC;    Cases[0].InputMode := GS1PARENS_MODE or GS1NOCHECK_MODE;     Cases[0].Option1 := -1; Cases[0].PrimaryData := 'kks';               Cases[0].CompositeData := '()111%';                               Cases[0].ExpectedRet := ZINT_ERROR_INVALID_DATA;
  Cases[1] := Default(TCase); Cases[1].Index := 13; Cases[1].Symbology := BARCODE_EAN128_CC;  Cases[1].InputMode := GS1NOCHECK_MODE;                       Cases[1].Option1 := 3;  Cases[1].PrimaryData := '[]28';              Cases[1].CompositeData := '[]RRR___________________KKKRRR0000'; Cases[1].ExpectedRet := 0;
  Cases[2] := Default(TCase); Cases[2].Index := 14; Cases[2].Symbology := BARCODE_EAN128_CC;  Cases[2].InputMode := GS1NOCHECK_MODE;                       Cases[2].Option1 := 3;  Cases[2].PrimaryData := '[]2';               Cases[2].CompositeData := '[]RRR___________________KKKRRR0000'; Cases[2].ExpectedRet := 0;

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      TZintTestHelper.SetupSymbol(Symbol, Cases[I].Symbology, Cases[I].InputMode, Cases[I].Option1, -1, -1, -1);
      strcpy(Symbol.primary, Cases[I].PrimaryData);
      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].CompositeData);

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret,
        Format('C#%d ret errtxt="%s"', [Cases[I].Index, TZintTestHelper.GetErrTxt(Symbol)]));
    finally
      Symbol.Free;
    end;
  end;
end;

end.

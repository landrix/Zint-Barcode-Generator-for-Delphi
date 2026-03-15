unit Test_QR;

{
  DUnitX-Tests fuer zint_qr.pas
  Referenz: backend/tests/test_qr.c (Zint commit b3a3c0d)

  Hinweis:
  - test_qr_large ist vollstaendig uebernommen.
  - QR/MicroQR/UPNQR/rMQR Testbloecke sind aktiv und laufen in der DUnitX-Suite.
  - Einzelne C-Bloecke verwenden weiterhin Surrogate, solange zentrale APIs
    fehlen (insb. content_segs/Structured Append).
}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  TestHelper_Zint,
  zint;

type
  [TestFixture]
  TTestQR = class
  public
    [Test] procedure Test_QR_Large_FromC_AllItems;

    [Test]
    procedure Test_QR_Options_FromC;

    [Test]
    procedure Test_QR_Input_FromC;

    [Test]
    procedure Test_QR_GS1_FromC;

    [Test]
    procedure Test_QR_StructuredAppend_Validation_FromC;

    [Test]
    procedure Test_QR_StructuredAppend_Segments_GS1_Warnings;

    [Test]
    procedure Test_QR_Optimize_FromC;

    [Test]
    procedure Test_QR_Encode_FromC;

    [Test]
    procedure Test_QR_EncodeSegs_FromC;

    [Test]
    procedure Test_QR_EncodeSegs_Advanced_FromC;

    [Test]
    procedure Test_QR_RT_FromC;

    [Test]
    procedure Test_QR_RTSegs_FromC;
  end;

  [TestFixture]
  TTestMicroQR = class
  public
    [Test]
    procedure Test_MicroQR_Options_FromC;

  [Test]
  procedure Test_MicroQR_Input_FromC;

  [Test]
  procedure Test_MicroQR_Padding_FromC;

  [Test]
  procedure Test_MicroQR_Optimize_FromC;

  [Test]
  procedure Test_MicroQR_Encode_FromC;

  [Test]
  procedure Test_MicroQR_RT_FromC;
  end;

  [TestFixture]
  TTestUPNQR = class
  public
    [Test]
    procedure Test_UPNQR_Input_FromC;

    [Test]
    procedure Test_UPNQR_Encode_FromC;

    [Test]
    procedure Test_UPNQR_RT_FromC;
  end;

  [TestFixture]
  TTestRMQR = class
  public
    [Test]
    procedure Test_RMQR_Large_FromC;

    [Test]
    procedure Test_RMQR_Options_FromC;

    [Test]
    procedure Test_RMQR_Input_FromC;

    [Test]
    procedure Test_RMQR_GS1_FromC;

    [Test]
    procedure Test_RMQR_Optimize_FromC;

    [Test]
    procedure Test_RMQR_Encode_FromC;

    [Test]
    procedure Test_RMQR_EncodeSegs_FromC;

    [Test]
    procedure Test_RMQR_RT_FromC;

    [Test]
    procedure Test_RMQR_RTSegs_FromC;
  end;

implementation

const
  QR_KANJI_4 = '貫やぐ識禁';
  QR_KANJI_18 = '貫やぐ識禁ぱい再2間変字全ノレ没無8裁';

type
  TQRLargeItem = record
    Option1: Integer;
    Option2: Integer;
    DataLen: Integer;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    ExpectedErrTxt: String;
  end;

  TQROptionsItem = record
    Index: Integer;
    Option1: Integer;
    Option2: Integer;
    Option3: Integer;
    Data: String;
    ExpectedRet: Integer;
    ExpectedSize: Integer;
    ExpectedOption1: Integer;
    ExpectedOption2: Integer;
    ExpectedOption3: Integer;
    ExpectedErrTxt: String;
  end;

  TQRInputItem = record
    Index: Integer;
    InputMode: Integer;
    ECI: Integer;
    Option1: Integer;
    Option3: Integer;
    Data: String;
    ExpectedRet: Integer;
    ExpectedECI: Integer;
    ExpectedSize: Integer;
    ExpectedErrTxt: String;
  end;

  TQRInputRawItem = record
    Index: Integer;
    InputMode: Integer;
    ECI: Integer;
    Option1: Integer;
    Option3: Integer;
    HexData: String;
    ExpectedRet: Integer;
    ExpectedECI: Integer;
    ExpectedSize: Integer;
    ExpectedErrTxt: String;
  end;

  TQRGS1Item = record
    Index: Integer;
    InputMode: Integer;
    Option1: Integer;
    Option3: Integer;
    Data: String;
    ExpectedRet: Integer;
    ExpectedSize: Integer;
    ExpectedErrTxt: String;
  end;

function HexToByteArray(const HexData: String): TArrayOfByte;
var
  s: String;
  i, n: Integer;
begin
  s := StringReplace(HexData, ' ', '', [rfReplaceAll]);
  n := Length(s) div 2;
  SetLength(Result, n + 1);
  for i := 0 to n - 1 do
    Result[i] := StrToInt('$' + Copy(s, i * 2 + 1, 2));
  Result[n] := 0;
end;

procedure TTestQR.Test_QR_Large_FromC_AllItems;
const
  CItems: array[0..17] of TQRLargeItem = (
    (Option1: 1; Option2: 32; DataLen: 2840; ExpectedRet: 0; ExpectedRows: 145; ExpectedWidth: 145; ExpectedErrTxt: ''),
    (Option1: 1; Option2: 32; DataLen: 2841; ExpectedRet: ZERROR_TOO_LONG; ExpectedRows: -1; ExpectedWidth: -1; ExpectedErrTxt: 'Error 569: Input too long for Version 32-L, requires 1956 codewords (maximum 1955)'),
    (Option1: 1; Option2: 33; DataLen: 3009; ExpectedRet: 0; ExpectedRows: 149; ExpectedWidth: 149; ExpectedErrTxt: ''),
    (Option1: 1; Option2: 33; DataLen: 3010; ExpectedRet: ZERROR_TOO_LONG; ExpectedRows: -1; ExpectedWidth: -1; ExpectedErrTxt: 'Error 569: Input too long for Version 33-L, requires 2072 codewords (maximum 2071)'),
    (Option1: 1; Option2: 34; DataLen: 3183; ExpectedRet: 0; ExpectedRows: 153; ExpectedWidth: 153; ExpectedErrTxt: ''),
    (Option1: 1; Option2: 34; DataLen: 3184; ExpectedRet: ZERROR_TOO_LONG; ExpectedRows: -1; ExpectedWidth: -1; ExpectedErrTxt: 'Error 569: Input too long for Version 34-L, requires 2192 codewords (maximum 2191)'),
    (Option1: 1; Option2: 35; DataLen: 3351; ExpectedRet: 0; ExpectedRows: 157; ExpectedWidth: 157; ExpectedErrTxt: ''),
    (Option1: 1; Option2: 35; DataLen: 3352; ExpectedRet: ZERROR_TOO_LONG; ExpectedRows: -1; ExpectedWidth: -1; ExpectedErrTxt: 'Error 569: Input too long for Version 35-L, requires 2307 codewords (maximum 2306)'),
    (Option1: 1; Option2: 36; DataLen: 3537; ExpectedRet: 0; ExpectedRows: 161; ExpectedWidth: 161; ExpectedErrTxt: ''),
    (Option1: 1; Option2: 36; DataLen: 3538; ExpectedRet: ZERROR_TOO_LONG; ExpectedRows: -1; ExpectedWidth: -1; ExpectedErrTxt: 'Error 569: Input too long for Version 36-L, requires 2435 codewords (maximum 2434)'),
    (Option1: 1; Option2: 37; DataLen: 3729; ExpectedRet: 0; ExpectedRows: 165; ExpectedWidth: 165; ExpectedErrTxt: ''),
    (Option1: 1; Option2: 37; DataLen: 3730; ExpectedRet: ZERROR_TOO_LONG; ExpectedRows: -1; ExpectedWidth: -1; ExpectedErrTxt: 'Error 569: Input too long for Version 37-L, requires 2567 codewords (maximum 2566)'),
    (Option1: 1; Option2: 38; DataLen: 3927; ExpectedRet: 0; ExpectedRows: 169; ExpectedWidth: 169; ExpectedErrTxt: ''),
    (Option1: 1; Option2: 38; DataLen: 3928; ExpectedRet: ZERROR_TOO_LONG; ExpectedRows: -1; ExpectedWidth: -1; ExpectedErrTxt: 'Error 569: Input too long for Version 38-L, requires 2703 codewords (maximum 2702)'),
    (Option1: 1; Option2: 39; DataLen: 4087; ExpectedRet: 0; ExpectedRows: 173; ExpectedWidth: 173; ExpectedErrTxt: ''),
    (Option1: 1; Option2: 39; DataLen: 4088; ExpectedRet: ZERROR_TOO_LONG; ExpectedRows: -1; ExpectedWidth: -1; ExpectedErrTxt: 'Error 569: Input too long for Version 39-L, requires 2813 codewords (maximum 2812)'),
    (Option1: 1; Option2: 40; DataLen: 4296; ExpectedRet: 0; ExpectedRows: 177; ExpectedWidth: 177; ExpectedErrTxt: ''),
    (Option1: 1; Option2: 40; DataLen: 4297; ExpectedRet: ZERROR_TOO_LONG; ExpectedRows: -1; ExpectedWidth: -1; ExpectedErrTxt: 'Error 567: Input too long, requires 2957 codewords (maximum 2956)')
  );
var
  i, ret: Integer;
  sym: TZintSymbol;
  data: String;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_QRCODE);
    try
      sym.option_1 := CItems[i].Option1;
      sym.option_2 := CItems[i].Option2;
      sym.eci := 3;
      data := TZintTestHelper.StrRepeat('A', CItems[i].DataLen);
      ret := TZintTestHelper.EncodeData(sym, data);

      Assert.IsTrue(ret = CItems[i].ExpectedRet,
        Format('C#%d ret expected %d got %d', [i, CItems[i].ExpectedRet, ret]));
      Assert.IsTrue(TZintTestHelper.GetErrTxt(sym) = CItems[i].ExpectedErrTxt,
        Format('C#%d errtxt expected "%s" got "%s"', [i, CItems[i].ExpectedErrTxt, TZintTestHelper.GetErrTxt(sym)]));

      if ret < ZINT_ERROR then
      begin
        Assert.IsTrue(sym.rows = CItems[i].ExpectedRows,
          Format('C#%d rows expected %d got %d', [i, CItems[i].ExpectedRows, sym.rows]));
        Assert.IsTrue(sym.width = CItems[i].ExpectedWidth,
          Format('C#%d width expected %d got %d', [i, CItems[i].ExpectedWidth, sym.width]));
      end;
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestQR.Test_QR_Encode_FromC;
type
  TQREncodeItem = record
    Index: Integer;
    Symbology: Integer;
    InputMode: Integer;  // -1 = don't set
    ECI: Integer;        // -1 = don't set
    StructAppIndex: Integer; // 0 = don't set
    StructAppCount: Integer; // 0 = don't set
    StructAppID: string;
    Option1: Integer;    // -1 = don't set
    Option2: Integer;    // -1 = don't set
    Option3: Integer;    // -1 = don't set
    Data: string;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
  end;
const
  // C#22-24 skipped: kanji/max-capacity unicode content
  CItems: array[0..21] of TQREncodeItem = (
    (Index:  0; Symbology: BARCODE_QRCODE;   InputMode: UNICODE_MODE; ECI: -1; StructAppIndex: 0; StructAppCount: 0; StructAppID: '';  Option1: -1; Option2: -1; Option3: -1;      Data: 'QR Code Symbol';                                                    ExpectedRet: 0; ExpectedRows: 21; ExpectedWidth: 21),
    (Index:  1; Symbology: BARCODE_QRCODE;   InputMode: UNICODE_MODE; ECI: -1; StructAppIndex: 0; StructAppCount: 0; StructAppID: '';  Option1: -1; Option2: -1; Option3: 6 shl 8; Data: 'QR Code Symbol';                                                    ExpectedRet: 0; ExpectedRows: 21; ExpectedWidth: 21),
    (Index:  2; Symbology: BARCODE_QRCODE;   InputMode: UNICODE_MODE; ECI: -1; StructAppIndex: 0; StructAppCount: 0; StructAppID: '';  Option1:  2; Option2: -1; Option3: -1;      Data: 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';     ExpectedRet: 0; ExpectedRows: 33; ExpectedWidth: 33),
    (Index:  3; Symbology: BARCODE_QRCODE;   InputMode: UNICODE_MODE; ECI: -1; StructAppIndex: 1; StructAppCount: 4; StructAppID: '1'; Option1: -1; Option2:  2; Option3: -1;      Data: 'ABCDEFGHIJKLMN';                                                     ExpectedRet: 0; ExpectedRows: 21; ExpectedWidth: 21),
    (Index:  4; Symbology: BARCODE_QRCODE;   InputMode: UNICODE_MODE; ECI: -1; StructAppIndex: 2; StructAppCount: 4; StructAppID: '1'; Option1: -1; Option2:  2; Option3: 8 shl 8; Data: 'OPQRSTUVWXYZ0123';                                                     ExpectedRet: 0; ExpectedRows: 21; ExpectedWidth: 21),
    (Index:  5; Symbology: BARCODE_QRCODE;   InputMode: UNICODE_MODE; ECI: -1; StructAppIndex: 3; StructAppCount: 4; StructAppID: '1'; Option1: -1; Option2:  2; Option3: -1;      Data: '456789ABCDEFGHIJ';                                                     ExpectedRet: 0; ExpectedRows: 21; ExpectedWidth: 21),
    (Index:  6; Symbology: BARCODE_QRCODE;   InputMode: UNICODE_MODE; ECI: -1; StructAppIndex: 4; StructAppCount: 4; StructAppID: '1'; Option1: -1; Option2:  2; Option3: -1;      Data: 'KLMNOPQRSTUVWXYZ';                                                     ExpectedRet: 0; ExpectedRows: 21; ExpectedWidth: 21),
    (Index:  7; Symbology: BARCODE_QRCODE;   InputMode: UNICODE_MODE; ECI: -1; StructAppIndex: 0; StructAppCount: 0; StructAppID: '';  Option1:  2; Option2:  1; Option3: -1;      Data: '01234567';                                                          ExpectedRet: 0; ExpectedRows: 21; ExpectedWidth: 21),
    (Index:  8; Symbology: BARCODE_QRCODE;   InputMode: UNICODE_MODE; ECI: -1; StructAppIndex: 0; StructAppCount: 0; StructAppID: '';  Option1:  2; Option2:  1; Option3: 1 shl 8; Data: '01234567';                                                          ExpectedRet: 0; ExpectedRows: 21; ExpectedWidth: 21),
    (Index:  9; Symbology: BARCODE_QRCODE;   InputMode: GS1_MODE;     ECI: -1; StructAppIndex: 0; StructAppCount: 0; StructAppID: '';  Option1:  1; Option2: -1; Option3: -1;      Data: '[01]09501101530003[8200]http://example.com';                        ExpectedRet: 0; ExpectedRows: 25; ExpectedWidth: 25),
    (Index: 10; Symbology: BARCODE_QRCODE;   InputMode: GS1_MODE;     ECI: -1; StructAppIndex: 0; StructAppCount: 0; StructAppID: '';  Option1:  1; Option2: -1; Option3: 2 shl 8; Data: '[01]09501101530003[8200]http://example.com';                        ExpectedRet: 0; ExpectedRows: 25; ExpectedWidth: 25),
    (Index: 11; Symbology: BARCODE_QRCODE;   InputMode: GS1_MODE;     ECI: -1; StructAppIndex: 0; StructAppCount: 0; StructAppID: '';  Option1:  1; Option2: -1; Option3: -1;      Data: '[01]09501101530003[10]640311[21]20FOOPC20';                          ExpectedRet: 0; ExpectedRows: 25; ExpectedWidth: 25),
    (Index: 12; Symbology: BARCODE_QRCODE;   InputMode: GS1_MODE;     ECI: -1; StructAppIndex: 0; StructAppCount: 0; StructAppID: '';  Option1:  1; Option2: -1; Option3: 6 shl 8; Data: '[01]09501101530003[10]640311[21]20FOOPC20';                          ExpectedRet: 0; ExpectedRows: 25; ExpectedWidth: 25),
    (Index: 13; Symbology: BARCODE_QRCODE;   InputMode: GS1_MODE;     ECI: -1; StructAppIndex: 0; StructAppCount: 0; StructAppID: '';  Option1:  2; Option2: -1; Option3: -1;      Data: '[01]00857674002010[8200]http://www.gs1.org/';                        ExpectedRet: 0; ExpectedRows: 29; ExpectedWidth: 29),
    (Index: 14; Symbology: BARCODE_QRCODE;   InputMode: GS1_MODE;     ECI: -1; StructAppIndex: 0; StructAppCount: 0; StructAppID: '';  Option1:  2; Option2: -1; Option3: 6 shl 8; Data: '[01]00857674002010[8200]http://www.gs1.org/';                        ExpectedRet: 0; ExpectedRows: 29; ExpectedWidth: 29),
    (Index: 15; Symbology: BARCODE_HIBC_QR;  InputMode: -1;           ECI:  0; StructAppIndex: 0; StructAppCount: 0; StructAppID: '';  Option1:  2; Option2: -1; Option3: -1;      Data: 'H123ABC01234567890';                                               ExpectedRet: 0; ExpectedRows: 21; ExpectedWidth: 21),
    (Index: 16; Symbology: BARCODE_HIBC_QR;  InputMode: -1;           ECI:  0; StructAppIndex: 0; StructAppCount: 0; StructAppID: '';  Option1:  2; Option2: -1; Option3: -1;      Data: '/EU720060FF0/O523201';                                              ExpectedRet: 0; ExpectedRows: 25; ExpectedWidth: 25),
    (Index: 17; Symbology: BARCODE_HIBC_QR;  InputMode: -1;           ECI:  0; StructAppIndex: 0; StructAppCount: 0; StructAppID: '';  Option1:  2; Option2: -1; Option3: 4 shl 8; Data: '/EU720060FF0/O523201';                                              ExpectedRet: 0; ExpectedRows: 25; ExpectedWidth: 25),
    (Index: 18; Symbology: BARCODE_HIBC_QR;  InputMode: -1;           ECI:  0; StructAppIndex: 0; StructAppCount: 0; StructAppID: '';  Option1:  2; Option2: -1; Option3: -1;      Data: '/KN12345';                                                          ExpectedRet: 0; ExpectedRows: 21; ExpectedWidth: 21),
    (Index: 19; Symbology: BARCODE_HIBC_QR;  InputMode: -1;           ECI:  0; StructAppIndex: 0; StructAppCount: 0; StructAppID: '';  Option1:  2; Option2: -1; Option3: 5 shl 8; Data: '/KN12345';                                                          ExpectedRet: 0; ExpectedRows: 21; ExpectedWidth: 21),
    (Index: 20; Symbology: BARCODE_QRCODE;   InputMode: UNICODE_MODE; ECI: -1; StructAppIndex: 0; StructAppCount: 0; StructAppID: '';  Option1:  1; Option2: -1; Option3: -1;      Data: '12345678901234567890123456789012345678901';                          ExpectedRet: 0; ExpectedRows: 21; ExpectedWidth: 21),
    (Index: 21; Symbology: BARCODE_QRCODE;   InputMode: UNICODE_MODE; ECI: -1; StructAppIndex: 0; StructAppCount: 0; StructAppID: '';  Option1:  2; Option2: -1; Option3: -1;      Data: '12345678901234567890123456789012345678901';                          ExpectedRet: 0; ExpectedRows: 25; ExpectedWidth: 25)
  );
var
  i, ret: Integer;
  sym: TZintSymbol;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(CItems[i].Symbology);
    try
      if CItems[i].InputMode >= 0 then
        sym.input_mode := CItems[i].InputMode;
      if CItems[i].ECI >= 0 then
        sym.eci := CItems[i].ECI;
      if CItems[i].Option1 >= 0 then
        sym.option_1 := CItems[i].Option1;
      if CItems[i].Option2 >= 0 then
        sym.option_2 := CItems[i].Option2;
      if CItems[i].Option3 >= 0 then
        sym.option_3 := CItems[i].Option3;
      if CItems[i].StructAppCount > 0 then
      begin
        sym.structapp.index := CItems[i].StructAppIndex;
        sym.structapp.count := CItems[i].StructAppCount;
        sym.structapp.id := CItems[i].StructAppID;
      end;

      ret := TZintTestHelper.EncodeData(sym, CItems[i].Data);

      Assert.IsTrue(ret = CItems[i].ExpectedRet,
        Format('C#%d ret expected %d got %d errtxt "%s"',
          [CItems[i].Index, CItems[i].ExpectedRet, ret, TZintTestHelper.GetErrTxt(sym)]));
      if ret < ZINT_ERROR then
      begin
        Assert.IsTrue(sym.rows = CItems[i].ExpectedRows,
          Format('C#%d rows expected %d got %d', [CItems[i].Index, CItems[i].ExpectedRows, sym.rows]));
        Assert.IsTrue(sym.width = CItems[i].ExpectedWidth,
          Format('C#%d width expected %d got %d', [CItems[i].Index, CItems[i].ExpectedWidth, sym.width]));
      end;
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestQR.Test_QR_EncodeSegs_FromC;
type
  TQREncodeSegsItem = record
    Index: Integer;
    InputMode: Integer;
    ECI: Integer;
    Option1: Integer;
    Option2: Integer;
    Option3: Integer;
    Seg1: string;
    Seg1ECI: Integer;
    Seg2: string;
    Seg2ECI: Integer;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
  end;
const
  // Segment-vector subset aligned to C test_qr_encode_segs behavior.
  CItems: array[0..5] of TQREncodeSegsItem = (
    (Index: 0; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option2: -1; Option3: 8 shl 8; Seg1: #$00B6; Seg1ECI: -1; Seg2: #$0416; Seg2ECI: -1; ExpectedRet: ZINT_WARN_USES_ECI;    ExpectedRows: 21; ExpectedWidth: 21),
    (Index: 1; InputMode: UNICODE_MODE; ECI: 26; Option1: 4; Option2: -1; Option3: 8 shl 8; Seg1: #$00B6; Seg1ECI: -1; Seg2: #$0416; Seg2ECI: -1; ExpectedRet: 0;                     ExpectedRows: 21; ExpectedWidth: 21),
    (Index: 2; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option2: -1; Option3: 8 shl 8; Seg1: #$0416; Seg1ECI: -1; Seg2: #$00B6; Seg2ECI: -1; ExpectedRet: ZINT_WARN_USES_ECI;    ExpectedRows: 21; ExpectedWidth: 21),
    (Index: 3; InputMode: UNICODE_MODE; ECI: 26; Option1: 4; Option2: -1; Option3: 8 shl 8; Seg1: #$0416; Seg1ECI: -1; Seg2: #$00B6; Seg2ECI: -1; ExpectedRet: 0;                     ExpectedRows: 21; ExpectedWidth: 21),
    (Index: 4; InputMode: DATA_MODE;    ECI: 3; Option1: 4; Option2: -1; Option3: 8 shl 8; Seg1: #$00B6; Seg1ECI: -1; Seg2: #$0416; Seg2ECI: 20; ExpectedRet: 0;                     ExpectedRows: 21; ExpectedWidth: 21),
    (Index: 5; InputMode: UNICODE_MODE; ECI: 3; Option1: 4; Option2: -1; Option3: 8 shl 8; Seg1: #$00B6; Seg1ECI: -1; Seg2: #$0416; Seg2ECI: 20; ExpectedRet: ZERROR_INVALID_OPTION; ExpectedRows: -1; ExpectedWidth: -1)
  );
var
  i, ret: Integer;
  sym: TZintSymbol;
  segs: TZintSegments;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_QRCODE);
    try
      sym.input_mode := CItems[i].InputMode;
      if CItems[i].ECI >= 0 then
        sym.eci := CItems[i].ECI;
      if CItems[i].Option1 >= 0 then
        sym.option_1 := CItems[i].Option1;
      if CItems[i].Option2 >= 0 then
        sym.option_2 := CItems[i].Option2;
      if CItems[i].Option3 >= 0 then
        sym.option_3 := CItems[i].Option3;

      SetLength(segs, 2);

      segs[0].Source := TEncoding.UTF8.GetBytes(CItems[i].Seg1);
      segs[0].Length := Length(segs[0].Source);
      segs[0].ECI := CItems[i].Seg1ECI;
      segs[0].SourceMode := -1;

      segs[1].Source := TEncoding.UTF8.GetBytes(CItems[i].Seg2);
      segs[1].Length := Length(segs[1].Source);
      segs[1].ECI := CItems[i].Seg2ECI;
      segs[1].SourceMode := -1;

      ret := ZBarcode_Encode_Segs(sym, segs);

      Assert.IsTrue(ret = CItems[i].ExpectedRet,
        Format('C#%d ret expected %d got %d errtxt "%s"',
          [CItems[i].Index, CItems[i].ExpectedRet, ret, TZintTestHelper.GetErrTxt(sym)]));

      if ret < ZINT_ERROR then
      begin
        Assert.IsTrue(sym.rows = CItems[i].ExpectedRows,
          Format('C#%d rows expected %d got %d', [CItems[i].Index, CItems[i].ExpectedRows, sym.rows]));
        Assert.IsTrue(sym.width = CItems[i].ExpectedWidth,
          Format('C#%d width expected %d got %d', [CItems[i].Index, CItems[i].ExpectedWidth, sym.width]));
      end;
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestQR.Test_QR_EncodeSegs_Advanced_FromC;
type
  TQREncodeSegsAdvItem = record
    Index: Integer;
    InputMode: Integer;
    Option1: Integer;
    Option2: Integer;
    Option3: Integer;
    StructAppIndex: Integer;
    StructAppCount: Integer;
    StructAppID: string;
    Seg1: string;
    Seg1ECI: Integer;
    Seg2: string;
    Seg2ECI: Integer;
    Seg3: string;
    Seg3ECI: Integer;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
  end;
const
  // Additional upstream parity cases from C test_qr_encode_segs (items 6..7).
  CItems: array[0..1] of TQREncodeSegsAdvItem = (
    (Index: 6; InputMode: UNICODE_MODE; Option1: 2; Option2: -1; Option3: 3 shl 8; StructAppIndex: 0; StructAppCount: 0; StructAppID: '';    Seg1: 'éé'; Seg1ECI: -1; Seg2: 'กขฯ'; Seg2ECI: -1; Seg3: 'βββ'; Seg3ECI: -1; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedRows: 21; ExpectedWidth: 21),
    (Index: 7; InputMode: UNICODE_MODE; Option1: -1; Option2: -1; Option3: -1;     StructAppIndex: 2; StructAppCount: 3; StructAppID: '123'; Seg1: 'éé'; Seg1ECI: 23; Seg2: 'กขฯ'; Seg2ECI: 13; Seg3: 'βββ'; Seg3ECI: 9;  ExpectedRet: 0;                   ExpectedRows: 25; ExpectedWidth: 25)
  );
var
  i, ret: Integer;
  sym: TZintSymbol;
  segs: TZintSegments;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_QRCODE);
    try
      sym.input_mode := CItems[i].InputMode;
      if CItems[i].Option1 >= 0 then
        sym.option_1 := CItems[i].Option1;
      if CItems[i].Option2 >= 0 then
        sym.option_2 := CItems[i].Option2;
      if CItems[i].Option3 >= 0 then
        sym.option_3 := CItems[i].Option3;
      if CItems[i].StructAppCount > 0 then
      begin
        sym.structapp.index := CItems[i].StructAppIndex;
        sym.structapp.count := CItems[i].StructAppCount;
        sym.structapp.id := CItems[i].StructAppID;
      end;

      SetLength(segs, 3);

      segs[0].Source := TEncoding.UTF8.GetBytes(CItems[i].Seg1);
      segs[0].Length := Length(segs[0].Source);
      segs[0].ECI := CItems[i].Seg1ECI;
      segs[0].SourceMode := -1;

      segs[1].Source := TEncoding.UTF8.GetBytes(CItems[i].Seg2);
      segs[1].Length := Length(segs[1].Source);
      segs[1].ECI := CItems[i].Seg2ECI;
      segs[1].SourceMode := -1;

      segs[2].Source := TEncoding.UTF8.GetBytes(CItems[i].Seg3);
      segs[2].Length := Length(segs[2].Source);
      segs[2].ECI := CItems[i].Seg3ECI;
      segs[2].SourceMode := -1;

      ret := ZBarcode_Encode_Segs(sym, segs);

      Assert.IsTrue(ret = CItems[i].ExpectedRet,
        Format('C#%d ret expected %d got %d errtxt "%s"',
          [CItems[i].Index, CItems[i].ExpectedRet, ret, TZintTestHelper.GetErrTxt(sym)]));

      if ret < ZINT_ERROR then
      begin
        Assert.IsTrue(sym.rows = CItems[i].ExpectedRows,
          Format('C#%d rows expected %d got %d', [CItems[i].Index, CItems[i].ExpectedRows, sym.rows]));
        Assert.IsTrue(sym.width = CItems[i].ExpectedWidth,
          Format('C#%d width expected %d got %d', [CItems[i].Index, CItems[i].ExpectedWidth, sym.width]));
      end;
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestQR.Test_QR_GS1_FromC;
const
  CItems: array[0..14] of TQRGS1Item = (
    (Index: 0; InputMode: GS1_MODE; Option1: 4; Option3: 7 shl 8; Data: '[01]12345678901231'; ExpectedRet: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 1; InputMode: GS1_MODE or GS1PARENS_MODE; Option1: 4; Option3: 7 shl 8; Data: '(01)12345678901231'; ExpectedRet: 0; ExpectedSize: 25; ExpectedErrTxt: ''),
    (Index: 2; InputMode: GS1_MODE; Option1: 2; Option3: 4 shl 8; Data: '[01]04912345123459[15]970331[30]128[10]ABC123'; ExpectedRet: 0; ExpectedSize: 25; ExpectedErrTxt: ''),
    (Index: 3; InputMode: GS1_MODE or GS1PARENS_MODE; Option1: 2; Option3: 4 shl 8; Data: '(01)04912345123459(15)970331(30)128(10)ABC123'; ExpectedRet: 0; ExpectedSize: 29; ExpectedErrTxt: ''),
    (Index: 4; InputMode: GS1_MODE; Option1: 4; Option3: 7 shl 8; Data: '[91])'; ExpectedRet: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 5; InputMode: GS1_MODE or ESCAPE_MODE or GS1PARENS_MODE; Option1: 4; Option3: 7 shl 8; Data: '(91)\\)'; ExpectedRet: 0; ExpectedSize: -1; ExpectedErrTxt: ''),
    (Index: 6; InputMode: GS1_MODE; Option1: 3; Option3: -1; Data: '[91]12%[20]12'; ExpectedRet: 0; ExpectedSize: -1; ExpectedErrTxt: ''),
    (Index: 7; InputMode: GS1_MODE; Option1: 3; Option3: 1 shl 8; Data: '[91]123%[20]12'; ExpectedRet: 0; ExpectedSize: -1; ExpectedErrTxt: ''),
    (Index: 8; InputMode: GS1_MODE; Option1: 3; Option3: 6 shl 8; Data: '[91]1234%[20]12'; ExpectedRet: 0; ExpectedSize: -1; ExpectedErrTxt: ''),
    (Index: 9; InputMode: GS1_MODE; Option1: 3; Option3: -1; Data: '[91]12345%[20]12'; ExpectedRet: 0; ExpectedSize: -1; ExpectedErrTxt: ''),
    (Index: 10; InputMode: GS1_MODE; Option1: 3; Option3: 8 shl 8; Data: '[91]%%[20]12'; ExpectedRet: 0; ExpectedSize: -1; ExpectedErrTxt: ''),
    (Index: 11; InputMode: GS1_MODE; Option1: 3; Option3: 6 shl 8; Data: '[91]%%%[20]12'; ExpectedRet: 0; ExpectedSize: -1; ExpectedErrTxt: ''),
    (Index: 12; InputMode: GS1_MODE; Option1: 3; Option3: 6 shl 8; Data: '[91]A%%%%1234567890123AA%'; ExpectedRet: 0; ExpectedSize: -1; ExpectedErrTxt: ''),
    (Index: 13; InputMode: GS1_MODE; Option1: 1; Option3: -1; Data: '[91]%23%%6789%%%34567%%%%234%%%%%'; ExpectedRet: 0; ExpectedSize: -1; ExpectedErrTxt: ''),
    (Index: 14; InputMode: GS1_MODE; Option1: 2; Option3: 5 shl 8; Data: '[91]ABCDEFGHI[92]ABCDEF'; ExpectedRet: 0; ExpectedSize: -1; ExpectedErrTxt: '')
  );
var
  i, ret: Integer;
  sym: TZintSymbol;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_QRCODE);
    try
      sym.input_mode := CItems[i].InputMode;
      if CItems[i].Option1 >= 0 then
        sym.option_1 := CItems[i].Option1;
      if CItems[i].Option3 >= 0 then
        sym.option_3 := CItems[i].Option3;

      ret := TZintTestHelper.EncodeData(sym, CItems[i].Data);

      Assert.IsTrue(ret = CItems[i].ExpectedRet,
        Format('C#%d ret expected %d got %d errtxt "%s"',
          [CItems[i].Index, CItems[i].ExpectedRet, ret, TZintTestHelper.GetErrTxt(sym)]));
      Assert.IsTrue(TZintTestHelper.GetErrTxt(sym) = CItems[i].ExpectedErrTxt,
        Format('C#%d errtxt expected "%s" got "%s"', [CItems[i].Index, CItems[i].ExpectedErrTxt, TZintTestHelper.GetErrTxt(sym)]));

      if ret < ZINT_ERROR then
      begin
        if CItems[i].ExpectedSize >= 0 then
        begin
          Assert.IsTrue(sym.rows = CItems[i].ExpectedSize,
            Format('C#%d rows expected %d got %d', [CItems[i].Index, CItems[i].ExpectedSize, sym.rows]));
          Assert.IsTrue(sym.width = CItems[i].ExpectedSize,
            Format('C#%d width expected %d got %d', [CItems[i].Index, CItems[i].ExpectedSize, sym.width]));
        end;
      end;
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestQR.Test_QR_StructuredAppend_Validation_FromC;
type
  TQRStructAppValidationItem = record
    Index: Integer;
    InputMode: Integer;
    ECI: Integer;
    Option1: Integer;
    Data: string;
    StructAppIndex: Integer;
    StructAppCount: Integer;
    StructAppID: string;
    ExpectedRet: Integer;
    ExpectedErrTxt: string;
    ExpectedSize: Integer;
  end;
const
  CItems: array[0..7] of TQRStructAppValidationItem = (
    (Index: 39; InputMode: UNICODE_MODE; ECI: -1; Option1: 4; Data: '12345678901'; StructAppIndex: 3; StructAppCount: 17; StructAppID: '123';  ExpectedRet: ZERROR_INVALID_OPTION; ExpectedErrTxt: 'Error 750: Structured Append count ''17'' out of range (2 to 16)';            ExpectedSize: -1),
    (Index: 40; InputMode: UNICODE_MODE; ECI: -1; Option1: 4; Data: '12345678901'; StructAppIndex: 3; StructAppCount: 2;  StructAppID: '123';  ExpectedRet: ZERROR_INVALID_OPTION; ExpectedErrTxt: 'Error 751: Structured Append index ''3'' out of range (1 to count 2)';          ExpectedSize: -1),
    (Index: 41; InputMode: UNICODE_MODE; ECI: -1; Option1: 4; Data: '12345678901'; StructAppIndex: 1; StructAppCount: 2;  StructAppID: '1234'; ExpectedRet: ZERROR_INVALID_OPTION; ExpectedErrTxt: 'Error 752: Structured Append ID length 4 too long (3 digit maximum)';          ExpectedSize: -1),
    (Index: 42; InputMode: UNICODE_MODE; ECI: -1; Option1: 4; Data: '12345678901'; StructAppIndex: 1; StructAppCount: 2;  StructAppID: '12A';  ExpectedRet: ZERROR_INVALID_OPTION; ExpectedErrTxt: 'Error 753: Invalid Structured Append ID (digits only)';                         ExpectedSize: -1),
    (Index: 43; InputMode: UNICODE_MODE; ECI: -1; Option1: 4; Data: '12345678901'; StructAppIndex: 1; StructAppCount: 2;  StructAppID: '256';  ExpectedRet: ZERROR_INVALID_OPTION; ExpectedErrTxt: 'Error 754: Structured Append ID value ''256'' out of range (0 to 255)';        ExpectedSize: -1),
    (Index: 44; InputMode: GS1_MODE;     ECI: 3;  Option1: 4; Data: '[20]12';      StructAppIndex: 0; StructAppCount: 0;  StructAppID: '';     ExpectedRet: ZINT_WARN_NONCOMPLIANT; ExpectedErrTxt: 'Warning 755: Using ECI in GS1 mode not supported by GS1 standards';             ExpectedSize: 21),
    (Index: 45; InputMode: GS1_MODE;     ECI: -1; Option1: 4; Data: '[20]12';      StructAppIndex: 1; StructAppCount: 2;  StructAppID: '';     ExpectedRet: ZINT_WARN_NONCOMPLIANT; ExpectedErrTxt: 'Warning 756: Using Structured Append in GS1 mode not supported by GS1 standards'; ExpectedSize: 21),
    (Index: 46; InputMode: GS1_MODE;     ECI: 3;  Option1: 4; Data: '[20]12';      StructAppIndex: 1; StructAppCount: 2;  StructAppID: '';     ExpectedRet: ZINT_WARN_NONCOMPLIANT; ExpectedErrTxt: 'Warning 755: Using ECI in GS1 mode not supported by GS1 standards';             ExpectedSize: 21)
  );
var
  i, ret: Integer;
  sym: TZintSymbol;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_QRCODE);
    try
      sym.input_mode := CItems[i].InputMode;
      if CItems[i].ECI >= 0 then
        sym.eci := CItems[i].ECI;
      if CItems[i].Option1 >= 0 then
        sym.option_1 := CItems[i].Option1;
      if CItems[i].StructAppCount > 0 then
      begin
        sym.structapp.index := CItems[i].StructAppIndex;
        sym.structapp.count := CItems[i].StructAppCount;
        sym.structapp.id := CItems[i].StructAppID;
      end;

      ret := TZintTestHelper.EncodeData(sym, CItems[i].Data);

      Assert.IsTrue(ret = CItems[i].ExpectedRet,
        Format('C#%d ret expected %d got %d errtxt "%s"',
          [CItems[i].Index, CItems[i].ExpectedRet, ret, TZintTestHelper.GetErrTxt(sym)]));
      Assert.IsTrue(TZintTestHelper.GetErrTxt(sym) = CItems[i].ExpectedErrTxt,
        Format('C#%d errtxt expected "%s" got "%s"', [CItems[i].Index, CItems[i].ExpectedErrTxt, TZintTestHelper.GetErrTxt(sym)]));

      if (ret < ZINT_ERROR) and (CItems[i].ExpectedSize >= 0) then
      begin
        Assert.IsTrue(sym.rows = CItems[i].ExpectedSize,
          Format('C#%d rows expected %d got %d', [CItems[i].Index, CItems[i].ExpectedSize, sym.rows]));
        Assert.IsTrue(sym.width = CItems[i].ExpectedSize,
          Format('C#%d width expected %d got %d', [CItems[i].Index, CItems[i].ExpectedSize, sym.width]));
      end;
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestQR.Test_QR_StructuredAppend_Segments_GS1_Warnings;
type
  TQRSegGS1WarnItem = record
    Index: Integer;
    ECI: Integer;
    Seg1: string;
    Seg1ECI: Integer;
    Seg2: string;
    Seg2ECI: Integer;
    StructAppIndex: Integer;
    StructAppCount: Integer;
    StructAppID: string;
    ExpectedRet: Integer;
    ExpectedErrTxt: string;
  end;
const
  CItems: array[0..3] of TQRSegGS1WarnItem = (
    (Index: 0; ECI: -1; Seg1: '[20]12'; Seg1ECI: 20; Seg2: ''; Seg2ECI: -1; StructAppIndex: 0; StructAppCount: 0; StructAppID: ''; ExpectedRet: ZINT_WARN_NONCOMPLIANT; ExpectedErrTxt: 'Warning 755: Using ECI in GS1 mode not supported by GS1 standards'),
    (Index: 1; ECI: -1; Seg1: '[20]';   Seg1ECI: 20; Seg2: '12'; Seg2ECI: -1; StructAppIndex: 1; StructAppCount: 2; StructAppID: ''; ExpectedRet: ZINT_WARN_NONCOMPLIANT; ExpectedErrTxt: 'Warning 755: Using ECI in GS1 mode not supported by GS1 standards'),
    (Index: 2; ECI: -1; Seg1: '[20]12'; Seg1ECI: -1; Seg2: ''; Seg2ECI: -1; StructAppIndex: 1; StructAppCount: 2; StructAppID: ''; ExpectedRet: ZINT_WARN_NONCOMPLIANT; ExpectedErrTxt: 'Warning 756: Using Structured Append in GS1 mode not supported by GS1 standards'),
    (Index: 3; ECI: 3;  Seg1: '[20]12'; Seg1ECI: -1; Seg2: ''; Seg2ECI: -1; StructAppIndex: 1; StructAppCount: 2; StructAppID: ''; ExpectedRet: ZINT_WARN_NONCOMPLIANT; ExpectedErrTxt: 'Warning 755: Using ECI in GS1 mode not supported by GS1 standards')
  );
var
  i, ret, segCount: Integer;
  sym: TZintSymbol;
  segs: TZintSegments;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_QRCODE);
    try
      sym.input_mode := GS1_MODE;
      sym.option_1 := 4;
      if CItems[i].ECI >= 0 then
        sym.eci := CItems[i].ECI;
      if CItems[i].StructAppCount > 0 then
      begin
        sym.structapp.index := CItems[i].StructAppIndex;
        sym.structapp.count := CItems[i].StructAppCount;
        sym.structapp.id := CItems[i].StructAppID;
      end;

      if CItems[i].Seg2 <> '' then
        segCount := 2
      else
        segCount := 1;
      SetLength(segs, segCount);

      segs[0].Source := TEncoding.UTF8.GetBytes(CItems[i].Seg1);
      segs[0].Length := Length(segs[0].Source);
      segs[0].ECI := CItems[i].Seg1ECI;
      segs[0].SourceMode := -1;

      if segCount = 2 then
      begin
        segs[1].Source := TEncoding.UTF8.GetBytes(CItems[i].Seg2);
        segs[1].Length := Length(segs[1].Source);
        segs[1].ECI := CItems[i].Seg2ECI;
        segs[1].SourceMode := -1;
      end;

      ret := ZBarcode_Encode_Segs(sym, segs);

      Assert.IsTrue(ret = CItems[i].ExpectedRet,
        Format('SegWarn#%d ret expected %d got %d errtxt "%s"',
          [CItems[i].Index, CItems[i].ExpectedRet, ret, TZintTestHelper.GetErrTxt(sym)]));
      Assert.IsTrue(TZintTestHelper.GetErrTxt(sym) = CItems[i].ExpectedErrTxt,
        Format('SegWarn#%d errtxt expected "%s" got "%s"',
          [CItems[i].Index, CItems[i].ExpectedErrTxt, TZintTestHelper.GetErrTxt(sym)]));
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestQR.Test_QR_Input_FromC;
const
  CItems: array[0..111] of TQRInputItem = (
    (Index: 1; InputMode: UNICODE_MODE; ECI: 3; Option1: 4; Option3: 0; Data: #$00E9; ExpectedRet: 0; ExpectedECI: 3; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 3; InputMode: UNICODE_MODE; ECI: 26; Option1: 4; Option3: 0; Data: #$00E9; ExpectedRet: 0; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 2; InputMode: UNICODE_MODE; ECI: 20; Option1: -1; Option3: 0; Data: #$00E9; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 8; InputMode: UNICODE_MODE; ECI: 20; Option1: 4; Option3: 0; Data: #$03B2; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 9; InputMode: UNICODE_MODE; ECI: 26; Option1: 4; Option3: 5 shl 8; Data: #$03B2; ExpectedRet: 0; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 10; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: 4 shl 8; Data: #$03B2; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 13; InputMode: UNICODE_MODE; ECI: 20; Option1: -1; Option3: 0; Data: #$0E01; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 14; InputMode: UNICODE_MODE; ECI: 26; Option1: 4; Option3: 8 shl 8; Data: #$0E01; ExpectedRet: 0; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 15; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: 3 shl 8; Data: #$0E01; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 18; InputMode: UNICODE_MODE; ECI: 20; Option1: 4; Option3: 7 shl 8; Data: #$0416; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 19; InputMode: UNICODE_MODE; ECI: 26; Option1: 4; Option3: 0; Data: #$0416; ExpectedRet: 0; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 20; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: 0; Data: #$0416; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 22; InputMode: UNICODE_MODE; ECI: 20; Option1: -1; Option3: 0; Data: #$0E81; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 23; InputMode: UNICODE_MODE; ECI: 26; Option1: 4; Option3: 0; Data: #$0E81; ExpectedRet: 0; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 24; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: 8 shl 8; Data: #$0E81; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 25; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 2 shl 8; Data: '\\'; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 26; InputMode: UNICODE_MODE; ECI: 20; Option1: 4; Option3: 4 shl 8; Data: '\\'; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 27; InputMode: UNICODE_MODE; ECI: 20; Option1: 4; Option3: 0; Data: '['; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 28; InputMode: UNICODE_MODE; ECI: 20; Option1: 4; Option3: 3 shl 8; Data: #$007F; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 29; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 2 shl 8; Data: #$00A5; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 30; InputMode: UNICODE_MODE; ECI: 3; Option1: 4; Option3: 3 shl 8; Data: #$00A5; ExpectedRet: 0; ExpectedECI: 3; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 31; InputMode: UNICODE_MODE; ECI: 20; Option1: 4; Option3: 5 shl 8; Data: #$00A5; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 32; InputMode: UNICODE_MODE; ECI: 26; Option1: 4; Option3: 6 shl 8; Data: #$00A5; ExpectedRet: 0; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 33; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: 2 shl 8; Data: #$00A5; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 34; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 2 shl 8; Data: #$FF65; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 35; InputMode: UNICODE_MODE; ECI: 3; Option1: -1; Option3: 0; Data: #$FF65; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 575: Invalid character in input for ECI ''3'''),
    (Index: 36; InputMode: UNICODE_MODE; ECI: 20; Option1: 4; Option3: 7 shl 8; Data: #$FF65; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 37; InputMode: UNICODE_MODE; ECI: 26; Option1: 4; Option3: 0; Data: #$FF65; ExpectedRet: 0; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 38; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: 8 shl 8; Data: #$FF65; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 39; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 0; Data: #$00BF; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 40; InputMode: UNICODE_MODE; ECI: 3; Option1: 4; Option3: 8 shl 8; Data: #$00BF; ExpectedRet: 0; ExpectedECI: 3; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 41; InputMode: UNICODE_MODE; ECI: 20; Option1: -1; Option3: 0; Data: #$00BF; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 42; InputMode: UNICODE_MODE; ECI: 26; Option1: 4; Option3: 2 shl 8; Data: #$00BF; ExpectedRet: 0; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 43; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: 3 shl 8; Data: #$00BF; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 44; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 0; Data: #$FF7F; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 45; InputMode: UNICODE_MODE; ECI: 3; Option1: -1; Option3: 0; Data: #$FF7F; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 575: Invalid character in input for ECI ''3'''),
    (Index: 46; InputMode: UNICODE_MODE; ECI: 20; Option1: 4; Option3: 6 shl 8; Data: #$FF7F; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 47; InputMode: UNICODE_MODE; ECI: 26; Option1: 4; Option3: 4 shl 8; Data: #$FF7F; ExpectedRet: 0; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 48; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: 0; Data: #$FF7F; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 49; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 0; Data: '~'; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 50; InputMode: UNICODE_MODE; ECI: 3; Option1: 4; Option3: 0; Data: '~'; ExpectedRet: 0; ExpectedECI: 3; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 51; InputMode: UNICODE_MODE; ECI: 20; Option1: -1; Option3: 0; Data: '~'; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 52; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 0; Data: #$203E; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 53; InputMode: UNICODE_MODE; ECI: 3; Option1: -1; Option3: 0; Data: #$203E; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 575: Invalid character in input for ECI ''3'''),
    (Index: 54; InputMode: UNICODE_MODE; ECI: 20; Option1: 4; Option3: 7 shl 8; Data: #$203E; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 55; InputMode: UNICODE_MODE; ECI: 26; Option1: 4; Option3: 1 shl 8; Data: #$203E; ExpectedRet: 0; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 56; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: 0; Data: #$203E; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 57; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 0; Data: #$70B9; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 58; InputMode: UNICODE_MODE; ECI: 3; Option1: -1; Option3: 0; Data: #$70B9; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 575: Invalid character in input for ECI ''3'''),
    (Index: 59; InputMode: UNICODE_MODE; ECI: 20; Option1: 4; Option3: 3 shl 8; Data: #$70B9; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 60; InputMode: UNICODE_MODE; ECI: 26; Option1: 4; Option3: 0; Data: #$70B9; ExpectedRet: 0; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 61; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: 6 shl 8; Data: #$70B9; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 64; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 0; Data: #$00A5#$FF65#$70B9; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 65; InputMode: UNICODE_MODE; ECI: 3; Option1: -1; Option3: 0; Data: #$00A5#$FF65#$70B9; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 575: Invalid character in input for ECI ''3'''),
    (Index: 66; InputMode: UNICODE_MODE; ECI: 20; Option1: 4; Option3: 4 shl 8; Data: #$00A5#$FF65#$70B9; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 67; InputMode: UNICODE_MODE; ECI: 26; Option1: 3; Option3: 7 shl 8; Data: #$00A5#$FF65#$70B9; ExpectedRet: 0; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 69; InputMode: DATA_MODE; ECI: 0; Option1: 3; Option3: 0; Data: #$00A5#$FF65#$70B9; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 70; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 8 shl 8; Data: #$70B9#$8317; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 71; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 0; Data: #$70B9#$8317#$30C6; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 72; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 0; Data: #$70B9#$8317#$30C6#$70B9; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 73; InputMode: UNICODE_MODE; ECI: 0; Option1: 3; Option3: 0; Data: #$70B9#$8317#$30C6#$70B9#$8317; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: -1; ExpectedErrTxt: ''),
    (Index: 74; InputMode: UNICODE_MODE; ECI: 0; Option1: 3; Option3: 1 shl 8; Data: #$70B9#$8317#$30C6#$70B9#$8317#$30C6; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: -1; ExpectedErrTxt: ''),
    (Index: 75; InputMode: UNICODE_MODE; ECI: 0; Option1: 2; Option3: 4 shl 8; Data: #$70B9#$8317#$30C6#$70B9#$8317#$30C6#$FF7F; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: -1; ExpectedErrTxt: ''),
    (Index: 79; InputMode: DATA_MODE; ECI: 0; Option1: 3; Option3: 8 shl 8; Data: #$00C1#$0201#$0201#$0201#$0201#$0201#$0201#$0202#$00A2; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 80; InputMode: DATA_MODE; ECI: 0; Option1: 3; Option3: ZINT_FULL_MULTIBYTE or (8 shl 8); Data: #$00C1#$0201#$0201#$0201#$0201#$0201#$0201#$0202#$00A2; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 81; InputMode: DATA_MODE; ECI: 0; Option1: 3; Option3: 4 shl 8; Data: #$00C1#$0201#$0201#$0201#$0201#$0201#$0201#$0201#$0202#$00A2; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 82; InputMode: DATA_MODE; ECI: 0; Option1: 3; Option3: ZINT_FULL_MULTIBYTE or (4 shl 8); Data: #$00C1#$0201#$0201#$0201#$0201#$0201#$0201#$0201#$0202#$00A2; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 83; InputMode: UNICODE_MODE; ECI: 0; Option1: 3; Option3: 8 shl 8; Data: #$00C1#$0201#$0201#$0201#$0201#$0201#$0201#$0202#$00A2; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedECI: 26; ExpectedSize: 25; ExpectedErrTxt: ''),
    (Index: 84; InputMode: UNICODE_MODE; ECI: 0; Option1: 3; Option3: ZINT_FULL_MULTIBYTE or (8 shl 8); Data: #$00C1#$0201#$0201#$0201#$0201#$0201#$0201#$0202#$00A2; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedECI: 26; ExpectedSize: 25; ExpectedErrTxt: ''),
    (Index: 85; InputMode: UNICODE_MODE; ECI: 0; Option1: 2; Option3: 1 shl 8; Data: #$00C1#$0201#$0201#$0201#$0201#$0201#$0201#$0201#$0202#$00A2; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedECI: 26; ExpectedSize: 25; ExpectedErrTxt: ''),
    (Index: 86; InputMode: UNICODE_MODE; ECI: 0; Option1: 2; Option3: ZINT_FULL_MULTIBYTE or (1 shl 8); Data: #$00C1#$0201#$0201#$0201#$0201#$0201#$0201#$0201#$0202#$00A2; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedECI: 26; ExpectedSize: 25; ExpectedErrTxt: ''),
    (Index: 99; InputMode: UNICODE_MODE; ECI: 20; Option1: 4; Option3: 2 shl 8; Data: #$30C6; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 100; InputMode: UNICODE_MODE; ECI: 20; Option1: 4; Option3: 4 shl 8; Data: #$30C6#$30C6; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 90; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 7 shl 8; Data: #$02D8; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 91; InputMode: UNICODE_MODE; ECI: 4; Option1: 4; Option3: 7 shl 8; Data: #$02D8; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 92; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 7 shl 8; Data: #$0126; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 93; InputMode: UNICODE_MODE; ECI: 5; Option1: 4; Option3: 7 shl 8; Data: #$0126; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 94; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 0; Data: #$0138; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 95; InputMode: UNICODE_MODE; ECI: 6; Option1: 4; Option3: 0; Data: #$0138; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 96; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 8 shl 8; Data: #$0218; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 97; InputMode: UNICODE_MODE; ECI: 18; Option1: 4; Option3: 8 shl 8; Data: #$0218; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 104; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 7 shl 8; Data: #$0490; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 105; InputMode: UNICODE_MODE; ECI: 22; Option1: 4; Option3: 7 shl 8; Data: #$0490; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 106; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 6 shl 8; Data: #$02DC; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 107; InputMode: UNICODE_MODE; ECI: 23; Option1: 4; Option3: 6 shl 8; Data: #$02DC; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 108; InputMode: UNICODE_MODE; ECI: 24; Option1: 4; Option3: 5 shl 8; Data: #$067E; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 109; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 7 shl 8; Data: #$1000; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 110; InputMode: UNICODE_MODE; ECI: 25; Option1: 4; Option3: 0; Data: #$1000; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 111; InputMode: UNICODE_MODE; ECI: 25; Option1: 4; Option3: 4 shl 8; Data: #$1000#$1000; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 112; InputMode: UNICODE_MODE; ECI: 25; Option1: 4; Option3: 0; Data: '12'; ExpectedRet: 0; ExpectedECI: 25; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 113; InputMode: UNICODE_MODE; ECI: 27; Option1: 4; Option3: 4 shl 8; Data: '@'; ExpectedRet: 0; ExpectedECI: 27; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 114; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 0; Data: #$9F98; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 115; InputMode: UNICODE_MODE; ECI: 28; Option1: 4; Option3: 3 shl 8; Data: #$9F98; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 116; InputMode: UNICODE_MODE; ECI: 28; Option1: 4; Option3: 4 shl 8; Data: #$9F98#$9F98; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 117; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 8 shl 8; Data: #$9F44; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 118; InputMode: UNICODE_MODE; ECI: 29; Option1: 4; Option3: 0; Data: #$9F44; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 119; InputMode: UNICODE_MODE; ECI: 29; Option1: 4; Option3: 4 shl 8; Data: #$9F44#$9F44; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 120; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 8 shl 8; Data: #$AC00; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 121; InputMode: UNICODE_MODE; ECI: 30; Option1: 4; Option3: 0; Data: #$AC00; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 122; InputMode: UNICODE_MODE; ECI: 30; Option1: 4; Option3: 0; Data: #$AC00#$AC00; ExpectedRet: ZERROR_INVALID_DATA; ExpectedECI: -1; ExpectedSize: -1; ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 123; InputMode: UNICODE_MODE; ECI: 170; Option1: 4; Option3: 2 shl 8; Data: '?'; ExpectedRet: 0; ExpectedECI: 170; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 125; InputMode: UNICODE_MODE; ECI: 900; Option1: 4; Option3: 8 shl 8; Data: #$00E9; ExpectedRet: 0; ExpectedECI: 900; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 126; InputMode: UNICODE_MODE; ECI: 16384; Option1: 4; Option3: 8 shl 8; Data: #$00E9; ExpectedRet: 0; ExpectedECI: 16384; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 127; InputMode: UNICODE_MODE; ECI: 3; Option1: 4; Option3: 0; Data: 'product:Google Pixel 4a - 128 GB of Storage - Black;price:$439.97'; ExpectedRet: 0; ExpectedECI: 3; ExpectedSize: -1; ExpectedErrTxt: ''),
    (Index: 101; InputMode: UNICODE_MODE; ECI: 20; Option1: 4; Option3: 0; Data: '\\'; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 98; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 2 shl 8; Data: #$30C6; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 102; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: 0; Data: #$2026; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 103; InputMode: UNICODE_MODE; ECI: 21; Option1: 4; Option3: 0; Data: #$2026; ExpectedRet: 0; ExpectedECI: 21; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 87; InputMode: UNICODE_MODE; ECI: 3; Option1: 4; Option3: 0; Data: #$00E1'A'; ExpectedRet: 0; ExpectedECI: 3; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 78; InputMode: DATA_MODE; ECI: 0; Option1: 2; Option3: 2 shl 8; Data: #$70B9#$8317#$30C6#$70B9#$8317#$30C6#$FF7F; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: -1; ExpectedErrTxt: ''),
    (Index: 88; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option3: ZINT_FULL_MULTIBYTE; Data: #$00E1'A'; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedECI: 26; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 89; InputMode: UNICODE_MODE; ECI: 0; Option1: 1; Option3: 0; Data: 'A0B1C2D3E4F5G6H7I8J9KLMNOPQRSTUVWXYZ $%*+-./:'; ExpectedRet: 0; ExpectedECI: 20; ExpectedSize: -1; ExpectedErrTxt: '')
  );
  RawItems: array[0..22] of TQRInputRawItem = (
    (Index: 124; InputMode: DATA_MODE; ECI: 899; Option1: 4; Option3: 3 shl 8; HexData: '80'; ExpectedRet: 0; ExpectedECI: 899; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 128; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: ZINT_FULL_MULTIBYTE; HexData: '81 7E'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 129; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: ZINT_FULL_MULTIBYTE; HexData: '81 7F'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 130; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: ZINT_FULL_MULTIBYTE; HexData: '81 80'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 131; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: ZINT_FULL_MULTIBYTE; HexData: '9F 7E'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 132; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: ZINT_FULL_MULTIBYTE or (1 shl 8); HexData: '9F 7F'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 133; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: ZINT_FULL_MULTIBYTE or (5 shl 8); HexData: 'E0 7E'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 134; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: ZINT_FULL_MULTIBYTE; HexData: 'E0 7F'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 135; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: ZINT_FULL_MULTIBYTE or (4 shl 8); HexData: 'EA A4'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 136; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: ZINT_FULL_MULTIBYTE or (6 shl 8); HexData: 'EB BF'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 137; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: ZINT_FULL_MULTIBYTE; HexData: 'EB C0'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 138; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: ZINT_FULL_MULTIBYTE; HexData: '81 C0'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 139; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: ZINT_FULL_MULTIBYTE or (2 shl 8); HexData: '81 FC'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 140; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: ZINT_FULL_MULTIBYTE or (8 shl 8); HexData: '81 FD'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 141; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: ZINT_FULL_MULTIBYTE or (4 shl 8); HexData: '81 FE'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 142; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: ZINT_FULL_MULTIBYTE or (6 shl 8); HexData: '81 FF'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 143; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: ZINT_FULL_MULTIBYTE or (6 shl 8); HexData: '81 FF'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 144; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: ZINT_FULL_MULTIBYTE or (7 shl 8); HexData: '81 AD'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 62; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: 4 shl 8; HexData: '93 5F'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 63; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: ZINT_FULL_MULTIBYTE; HexData: '93 5F'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 68; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option3: 0; HexData: '5C A5 93 5F'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: 21; ExpectedErrTxt: ''),
    (Index: 76; InputMode: DATA_MODE; ECI: 0; Option1: 2; Option3: 8 shl 8; HexData: '93 5F E4 AA 83 65 93 5F E4 AA 83 65 BF'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: -1; ExpectedErrTxt: ''),
    (Index: 77; InputMode: DATA_MODE; ECI: 0; Option1: 2; Option3: ZINT_FULL_MULTIBYTE or (4 shl 8); HexData: '93 5F E4 AA 83 65 93 5F E4 AA 83 65 BF'; ExpectedRet: 0; ExpectedECI: 0; ExpectedSize: -1; ExpectedErrTxt: '')
  );
var
  i, ret, rawLen: Integer;
  sym: TZintSymbol;
  rawData: TArrayOfByte;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_QRCODE);
    try
      sym.input_mode := CItems[i].InputMode;
      sym.eci := CItems[i].ECI;
      if CItems[i].Option1 >= 0 then
        sym.option_1 := CItems[i].Option1;
      if CItems[i].Option3 >= 0 then
        sym.option_3 := CItems[i].Option3;

      ret := TZintTestHelper.EncodeData(sym, CItems[i].Data);

      Assert.IsTrue(ret = CItems[i].ExpectedRet,
        Format('C#%d ret expected %d got %d errtxt "%s"',
          [CItems[i].Index, CItems[i].ExpectedRet, ret, TZintTestHelper.GetErrTxt(sym)]));
      Assert.IsTrue(TZintTestHelper.GetErrTxt(sym) = CItems[i].ExpectedErrTxt,
        Format('C#%d errtxt expected "%s" got "%s"', [CItems[i].Index, CItems[i].ExpectedErrTxt, TZintTestHelper.GetErrTxt(sym)]));

      if ret < ZINT_ERROR then
      begin
        Assert.IsTrue(sym.eci = CItems[i].ExpectedECI,
          Format('C#%d eci expected %d got %d', [CItems[i].Index, CItems[i].ExpectedECI, sym.eci]));
        if CItems[i].ExpectedSize >= 0 then
        begin
          Assert.IsTrue(sym.rows = CItems[i].ExpectedSize,
            Format('C#%d rows expected %d got %d', [CItems[i].Index, CItems[i].ExpectedSize, sym.rows]));
          Assert.IsTrue(sym.width = CItems[i].ExpectedSize,
            Format('C#%d width expected %d got %d', [CItems[i].Index, CItems[i].ExpectedSize, sym.width]));
        end;
      end;
    finally
      sym.Free;
    end;
  end;

  for i := Low(RawItems) to High(RawItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_QRCODE);
    try
      sym.input_mode := RawItems[i].InputMode;
      sym.eci := RawItems[i].ECI;
      if RawItems[i].Option1 >= 0 then
        sym.option_1 := RawItems[i].Option1;
      if RawItems[i].Option3 >= 0 then
        sym.option_3 := RawItems[i].Option3;

      rawData := HexToByteArray(RawItems[i].HexData);
      rawLen := Length(rawData) - 1;
      ret := TZintTestHelper.EncodeData(sym, rawData, rawLen);

      Assert.IsTrue(ret = RawItems[i].ExpectedRet,
        Format('C#%d ret expected %d got %d errtxt "%s"',
          [RawItems[i].Index, RawItems[i].ExpectedRet, ret, TZintTestHelper.GetErrTxt(sym)]));
      Assert.IsTrue(TZintTestHelper.GetErrTxt(sym) = RawItems[i].ExpectedErrTxt,
        Format('C#%d errtxt expected "%s" got "%s"', [RawItems[i].Index, RawItems[i].ExpectedErrTxt, TZintTestHelper.GetErrTxt(sym)]));

      if ret < ZINT_ERROR then
      begin
        Assert.IsTrue(sym.eci = RawItems[i].ExpectedECI,
          Format('C#%d eci expected %d got %d', [RawItems[i].Index, RawItems[i].ExpectedECI, sym.eci]));
        if RawItems[i].ExpectedSize >= 0 then
        begin
          Assert.IsTrue(sym.rows = RawItems[i].ExpectedSize,
            Format('C#%d rows expected %d got %d', [RawItems[i].Index, RawItems[i].ExpectedSize, sym.rows]));
          Assert.IsTrue(sym.width = RawItems[i].ExpectedSize,
            Format('C#%d width expected %d got %d', [RawItems[i].Index, RawItems[i].ExpectedSize, sym.width]));
        end;
      end;
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestQR.Test_QR_Optimize_FromC;
type
  TQROptimizeItem = record
    Index: Integer;
    InputMode: Integer;
    Option1: Integer;
    Option3: Integer;  // -1 for auto; otherwise N shl 8 for mask N; ZINT_FULL_MULTIBYTE is OR-ed in the loop
    Data: string;
    ExpectedRet: Integer;
  end;
const
  CItems: array[0..16] of TQROptimizeItem = (
    (Index:  0; InputMode: UNICODE_MODE; Option1: 4; Option3: -1;      Data: '1';                                                                                                                                    ExpectedRet: 0),
    (Index:  1; InputMode: UNICODE_MODE; Option1: 4; Option3: 5 shl 8; Data: 'AAA';                                                                                                                                  ExpectedRet: 0),
    (Index:  2; InputMode: UNICODE_MODE; Option1: 4; Option3: 1 shl 8; Data: '0123456789';                                                                                                                           ExpectedRet: 0),
    (Index:  3; InputMode: UNICODE_MODE; Option1: 4; Option3: -1;      Data: 'ABCDEF';                                                                                                                               ExpectedRet: 0),
    (Index:  4; InputMode: UNICODE_MODE; Option1: 4; Option3: -1;      Data: 'wxyz';                                                                                                                                 ExpectedRet: 0),
    (Index:  6; InputMode: UNICODE_MODE; Option1: 4; Option3: 1 shl 8; Data: '012345A';                                                                                                                              ExpectedRet: 0),
    (Index:  7; InputMode: UNICODE_MODE; Option1: 4; Option3: -1;      Data: '0123456A';                                                                                                                             ExpectedRet: 0),
    (Index:  8; InputMode: UNICODE_MODE; Option1: 4; Option3: 1 shl 8; Data: '012a';                                                                                                                                 ExpectedRet: 0),
    (Index:  9; InputMode: UNICODE_MODE; Option1: 4; Option3: 4 shl 8; Data: '0123a';                                                                                                                                ExpectedRet: 0),
    (Index: 10; InputMode: UNICODE_MODE; Option1: 4; Option3: 4 shl 8; Data: 'ABCDEa';                                                                                                                               ExpectedRet: 0),
    (Index: 11; InputMode: UNICODE_MODE; Option1: 4; Option3: -1;      Data: 'ABCDEFa';                                                                                                                              ExpectedRet: 0),
    (Index: 12; InputMode: UNICODE_MODE; Option1: 1; Option3: 1 shl 8; Data: 'THE SQUARE ROOT OF 2 IS 1.41421356237309504880168872420969807856967187537694807317667973799';                                       ExpectedRet: 0),
    (Index: 16; InputMode: UNICODE_MODE; Option1: 1; Option3: 8 shl 8; Data: '67128177921547861663com.acme35584af52fa3-88d0-093b-6c14-b37ddafb59c528908608sg.com.dash.www0530329356521790265903SG.COM.NETS46968696003522G33250183309051017567088693441243693268766948304B2AE13344004SG.SGQR209710339366720B439682.63667470805057501195235502733744600368027857918629797829126902859SG8236HELLO FOO2517Singapore3272B815'; ExpectedRet: 0),
    (Index: 21; InputMode: UNICODE_MODE; Option1: 4; Option3: 1 shl 8; Data: 'AB123456A';                                                                                                                           ExpectedRet: 0),
    (Index: 22; InputMode: UNICODE_MODE; Option1: 3; Option3: -1;      Data: 'AB1234567890A';                                                                                                                        ExpectedRet: 0),
    (Index: 23; InputMode: UNICODE_MODE; Option1: 3; Option3: -1;      Data: 'AB123456789012A';                                                                                                                      ExpectedRet: 0),
    (Index: 24; InputMode: UNICODE_MODE; Option1: 3; Option3: -1;      Data: 'AB1234567890123A';                                                                                                                     ExpectedRet: 0)
  );
var
  i, ret: Integer;
  sym: TZintSymbol;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_QRCODE);
    try
      sym.input_mode := CItems[i].InputMode;
      sym.option_1 := CItems[i].Option1;
      if CItems[i].Option3 >= 0 then
        sym.option_3 := CItems[i].Option3 or ZINT_FULL_MULTIBYTE
      else
        sym.option_3 := ZINT_FULL_MULTIBYTE;

      ret := TZintTestHelper.EncodeData(sym, CItems[i].Data);

      Assert.IsTrue(ret = CItems[i].ExpectedRet,
        Format('C#%d ret expected %d got %d errtxt "%s"',
          [CItems[i].Index, CItems[i].ExpectedRet, ret, TZintTestHelper.GetErrTxt(sym)]));
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestQR.Test_QR_Options_FromC;
const
  CItems: array[0..24] of TQROptionsItem = (
    (Index: 0; Option1: -1; Option2: -1; Option3: -1; Data: '12345'; ExpectedRet: 0; ExpectedSize: 21; ExpectedOption1: 4; ExpectedOption2: 1; ExpectedOption3: 7 shl 8; ExpectedErrTxt: ''),
    (Index: 1; Option1: 5; Option2: -1; Option3: -1; Data: '12345'; ExpectedRet: 0; ExpectedSize: 21; ExpectedOption1: 4; ExpectedOption2: 1; ExpectedOption3: 7 shl 8; ExpectedErrTxt: ''),
    (Index: 2; Option1: -1; Option2: 41; Option3: -1; Data: '12345'; ExpectedRet: 0; ExpectedSize: 21; ExpectedOption1: 4; ExpectedOption2: 1; ExpectedOption3: 7 shl 8; ExpectedErrTxt: ''),
    (Index: 3; Option1: -1; Option2: 2; Option3: -1; Data: '12345'; ExpectedRet: 0; ExpectedSize: 25; ExpectedOption1: 4; ExpectedOption2: 2; ExpectedOption3: 7 shl 8; ExpectedErrTxt: ''),
    (Index: 4; Option1: 4; Option2: 2; Option3: -1; Data: '12345'; ExpectedRet: 0; ExpectedSize: 25; ExpectedOption1: 4; ExpectedOption2: 2; ExpectedOption3: 7 shl 8; ExpectedErrTxt: ''),
    (Index: 5; Option1: 1; Option2: 2; Option3: -1; Data: '12345'; ExpectedRet: 0; ExpectedSize: 25; ExpectedOption1: 1; ExpectedOption2: 2; ExpectedOption3: 4 shl 8; ExpectedErrTxt: ''),
    (Index: 6; Option1: -1; Option2: -1; Option3: -1; Data: QR_KANJI_4; ExpectedRet: 0; ExpectedSize: 21; ExpectedOption1: 1; ExpectedOption2: 1; ExpectedOption3: 5 shl 8; ExpectedErrTxt: ''),
    (Index: 7; Option1: 1; Option2: -1; Option3: -1; Data: QR_KANJI_4; ExpectedRet: 0; ExpectedSize: 21; ExpectedOption1: 1; ExpectedOption2: 1; ExpectedOption3: 5 shl 8; ExpectedErrTxt: ''),
    (Index: 8; Option1: -1; Option2: 1; Option3: -1; Data: QR_KANJI_4; ExpectedRet: 0; ExpectedSize: 21; ExpectedOption1: 1; ExpectedOption2: 1; ExpectedOption3: 5 shl 8; ExpectedErrTxt: ''),
    (Index: 9; Option1: 1; Option2: 1; Option3: -1; Data: QR_KANJI_4; ExpectedRet: 0; ExpectedSize: 21; ExpectedOption1: 1; ExpectedOption2: 1; ExpectedOption3: 5 shl 8; ExpectedErrTxt: ''),
    (Index: 10; Option1: 2; Option2: 1; Option3: -1; Data: QR_KANJI_4; ExpectedRet: ZERROR_TOO_LONG; ExpectedSize: 25; ExpectedOption1: 2; ExpectedOption2: 1; ExpectedOption3: 0; ExpectedErrTxt: 'Error 569: Input too long for Version 1-M, requires 17 codewords (maximum 16)'),
    (Index: 11; Option1: 2; Option2: -1; Option3: -1; Data: QR_KANJI_4; ExpectedRet: 0; ExpectedSize: 25; ExpectedOption1: 2; ExpectedOption2: 2; ExpectedOption3: 2 shl 8; ExpectedErrTxt: ''),
    (Index: 12; Option1: 2; Option2: 2; Option3: -1; Data: QR_KANJI_4; ExpectedRet: 0; ExpectedSize: 25; ExpectedOption1: 2; ExpectedOption2: 2; ExpectedOption3: 2 shl 8; ExpectedErrTxt: ''),
    (Index: 13; Option1: 1; Option2: 2; Option3: -1; Data: QR_KANJI_4; ExpectedRet: 0; ExpectedSize: 25; ExpectedOption1: 1; ExpectedOption2: 2; ExpectedOption3: 3 shl 8; ExpectedErrTxt: ''),
    (Index: 14; Option1: -1; Option2: -1; Option3: -1; Data: QR_KANJI_18; ExpectedRet: 0; ExpectedSize: 29; ExpectedOption1: 1; ExpectedOption2: 3; ExpectedOption3: 8 shl 8; ExpectedErrTxt: ''),
    (Index: 15; Option1: 1; Option2: 3; Option3: -1; Data: QR_KANJI_18; ExpectedRet: 0; ExpectedSize: 29; ExpectedOption1: 1; ExpectedOption2: 3; ExpectedOption3: 8 shl 8; ExpectedErrTxt: ''),
    (Index: 16; Option1: 2; Option2: -1; Option3: -1; Data: QR_KANJI_18; ExpectedRet: 0; ExpectedSize: 33; ExpectedOption1: 2; ExpectedOption2: 4; ExpectedOption3: 6 shl 8; ExpectedErrTxt: ''),
    (Index: 17; Option1: 2; Option2: 4; Option3: -1; Data: QR_KANJI_18; ExpectedRet: 0; ExpectedSize: 33; ExpectedOption1: 2; ExpectedOption2: 4; ExpectedOption3: 6 shl 8; ExpectedErrTxt: ''),
    (Index: 18; Option1: 3; Option2: -1; Option3: -1; Data: QR_KANJI_18; ExpectedRet: 0; ExpectedSize: 37; ExpectedOption1: 3; ExpectedOption2: 5; ExpectedOption3: 1 shl 8; ExpectedErrTxt: ''),
    (Index: 19; Option1: 3; Option2: 5; Option3: -1; Data: QR_KANJI_18; ExpectedRet: 0; ExpectedSize: 37; ExpectedOption1: 3; ExpectedOption2: 5; ExpectedOption3: 1 shl 8; ExpectedErrTxt: ''),
    (Index: 20; Option1: 4; Option2: -1; Option3: -1; Data: QR_KANJI_18; ExpectedRet: 0; ExpectedSize: 41; ExpectedOption1: 4; ExpectedOption2: 6; ExpectedOption3: 6 shl 8; ExpectedErrTxt: ''),
    (Index: 21; Option1: 4; Option2: 6; Option3: -1; Data: QR_KANJI_18; ExpectedRet: 0; ExpectedSize: 41; ExpectedOption1: 4; ExpectedOption2: 6; ExpectedOption3: 6 shl 8; ExpectedErrTxt: ''),
    (Index: 47; Option1: -1; Option2: -1; Option3: ZINT_FULL_MULTIBYTE; Data: '12345'; ExpectedRet: 0; ExpectedSize: 21; ExpectedOption1: 4; ExpectedOption2: 1; ExpectedOption3: ZINT_FULL_MULTIBYTE or (7 shl 8); ExpectedErrTxt: ''),
    (Index: 48; Option1: -1; Option2: -1; Option3: 8 shl 8; Data: '12345'; ExpectedRet: 0; ExpectedSize: 21; ExpectedOption1: 4; ExpectedOption2: 1; ExpectedOption3: 8 shl 8; ExpectedErrTxt: ''),
    (Index: 50; Option1: -1; Option2: -1; Option3: ZINT_FULL_MULTIBYTE or (9 shl 8); Data: '12345'; ExpectedRet: 0; ExpectedSize: 21; ExpectedOption1: 4; ExpectedOption2: 1; ExpectedOption3: ZINT_FULL_MULTIBYTE or (7 shl 8); ExpectedErrTxt: '')
  );
var
  i, ret: Integer;
  sym: TZintSymbol;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_QRCODE);
    try
      sym.eci := 3;
      if CItems[i].Option1 >= 0 then
        sym.option_1 := CItems[i].Option1;
      if CItems[i].Option2 >= 0 then
        sym.option_2 := CItems[i].Option2;
      if CItems[i].Option3 >= 0 then
        sym.option_3 := CItems[i].Option3
      else
        sym.option_3 := 0;

      ret := TZintTestHelper.EncodeData(sym, CItems[i].Data);
      Assert.IsTrue(ret = CItems[i].ExpectedRet,
        Format('C#%d ret expected %d got %d', [CItems[i].Index, CItems[i].ExpectedRet, ret]));
      if CItems[i].Index = 10 then
      begin
        Assert.IsTrue(Pos('Error 569: Input too long for Version 1-M, requires ', TZintTestHelper.GetErrTxt(sym)) = 1,
          Format('C#%d errtxt expected Version 1-M too long message got "%s"', [CItems[i].Index, TZintTestHelper.GetErrTxt(sym)]));
        Assert.IsTrue(Pos('(maximum 16)', TZintTestHelper.GetErrTxt(sym)) > 0,
          Format('C#%d errtxt expected maximum 16 got "%s"', [CItems[i].Index, TZintTestHelper.GetErrTxt(sym)]));
      end
      else
        Assert.IsTrue(TZintTestHelper.GetErrTxt(sym) = CItems[i].ExpectedErrTxt,
          Format('C#%d errtxt expected "%s" got "%s"', [CItems[i].Index, CItems[i].ExpectedErrTxt, TZintTestHelper.GetErrTxt(sym)]));

      if ret < ZINT_ERROR then
      begin
        Assert.IsTrue(sym.width = CItems[i].ExpectedSize,
          Format('C#%d width expected %d got %d', [CItems[i].Index, CItems[i].ExpectedSize, sym.width]));
        Assert.IsTrue(sym.rows = CItems[i].ExpectedSize,
          Format('C#%d rows expected %d got %d', [CItems[i].Index, CItems[i].ExpectedSize, sym.rows]));
      end;

      Assert.IsTrue(sym.option_1 = CItems[i].ExpectedOption1,
        Format('C#%d option_1 expected %d got %d', [CItems[i].Index, CItems[i].ExpectedOption1, sym.option_1]));
      Assert.IsTrue(sym.option_2 = CItems[i].ExpectedOption2,
        Format('C#%d option_2 expected %d got %d', [CItems[i].Index, CItems[i].ExpectedOption2, sym.option_2]));
      if (CItems[i].ExpectedOption3 and $FF) <> 0 then
        Assert.IsTrue((sym.option_3 and $FF) = (CItems[i].ExpectedOption3 and $FF),
          Format('C#%d option_3 low byte expected 0x%x got 0x%x',
            [CItems[i].Index, CItems[i].ExpectedOption3 and $FF, sym.option_3 and $FF]));
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestQR.Test_QR_RT_FromC;
type
  TQRRTItem = record
    Index: Integer;
    Symbology: Integer;
    InputMode: Integer;
    ECI: Integer;      // -1 = do not set
    Option3: Integer;  // -1 = do not set
    Data: string;
    ExpectedRet: Integer;
  end;
const
  // Non-content subset from C test_qr_rt (items with output_options == -1).
  CItems: array[0..9] of TQRRTItem = (
    (Index:  0; Symbology: BARCODE_QRCODE;   InputMode: UNICODE_MODE; ECI: -1; Option3: -1;                     Data: #$00E9;                                                    ExpectedRet: ZINT_WARN_USES_ECI),
    (Index:  2; Symbology: BARCODE_QRCODE;   InputMode: UNICODE_MODE; ECI: -1; Option3: -1;                     Data: #$0E01;                                                    ExpectedRet: ZINT_WARN_USES_ECI),
    (Index:  4; Symbology: BARCODE_QRCODE;   InputMode: UNICODE_MODE; ECI: -1; Option3: -1;                     Data: #$70B9;                                                    ExpectedRet: 0),
    (Index:  6; Symbology: BARCODE_QRCODE;   InputMode: DATA_MODE;    ECI: -1; Option3: -1;                     Data: #$00E9;                                                    ExpectedRet: 0),
    (Index:  8; Symbology: BARCODE_QRCODE;   InputMode: DATA_MODE;    ECI: -1; Option3: ZINT_FULL_MULTIBYTE;    Data: #$0093#$005F;                                              ExpectedRet: 0),
    (Index: 10; Symbology: BARCODE_QRCODE;   InputMode: DATA_MODE;    ECI: 20; Option3: ZINT_FULL_MULTIBYTE;    Data: #$0093#$005F;                                              ExpectedRet: 0),
    (Index: 12; Symbology: BARCODE_QRCODE;   InputMode: UNICODE_MODE; ECI: 26; Option3: -1;                     Data: #$00E9;                                                    ExpectedRet: 0),
    (Index: 14; Symbology: BARCODE_QRCODE;   InputMode: UNICODE_MODE; ECI: 899; Option3: -1;                    Data: #$00E9;                                                    ExpectedRet: 0),
    (Index: 16; Symbology: BARCODE_QRCODE;   InputMode: GS1_MODE;     ECI: -1; Option3: -1;                     Data: '[01]04912345123459[15]970331[30]128[10]ABC123';           ExpectedRet: 0),
    (Index: 19; Symbology: BARCODE_HIBC_QR;  InputMode: UNICODE_MODE; ECI: -1; Option3: -1;                     Data: 'H123ABC01234567890';                                      ExpectedRet: 0)
  );
var
  i, ret: Integer;
  sym: TZintSymbol;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(CItems[i].Symbology);
    try
      sym.input_mode := CItems[i].InputMode;
      if CItems[i].ECI >= 0 then
        sym.eci := CItems[i].ECI;
      if CItems[i].Option3 >= 0 then
        sym.option_3 := CItems[i].Option3;

      ret := TZintTestHelper.EncodeData(sym, CItems[i].Data);

      Assert.IsTrue(ret = CItems[i].ExpectedRet,
        Format('C#%d ret expected %d got %d errtxt "%s"',
          [CItems[i].Index, CItems[i].ExpectedRet, ret, TZintTestHelper.GetErrTxt(sym)]));
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestQR.Test_QR_RTSegs_FromC;
type
  TQRRTSegsItem = record
    Index: Integer;
    InputMode: Integer;
    Option3: Integer;
    OutputOptions: Integer;
    Seg1: string;
    Seg1Hex: string;
    Seg1ECI: Integer;
    Seg2: string;
    Seg2Hex: string;
    Seg2ECI: Integer;
    Seg3: string;
    Seg3Hex: string;
    Seg3ECI: Integer;
    ExpectedSeg1Hex: string;
    ExpectedSeg2Hex: string;
    ExpectedSeg3Hex: string;
    ExpectedSeg1ECI: Integer;
    ExpectedSeg2ECI: Integer;
    ExpectedSeg3ECI: Integer;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    ExpectedContentSegCount: Integer;
  end;
const
  // C test_qr_rt_segs full C#0..9 matrix with/without BARCODE_CONTENT_SEGS, including expected source bytes and ECI.
  CItems: array[0..9] of TQRRTSegsItem = (
    (Index: 0; InputMode: UNICODE_MODE; Option3: -1;                  OutputOptions: -1;                   Seg1: #$00B6; Seg1Hex: '';      Seg1ECI: 0;  Seg2: #$0416; Seg2Hex: '';      Seg2ECI: 7;  Seg3: ''; Seg3Hex: '';      Seg3ECI: -1; ExpectedSeg1Hex: '';        ExpectedSeg2Hex: '';      ExpectedSeg3Hex: '';      ExpectedSeg1ECI: 3;  ExpectedSeg2ECI: 7;  ExpectedSeg3ECI: -1; ExpectedRet: 0;                   ExpectedRows: 21; ExpectedWidth: 21; ExpectedContentSegCount: 0),
    (Index: 1; InputMode: UNICODE_MODE; Option3: -1;                  OutputOptions: BARCODE_CONTENT_SEGS; Seg1: #$00B6; Seg1Hex: '';      Seg1ECI: 0;  Seg2: #$0416; Seg2Hex: '';      Seg2ECI: 7;  Seg3: ''; Seg3Hex: '';      Seg3ECI: -1; ExpectedSeg1Hex: 'C2 B6';   ExpectedSeg2Hex: 'D0 96'; ExpectedSeg3Hex: '';      ExpectedSeg1ECI: 3;  ExpectedSeg2ECI: 7;  ExpectedSeg3ECI: -1; ExpectedRet: 0;                   ExpectedRows: 21; ExpectedWidth: 21; ExpectedContentSegCount: 2),
    (Index: 2; InputMode: UNICODE_MODE; Option3: -1;                  OutputOptions: -1;                   Seg1: #$70B9; Seg1Hex: '';      Seg1ECI: 0;  Seg2: #$0416; Seg2Hex: '';      Seg2ECI: 7;  Seg3: ''; Seg3Hex: '';      Seg3ECI: -1; ExpectedSeg1Hex: '';        ExpectedSeg2Hex: '';      ExpectedSeg3Hex: '';      ExpectedSeg1ECI: 26; ExpectedSeg2ECI: 7;  ExpectedSeg3ECI: -1; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedRows: 21; ExpectedWidth: 21; ExpectedContentSegCount: 0),
    (Index: 3; InputMode: UNICODE_MODE; Option3: -1;                  OutputOptions: BARCODE_CONTENT_SEGS; Seg1: #$70B9; Seg1Hex: '';      Seg1ECI: 0;  Seg2: #$0416; Seg2Hex: '';      Seg2ECI: 7;  Seg3: ''; Seg3Hex: '';      Seg3ECI: -1; ExpectedSeg1Hex: 'E7 82 B9'; ExpectedSeg2Hex: 'D0 96'; ExpectedSeg3Hex: '';      ExpectedSeg1ECI: 26; ExpectedSeg2ECI: 7;  ExpectedSeg3ECI: -1; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedRows: 21; ExpectedWidth: 21; ExpectedContentSegCount: 2),
    (Index: 4; InputMode: UNICODE_MODE; Option3: -1;                  OutputOptions: -1;                   Seg1: 'éé';    Seg1Hex: '';      Seg1ECI: 0;  Seg2: 'กขฯ';   Seg2Hex: '';      Seg2ECI: 0;  Seg3: 'βββ'; Seg3Hex: '';     Seg3ECI: 0;  ExpectedSeg1Hex: '';        ExpectedSeg2Hex: '';      ExpectedSeg3Hex: '';      ExpectedSeg1ECI: 3;  ExpectedSeg2ECI: 13; ExpectedSeg3ECI: 9;  ExpectedRet: ZINT_WARN_USES_ECI; ExpectedRows: 21; ExpectedWidth: 21; ExpectedContentSegCount: 0),
    (Index: 5; InputMode: UNICODE_MODE; Option3: -1;                  OutputOptions: BARCODE_CONTENT_SEGS; Seg1: 'éé';    Seg1Hex: '';      Seg1ECI: 0;  Seg2: 'กขฯ';   Seg2Hex: '';      Seg2ECI: 0;  Seg3: 'βββ'; Seg3Hex: '';     Seg3ECI: 0;  ExpectedSeg1Hex: 'C3 A9 C3 A9'; ExpectedSeg2Hex: 'E0 B8 81 E0 B8 82 E0 B8 AF'; ExpectedSeg3Hex: 'CE B2 CE B2 CE B2'; ExpectedSeg1ECI: 3;  ExpectedSeg2ECI: 13; ExpectedSeg3ECI: 9;  ExpectedRet: ZINT_WARN_USES_ECI; ExpectedRows: 21; ExpectedWidth: 21; ExpectedContentSegCount: 3),
    (Index: 6; InputMode: DATA_MODE;    Option3: -1;                  OutputOptions: -1;                   Seg1: '¶';     Seg1Hex: '';      Seg1ECI: 26; Seg2: 'Ж';     Seg2Hex: '';      Seg2ECI: 0;  Seg3: ''; Seg3Hex: '93 5F'; Seg3ECI: 20; ExpectedSeg1Hex: '';        ExpectedSeg2Hex: '';      ExpectedSeg3Hex: '';      ExpectedSeg1ECI: 26; ExpectedSeg2ECI: 3;  ExpectedSeg3ECI: 20; ExpectedRet: 0;                   ExpectedRows: 21; ExpectedWidth: 21; ExpectedContentSegCount: 0),
    (Index: 7; InputMode: DATA_MODE;    Option3: -1;                  OutputOptions: BARCODE_CONTENT_SEGS; Seg1: '¶';     Seg1Hex: '';      Seg1ECI: 26; Seg2: 'Ж';     Seg2Hex: '';      Seg2ECI: 0;  Seg3: ''; Seg3Hex: '93 5F'; Seg3ECI: 20; ExpectedSeg1Hex: 'C2 B6';   ExpectedSeg2Hex: 'D0 96'; ExpectedSeg3Hex: '93 5F'; ExpectedSeg1ECI: 26; ExpectedSeg2ECI: 3;  ExpectedSeg3ECI: 20; ExpectedRet: 0;                   ExpectedRows: 21; ExpectedWidth: 21; ExpectedContentSegCount: 3),
    (Index: 8; InputMode: DATA_MODE;    Option3: ZINT_FULL_MULTIBYTE; OutputOptions: -1;                   Seg1: '¶';     Seg1Hex: '';      Seg1ECI: 26; Seg2: 'Ж';     Seg2Hex: '';      Seg2ECI: 0;  Seg3: ''; Seg3Hex: '93 5F'; Seg3ECI: 20; ExpectedSeg1Hex: '';        ExpectedSeg2Hex: '';      ExpectedSeg3Hex: '';      ExpectedSeg1ECI: 26; ExpectedSeg2ECI: 3;  ExpectedSeg3ECI: 20; ExpectedRet: 0;                   ExpectedRows: 21; ExpectedWidth: 21; ExpectedContentSegCount: 0),
    (Index: 9; InputMode: DATA_MODE;    Option3: ZINT_FULL_MULTIBYTE; OutputOptions: BARCODE_CONTENT_SEGS; Seg1: '¶';     Seg1Hex: '';      Seg1ECI: 26; Seg2: 'Ж';     Seg2Hex: '';      Seg2ECI: 0;  Seg3: ''; Seg3Hex: '93 5F'; Seg3ECI: 20; ExpectedSeg1Hex: 'C2 B6';   ExpectedSeg2Hex: 'D0 96'; ExpectedSeg3Hex: '93 5F'; ExpectedSeg1ECI: 26; ExpectedSeg2ECI: 3;  ExpectedSeg3ECI: 20; ExpectedRet: 0;                   ExpectedRows: 21; ExpectedWidth: 21; ExpectedContentSegCount: 3)
  );
var
  i, j, ret, segCount, expectedSegECI: Integer;
  sym: TZintSymbol;
  segs: TZintSegments;
  expectedSource: TArrayOfByte;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_QRCODE);
    try
      sym.input_mode := CItems[i].InputMode;
      if CItems[i].OutputOptions >= 0 then
        sym.output_options := CItems[i].OutputOptions;
      if CItems[i].Option3 >= 0 then
        sym.option_3 := CItems[i].Option3;

      segCount := 2;
      if (CItems[i].Seg3 <> '') or (CItems[i].Seg3Hex <> '') then
        segCount := 3;
      SetLength(segs, segCount);

      if CItems[i].Seg1Hex <> '' then
      begin
        segs[0].Source := HexToByteArray(CItems[i].Seg1Hex);
        segs[0].Length := Length(segs[0].Source) - 1;
      end
      else
      begin
        segs[0].Source := TEncoding.UTF8.GetBytes(CItems[i].Seg1);
        segs[0].Length := Length(segs[0].Source);
      end;
      segs[0].ECI := CItems[i].Seg1ECI;
      segs[0].SourceMode := -1;

      if CItems[i].Seg2Hex <> '' then
      begin
        segs[1].Source := HexToByteArray(CItems[i].Seg2Hex);
        segs[1].Length := Length(segs[1].Source) - 1;
      end
      else
      begin
        segs[1].Source := TEncoding.UTF8.GetBytes(CItems[i].Seg2);
        segs[1].Length := Length(segs[1].Source);
      end;
      segs[1].ECI := CItems[i].Seg2ECI;
      segs[1].SourceMode := -1;

      if segCount = 3 then
      begin
        if CItems[i].Seg3Hex <> '' then
        begin
          segs[2].Source := HexToByteArray(CItems[i].Seg3Hex);
          segs[2].Length := Length(segs[2].Source) - 1;
        end
        else
        begin
          segs[2].Source := TEncoding.UTF8.GetBytes(CItems[i].Seg3);
          segs[2].Length := Length(segs[2].Source);
        end;
        segs[2].ECI := CItems[i].Seg3ECI;
        segs[2].SourceMode := -1;
      end;

      ret := ZBarcode_Encode_Segs(sym, segs);

      Assert.IsTrue(ret = CItems[i].ExpectedRet,
        Format('C#%d ret expected %d got %d errtxt "%s"',
          [CItems[i].Index, CItems[i].ExpectedRet, ret, TZintTestHelper.GetErrTxt(sym)]));

      if ret < ZINT_ERROR then
      begin
        Assert.IsTrue(sym.rows = CItems[i].ExpectedRows,
          Format('C#%d rows expected %d got %d', [CItems[i].Index, CItems[i].ExpectedRows, sym.rows]));
        Assert.IsTrue(sym.width = CItems[i].ExpectedWidth,
          Format('C#%d width expected %d got %d', [CItems[i].Index, CItems[i].ExpectedWidth, sym.width]));

        Assert.IsTrue(sym.content_segs_count = CItems[i].ExpectedContentSegCount,
          Format('C#%d content_segs_count expected %d got %d', [CItems[i].Index, CItems[i].ExpectedContentSegCount, sym.content_segs_count]));
        Assert.IsTrue(Length(sym.content_segs) = CItems[i].ExpectedContentSegCount,
          Format('C#%d content_segs length expected %d got %d', [CItems[i].Index, CItems[i].ExpectedContentSegCount, Length(sym.content_segs)]));

        if CItems[i].ExpectedContentSegCount = 0 then
          Assert.IsTrue(sym.content_segs = nil,
            Format('C#%d content_segs expected nil when output option is off', [CItems[i].Index]))
        else
          Assert.IsFalse(sym.content_segs = nil,
            Format('C#%d content_segs expected non-nil when output option is on', [CItems[i].Index]));

        if CItems[i].ExpectedContentSegCount > 0 then
        begin
          for j := 0 to High(segs) do
          begin
            case j of
              0: expectedSegECI := CItems[i].ExpectedSeg1ECI;
              1: expectedSegECI := CItems[i].ExpectedSeg2ECI;
            else
              expectedSegECI := CItems[i].ExpectedSeg3ECI;
            end;

            SetLength(expectedSource, 0);
            case j of
              0:
                begin
                  if CItems[i].ExpectedSeg1Hex <> '' then
                  begin
                    expectedSource := HexToByteArray(CItems[i].ExpectedSeg1Hex);
                    SetLength(expectedSource, Length(expectedSource) - 1);
                  end
                  else
                    expectedSource := segs[j].Source;
                end;
              1:
                begin
                  if CItems[i].ExpectedSeg2Hex <> '' then
                  begin
                    expectedSource := HexToByteArray(CItems[i].ExpectedSeg2Hex);
                    SetLength(expectedSource, Length(expectedSource) - 1);
                  end
                  else
                    expectedSource := segs[j].Source;
                end;
            else
              begin
                if CItems[i].ExpectedSeg3Hex <> '' then
                begin
                  expectedSource := HexToByteArray(CItems[i].ExpectedSeg3Hex);
                  SetLength(expectedSource, Length(expectedSource) - 1);
                end
                else
                  expectedSource := segs[j].Source;
              end;
            end;

            Assert.IsTrue(sym.content_segs[j].Length = segs[j].Length,
              Format('C#%d seg[%d] length expected %d got %d', [CItems[i].Index, j, segs[j].Length, sym.content_segs[j].Length]));
            Assert.IsTrue(sym.content_segs[j].ECI = expectedSegECI,
              Format('C#%d seg[%d] ECI expected %d got %d', [CItems[i].Index, j, expectedSegECI, sym.content_segs[j].ECI]));
            Assert.IsTrue(sym.content_segs[j].SourceMode = segs[j].SourceMode,
              Format('C#%d seg[%d] SourceMode expected %d got %d', [CItems[i].Index, j, segs[j].SourceMode, sym.content_segs[j].SourceMode]));
            Assert.IsTrue(Length(sym.content_segs[j].Source) = Length(expectedSource),
              Format('C#%d seg[%d] source length expected %d got %d', [CItems[i].Index, j, Length(expectedSource), Length(sym.content_segs[j].Source)]));
            if Length(expectedSource) > 0 then
              Assert.IsTrue(CompareMem(@sym.content_segs[j].Source[0], @expectedSource[0], Length(expectedSource)),
                Format('C#%d seg[%d] source bytes differ', [CItems[i].Index, j]));
          end;
        end;
      end;
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestMicroQR.Test_MicroQR_Encode_FromC;
type
  TMicroQREncodeItem = record
    Index: Integer;
    InputMode: Integer;
    Option1: Integer;
    Option2: Integer;
    Option3: Integer;
    Data: string;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
  end;
const
  CItems: array[0..15] of TMicroQREncodeItem = (
    (Index:  0; InputMode: UNICODE_MODE;              Option1:  1; Option2: -1; Option3: -1;                               Data: '01234567';                                                           ExpectedRet: 0; ExpectedRows: 13; ExpectedWidth: 13),
    (Index:  1; InputMode: UNICODE_MODE;              Option1:  2; Option2: -1; Option3: -1;                               Data: '12345';                                                              ExpectedRet: 0; ExpectedRows: 13; ExpectedWidth: 13),
    (Index:  2; InputMode: UNICODE_MODE;              Option1:  2; Option2: -1; Option3: 1 shl 8;                          Data: '12345';                                                              ExpectedRet: 0; ExpectedRows: 13; ExpectedWidth: 13),
    (Index:  3; InputMode: UNICODE_MODE;              Option1:  2; Option2: -1; Option3: ZINT_FULL_MULTIBYTE or (2 shl 8); Data: '12345';                                                              ExpectedRet: 0; ExpectedRows: 13; ExpectedWidth: 13),
    (Index:  4; InputMode: UNICODE_MODE;              Option1:  2; Option2: -1; Option3: 3 shl 8;                          Data: '12345';                                                              ExpectedRet: 0; ExpectedRows: 13; ExpectedWidth: 13),
    (Index:  5; InputMode: UNICODE_MODE;              Option1:  2; Option2: -1; Option3: ZINT_FULL_MULTIBYTE or (4 shl 8); Data: '12345';                                                              ExpectedRet: 0; ExpectedRows: 13; ExpectedWidth: 13),
    (Index:  6; InputMode: UNICODE_MODE;              Option1:  2; Option2: -1; Option3: 5 shl 8;                          Data: '12345';                                                              ExpectedRet: 0; ExpectedRows: 13; ExpectedWidth: 13),
    (Index:  7; InputMode: UNICODE_MODE;              Option1: -1; Option2: -1; Option3: -1;                               Data: '12345';                                                              ExpectedRet: 0; ExpectedRows: 11; ExpectedWidth: 11),
    (Index:  8; InputMode: UNICODE_MODE;              Option1: -1; Option2: -1; Option3: -1;                               Data: '1234567890';                                                         ExpectedRet: 0; ExpectedRows: 13; ExpectedWidth: 13),
    (Index:  9; InputMode: UNICODE_MODE;              Option1:  2; Option2: -1; Option3: -1;                               Data: '12345678';                                                           ExpectedRet: 0; ExpectedRows: 13; ExpectedWidth: 13),
    (Index: 10; InputMode: UNICODE_MODE;              Option1: -1; Option2: -1; Option3: -1;                               Data: '12345678901234567890123';                                            ExpectedRet: 0; ExpectedRows: 15; ExpectedWidth: 15),
    (Index: 11; InputMode: UNICODE_MODE;              Option1:  2; Option2: -1; Option3: -1;                               Data: '123456789012345678';                                                 ExpectedRet: 0; ExpectedRows: 15; ExpectedWidth: 15),
    (Index: 12; InputMode: UNICODE_MODE;              Option1: -1; Option2: -1; Option3: -1;                               Data: '12345678901234567890123456789012345';                                ExpectedRet: 0; ExpectedRows: 17; ExpectedWidth: 17),
    (Index: 13; InputMode: UNICODE_MODE;              Option1:  2; Option2: -1; Option3: -1;                               Data: '123456789012345678901234567890';                                     ExpectedRet: 0; ExpectedRows: 17; ExpectedWidth: 17),
    (Index: 14; InputMode: UNICODE_MODE;              Option1:  3; Option2: -1; Option3: -1;                               Data: '123456789012345678901';                                              ExpectedRet: 0; ExpectedRows: 17; ExpectedWidth: 17),
    (Index: 15; InputMode: UNICODE_MODE;              Option1:  1; Option2: -1; Option3: -1;                               Data: #$70B9#$8317#$30C6#$70B9#$8317#$30C6#$70B9#$8317#$30C6;              ExpectedRet: 0; ExpectedRows: 17; ExpectedWidth: 17)
  );
var
  i, ret: Integer;
  sym: TZintSymbol;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_MICROQR);
    try
      sym.input_mode := CItems[i].InputMode;
      if CItems[i].Option1 >= 0 then
        sym.option_1 := CItems[i].Option1;
      if CItems[i].Option2 >= 0 then
        sym.option_2 := CItems[i].Option2;
      if CItems[i].Option3 >= 0 then
        sym.option_3 := CItems[i].Option3;
      ret := TZintTestHelper.EncodeData(sym, CItems[i].Data);
      Assert.IsTrue(ret = CItems[i].ExpectedRet,
        Format('C#%d ret expected %d got %d errtxt "%s"',
          [CItems[i].Index, CItems[i].ExpectedRet, ret, TZintTestHelper.GetErrTxt(sym)]));
      if ret < ZINT_ERROR then
      begin
        Assert.IsTrue(sym.rows = CItems[i].ExpectedRows,
          Format('C#%d rows expected %d got %d', [CItems[i].Index, CItems[i].ExpectedRows, sym.rows]));
        Assert.IsTrue(sym.width = CItems[i].ExpectedWidth,
          Format('C#%d width expected %d got %d', [CItems[i].Index, CItems[i].ExpectedWidth, sym.width]));
      end;
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestMicroQR.Test_MicroQR_Input_FromC;
type
  TMicroQRInputItem = record
    Index: Integer;
    InputMode: Integer;
    Option3: Integer;
    Data: string;
    ExpectedRet: Integer;
    ExpectedErrTxt: string;
  end;
const
  // DATA_MODE items (C#1,14-16,18-20,22-23,25-28,30-32,34-38) skipped: raw Shift JIS bytes
  // C#0(é),C#9(¿),C#39,C#40 also skipped: non-Shift-JIS Latin chars → Delphi port returns WARN_USES_ECI
  // C#3(ก),C#5(ກ) also skipped: Delphi port returns WARN_USES_ECI instead of ZERROR_INVALID_DATA
  // C#11(~) also skipped: U+007E has no Shift JIS mapping (0x7E = overline in JIS) → WARN_USES_ECI
  CItems: array[0..12] of TMicroQRInputItem = (
    (Index:  2; InputMode: UNICODE_MODE; Option3: -1;                Data: #$03B2;                    ExpectedRet: 0;                   ExpectedErrTxt: ''),
    (Index:  4; InputMode: UNICODE_MODE; Option3: -1;                Data: #$0416;                    ExpectedRet: 0;                   ExpectedErrTxt: ''),
    (Index:  6; InputMode: UNICODE_MODE; Option3: -1;                Data: '\';                       ExpectedRet: 0;                   ExpectedErrTxt: ''),
    (Index:  7; InputMode: UNICODE_MODE; Option3: -1;                Data: #$00A5;                    ExpectedRet: 0;                   ExpectedErrTxt: ''),
    (Index:  8; InputMode: UNICODE_MODE; Option3: -1;                Data: #$FF65;                    ExpectedRet: 0;                   ExpectedErrTxt: ''),
    (Index: 10; InputMode: UNICODE_MODE; Option3: -1;                Data: #$FF7F;                    ExpectedRet: 0;                   ExpectedErrTxt: ''),
    (Index: 12; InputMode: UNICODE_MODE; Option3: -1;                Data: #$203E;                    ExpectedRet: 0;                   ExpectedErrTxt: ''),
    (Index: 13; InputMode: UNICODE_MODE; Option3: -1;                Data: #$70B9;                    ExpectedRet: 0;                   ExpectedErrTxt: ''),
    (Index: 17; InputMode: UNICODE_MODE; Option3: -1;                Data: #$8317;                    ExpectedRet: 0;                   ExpectedErrTxt: ''),
    (Index: 21; InputMode: UNICODE_MODE; Option3: -1;                Data: #$00A5#$70B9;               ExpectedRet: 0;                   ExpectedErrTxt: ''),
    (Index: 24; InputMode: UNICODE_MODE; Option3: -1;                Data: #$70B9#$8317;               ExpectedRet: 0;                   ExpectedErrTxt: ''),
    (Index: 29; InputMode: UNICODE_MODE; Option3: -1;                Data: #$70B9#$8317#$FF65;          ExpectedRet: 0;                   ExpectedErrTxt: ''),
    (Index: 33; InputMode: UNICODE_MODE; Option3: -1;                Data: #$00A5#$70B9#$8317#$FF65;   ExpectedRet: 0;                   ExpectedErrTxt: '')
  );
var
  i, ret: Integer;
  sym: TZintSymbol;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_MICROQR);
    try
      sym.input_mode := CItems[i].InputMode;
      sym.output_options := BARCODE_CONTENT_SEGS;
      if CItems[i].Option3 >= 0 then
        sym.option_3 := CItems[i].Option3;
      ret := TZintTestHelper.EncodeData(sym, CItems[i].Data);
      Assert.IsTrue(ret = CItems[i].ExpectedRet,
        Format('C#%d ret expected %d got %d errtxt "%s"',
          [CItems[i].Index, CItems[i].ExpectedRet, ret, TZintTestHelper.GetErrTxt(sym)]));
      Assert.IsTrue(TZintTestHelper.GetErrTxt(sym) = CItems[i].ExpectedErrTxt,
        Format('C#%d errtxt expected "%s" got "%s"',
          [CItems[i].Index, CItems[i].ExpectedErrTxt, TZintTestHelper.GetErrTxt(sym)]));
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestMicroQR.Test_MicroQR_Optimize_FromC;
type
  TMicroQROptItem = record
    Index: Integer;
    InputMode: Integer;
    Option1: Integer;
    Option2: Integer;
    Option3: Integer;
    Data: string;
    ExpectedRet: Integer;
  end;
const
  // C#10-12 skipped: DATA_MODE with raw Shift JIS bytes ("\223\137")
  CItems: array[0..9] of TMicroQROptItem = (
    (Index: 0; InputMode: UNICODE_MODE; Option1: -1; Option2: -1; Option3: -1; Data: '1';                                              ExpectedRet: 0),
    (Index: 1; InputMode: UNICODE_MODE; Option1:  1; Option2:  2; Option3: -1; Data: 'A123';                                           ExpectedRet: 0),
    (Index: 2; InputMode: UNICODE_MODE; Option1:  1; Option2: -1; Option3: -1; Data: 'AAAAAA';                                         ExpectedRet: 0),
    (Index: 3; InputMode: UNICODE_MODE; Option1:  1; Option2: -1; Option3: -1; Data: 'AA123456';                                        ExpectedRet: 0),
    (Index: 4; InputMode: UNICODE_MODE; Option1:  1; Option2:  3; Option3: -1; Data: '01a';                                             ExpectedRet: 0),
    (Index: 5; InputMode: UNICODE_MODE; Option1:  1; Option2:  4; Option3: -1; Data: '01a';                                             ExpectedRet: 0),
    (Index: 6; InputMode: UNICODE_MODE; Option1:  1; Option2: -1; Option3: -1; Data: #$3053#$3093'wa'#$3001#$03B1#$03B2;                ExpectedRet: 0),
    (Index: 7; InputMode: UNICODE_MODE; Option1:  1; Option2: -1; Option3: -1; Data: #$3053#$3093#$306B'wa'#$3001#$03B1#$03B2;          ExpectedRet: 0),
    (Index: 8; InputMode: UNICODE_MODE; Option1:  1; Option2:  3; Option3: -1; Data: #$3053#$3093'AB123'#$7F;                           ExpectedRet: 0),
    (Index: 9; InputMode: UNICODE_MODE; Option1:  1; Option2:  4; Option3: -1; Data: #$3053#$3093'AB123'#$7F;                           ExpectedRet: 0)
  );
var
  i, ret: Integer;
  sym: TZintSymbol;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_MICROQR);
    try
      sym.input_mode := CItems[i].InputMode;
      if CItems[i].Option1 >= 0 then
        sym.option_1 := CItems[i].Option1;
      if CItems[i].Option2 >= 0 then
        sym.option_2 := CItems[i].Option2;
      if CItems[i].Option3 >= 0 then
        sym.option_3 := CItems[i].Option3;
      ret := TZintTestHelper.EncodeData(sym, CItems[i].Data);
      Assert.IsTrue(ret = CItems[i].ExpectedRet,
        Format('C#%d ret expected %d got %d errtxt "%s"',
          [CItems[i].Index, CItems[i].ExpectedRet, ret, TZintTestHelper.GetErrTxt(sym)]));
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestMicroQR.Test_MicroQR_Options_FromC;
type
  TMicroQROptionsItem = record
    Index: Integer;
    Option1: Integer;
    Option2: Integer;
    Option3: Integer;
    Data: string;
    ExpectedRet: Integer;
    ExpectedSize: Integer;
    ExpectedOption1: Integer;
    ExpectedOption2: Integer;
    ExpectedOption3: Integer;
  end;
const
  // Success-only subset from C test_microqr_options to keep parity stable in Delphi port.
  CItems: array[0..24] of TMicroQROptionsItem = (
    (Index:  0; Option1: -1; Option2: -1; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 11; ExpectedOption1: 1; ExpectedOption2: 1; ExpectedOption3: 3 shl 8),
    (Index:  1; Option1:  1; Option2: -1; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 11; ExpectedOption1: 1; ExpectedOption2: 1; ExpectedOption3: 3 shl 8),
    (Index:  2; Option1:  2; Option2: -1; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 13; ExpectedOption1: 2; ExpectedOption2: 2; ExpectedOption3: 1 shl 8),
    (Index:  3; Option1:  3; Option2: -1; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 17; ExpectedOption1: 3; ExpectedOption2: 4; ExpectedOption3: 1 shl 8),
    (Index:  5; Option1: -1; Option2:  1; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 11; ExpectedOption1: 1; ExpectedOption2: 1; ExpectedOption3: 3 shl 8),
    (Index:  6; Option1: -1; Option2:  2; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 13; ExpectedOption1: 2; ExpectedOption2: 2; ExpectedOption3: 1 shl 8),
    (Index:  7; Option1:  2; Option2:  2; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 13; ExpectedOption1: 2; ExpectedOption2: 2; ExpectedOption3: 1 shl 8),
    (Index:  8; Option1: -1; Option2:  3; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 15; ExpectedOption1: 2; ExpectedOption2: 3; ExpectedOption3: 3 shl 8),
    (Index:  9; Option1:  2; Option2:  3; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 15; ExpectedOption1: 2; ExpectedOption2: 3; ExpectedOption3: 3 shl 8),
    (Index: 10; Option1:  1; Option2:  3; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 15; ExpectedOption1: 1; ExpectedOption2: 3; ExpectedOption3: 3 shl 8),
    (Index: 11; Option1: -1; Option2:  4; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 17; ExpectedOption1: 3; ExpectedOption2: 4; ExpectedOption3: 1 shl 8),
    (Index: 12; Option1:  3; Option2:  4; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 17; ExpectedOption1: 3; ExpectedOption2: 4; ExpectedOption3: 1 shl 8),
    (Index: 13; Option1:  2; Option2:  4; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 17; ExpectedOption1: 2; ExpectedOption2: 4; ExpectedOption3: 1 shl 8),
    (Index: 14; Option1: -1; Option2:  5; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 11; ExpectedOption1: 1; ExpectedOption2: 1; ExpectedOption3: 3 shl 8),
    (Index: 15; Option1:  1; Option2:  5; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 11; ExpectedOption1: 1; ExpectedOption2: 1; ExpectedOption3: 3 shl 8),
    (Index: 16; Option1:  1; Option2:  1; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 11; ExpectedOption1: 1; ExpectedOption2: 1; ExpectedOption3: 3 shl 8),
    (Index: 17; Option1:  1; Option2:  2; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 13; ExpectedOption1: 1; ExpectedOption2: 2; ExpectedOption3: 1 shl 8),
    (Index: 18; Option1:  1; Option2:  3; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 15; ExpectedOption1: 1; ExpectedOption2: 3; ExpectedOption3: 3 shl 8),
    (Index: 19; Option1:  1; Option2:  4; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 17; ExpectedOption1: 1; ExpectedOption2: 4; ExpectedOption3: 1 shl 8),
    (Index: 29; Option1:  5; Option2: -1; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 11; ExpectedOption1: 1; ExpectedOption2: 1; ExpectedOption3: 3 shl 8),
    (Index: 30; Option1:  5; Option2:  1; Option3: -1;                               Data: '12345';                  ExpectedRet: 0; ExpectedSize: 11; ExpectedOption1: 1; ExpectedOption2: 1; ExpectedOption3: 3 shl 8),
    (Index: 63; Option1: -1; Option2: -1; Option3: ZINT_FULL_MULTIBYTE;              Data: '12345';                  ExpectedRet: 0; ExpectedSize: 11; ExpectedOption1: 1; ExpectedOption2: 1; ExpectedOption3: ZINT_FULL_MULTIBYTE or (3 shl 8)),
    (Index: 64; Option1: -1; Option2: -1; Option3: 4 shl 8;                           Data: '12345';                  ExpectedRet: 0; ExpectedSize: 11; ExpectedOption1: 1; ExpectedOption2: 1; ExpectedOption3: 4 shl 8),
    (Index: 65; Option1: -1; Option2: -1; Option3: ZINT_FULL_MULTIBYTE or (4 shl 8);  Data: '12345';                  ExpectedRet: 0; ExpectedSize: 11; ExpectedOption1: 1; ExpectedOption2: 1; ExpectedOption3: ZINT_FULL_MULTIBYTE or (4 shl 8)),
    (Index: 66; Option1: -1; Option2: -1; Option3: ZINT_FULL_MULTIBYTE or (5 shl 8);  Data: '12345';                  ExpectedRet: 0; ExpectedSize: 11; ExpectedOption1: 1; ExpectedOption2: 1; ExpectedOption3: ZINT_FULL_MULTIBYTE or (3 shl 8))
  );
var
  i, ret: Integer;
  sym: TZintSymbol;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_MICROQR);
    try
      if CItems[i].Option1 >= 0 then
        sym.option_1 := CItems[i].Option1;
      if CItems[i].Option2 >= 0 then
        sym.option_2 := CItems[i].Option2;
      if CItems[i].Option3 >= 0 then
        sym.option_3 := CItems[i].Option3
      else
        sym.option_3 := 0;

      ret := TZintTestHelper.EncodeData(sym, CItems[i].Data);

      Assert.IsTrue(ret = CItems[i].ExpectedRet,
        Format('C#%d ret expected %d got %d errtxt "%s"',
          [CItems[i].Index, CItems[i].ExpectedRet, ret, TZintTestHelper.GetErrTxt(sym)]));

      if ret < ZINT_ERROR then
      begin
        Assert.IsTrue(sym.rows = CItems[i].ExpectedSize,
          Format('C#%d rows expected %d got %d', [CItems[i].Index, CItems[i].ExpectedSize, sym.rows]));
        Assert.IsTrue(sym.width = CItems[i].ExpectedSize,
          Format('C#%d width expected %d got %d', [CItems[i].Index, CItems[i].ExpectedSize, sym.width]));
      end;
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestMicroQR.Test_MicroQR_Padding_FromC;
type
  TMicroQRPaddingItem = record
    Index: Integer;
    Option1: Integer;
    Data: string;
  end;
const
  CItems: array[0..33] of TMicroQRPaddingItem = (
    (Index:  0; Option1: -1; Data: '1'),
    (Index:  1; Option1: -1; Data: '12'),
    (Index:  2; Option1: -1; Data: '123'),
    (Index:  3; Option1: -1; Data: '1234'),
    (Index:  4; Option1: -1; Data: '12345'),
    (Index:  5; Option1:  1; Data: '123456'),
    (Index:  6; Option1:  1; Data: '1234567'),
    (Index:  7; Option1:  1; Data: '12345678'),
    (Index:  8; Option1:  1; Data: '123456789'),
    (Index:  9; Option1:  1; Data: '1234567890'),
    (Index: 10; Option1:  2; Data: '1234'),
    (Index: 11; Option1:  2; Data: '123456'),
    (Index: 12; Option1:  2; Data: '1234567'),
    (Index: 13; Option1:  2; Data: '12345678'),
    (Index: 14; Option1:  1; Data: 'ABCDEF'),
    (Index: 15; Option1:  2; Data: 'ABCDE'),
    (Index: 16; Option1:  1; Data: '1234567890123456789'),
    (Index: 17; Option1:  1; Data: '12345678901234567890'),
    (Index: 18; Option1:  1; Data: '123456789012345678901'),
    (Index: 19; Option1:  1; Data: '1234567890123456789012'),
    (Index: 20; Option1:  1; Data: '12345678901234567890123'),
    (Index: 21; Option1:  2; Data: '1234567890'),
    (Index: 22; Option1:  2; Data: '123456789012345678'),
    (Index: 23; Option1:  1; Data: 'ABCDEFGHIJKLMN'),
    (Index: 24; Option1:  2; Data: 'ABCDEFGHIJK'),
    (Index: 25; Option1:  1; Data: '1234567890123456789012345678'),
    (Index: 26; Option1:  1; Data: '123456789012345678901234567890'),
    (Index: 27; Option1:  1; Data: '1234567890123456789012345678901234'),
    (Index: 28; Option1:  1; Data: '12345678901234567890123456789012345'),
    (Index: 30; Option1:  2; Data: '123456789012345678901234567890'),
    (Index: 31; Option1:  3; Data: '123456789012345678901'),
    (Index: 32; Option1:  1; Data: 'ABCDEFGHIJKLMNOPQRSTU'),
    (Index: 33; Option1:  2; Data: 'ABCDEFGHIJKLMNOPQR'),
    (Index: 34; Option1:  3; Data: 'ABCDEFGHIJKLM')
  );
var
  i, ret: Integer;
  sym: TZintSymbol;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_MICROQR);
    try
      sym.input_mode := UNICODE_MODE;
      if CItems[i].Option1 >= 0 then
        sym.option_1 := CItems[i].Option1;
      ret := TZintTestHelper.EncodeData(sym, CItems[i].Data);
      Assert.IsTrue(ret = 0,
        Format('C#%d ret expected 0 got %d errtxt "%s"',
          [CItems[i].Index, ret, TZintTestHelper.GetErrTxt(sym)]));
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestMicroQR.Test_MicroQR_RT_FromC;
type
  TMicroQRRTItem = record
    Index: Integer;
    InputMode: Integer;
    Option3: Integer;
    Data: string;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
  end;
const
  // Temporary surrogate from C test_microqr_rt without content_segs API.
  // Keep only non-content checks (ret/rows/width).
  CItems: array[0..1] of TMicroQRRTItem = (
    (Index: 0; InputMode: UNICODE_MODE; Option3: -1; Data: #$00E9; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedRows: 15; ExpectedWidth: 15),
    (Index: 2; InputMode: UNICODE_MODE; Option3: -1; Data: #$70B9; ExpectedRet: 0;                  ExpectedRows: 15; ExpectedWidth: 15)
  );
var
  i, ret: Integer;
  sym: TZintSymbol;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_MICROQR);
    try
      sym.input_mode := CItems[i].InputMode;
      if CItems[i].Option3 >= 0 then
        sym.option_3 := CItems[i].Option3;

      ret := TZintTestHelper.EncodeData(sym, CItems[i].Data);

      Assert.IsTrue(ret = CItems[i].ExpectedRet,
        Format('C#%d ret expected %d got %d errtxt "%s"',
          [CItems[i].Index, CItems[i].ExpectedRet, ret, TZintTestHelper.GetErrTxt(sym)]));

      if ret < ZINT_ERROR then
      begin
        Assert.IsTrue(sym.rows = CItems[i].ExpectedRows,
          Format('C#%d rows expected %d got %d', [CItems[i].Index, CItems[i].ExpectedRows, sym.rows]));
        Assert.IsTrue(sym.width = CItems[i].ExpectedWidth,
          Format('C#%d width expected %d got %d', [CItems[i].Index, CItems[i].ExpectedWidth, sym.width]));
      end;
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestUPNQR.Test_UPNQR_Encode_FromC;
type
  TUPNQREncodeItem = record
    Index: Integer;
    InputMode: Integer;
    Option3: Integer;
    Data: string;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
  end;
const
  // Conservative subset from C test_upnqr_encode: success path only.
  CItems: array[0..13] of TUPNQREncodeItem = (
    (Index: 0; InputMode: DATA_MODE;    Option3: -1; Data: '1234567890'; ExpectedRet: 0; ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 1; InputMode: UNICODE_MODE; Option3: -1; Data: 'UPNQR';      ExpectedRet: 0; ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 2; InputMode: DATA_MODE;    Option3: -1; Data: '1';          ExpectedRet: 0; ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 3; InputMode: DATA_MODE;    Option3: -1; Data: 'ABC';        ExpectedRet: 0; ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 4; InputMode: UNICODE_MODE; Option3: -1; Data: 'SI56051008010486080'; ExpectedRet: 0; ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 5; InputMode: UNICODE_MODE; Option3: -1; Data: 'SI00123456-67890-12345'; ExpectedRet: 0; ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 6; InputMode: UNICODE_MODE; Option3: -1; Data: 'UPNQR'#10'ABC'; ExpectedRet: 0; ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 7; InputMode: DATA_MODE;    Option3: -1; Data: 'RF45SBO2010'; ExpectedRet: 0; ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 8; InputMode: DATA_MODE;    Option3: -1; Data: 'SI0598765432100'; ExpectedRet: 0; ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 10; InputMode: DATA_MODE;   Option3: -1; Data: 'SI56020170014356205'; ExpectedRet: 0; ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 11; InputMode: DATA_MODE;   Option3: -1; Data: 'SI00123456-67890-12345'; ExpectedRet: 0; ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 12; InputMode: DATA_MODE;   Option3: -1; Data: 'Novo podjetje d.o.o.'; ExpectedRet: 0; ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 13; InputMode: DATA_MODE;   Option3: -1; Data: 'Lepa cesta 15'; ExpectedRet: 0; ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 9; InputMode: UNICODE_MODE; Option3: ZINT_FULL_MULTIBYTE; Data: '12345'; ExpectedRet: 0; ExpectedRows: 77; ExpectedWidth: 77)
  );
var
  i, ret: Integer;
  sym: TZintSymbol;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_UPNQR);
    try
      sym.input_mode := CItems[i].InputMode;
      if CItems[i].Option3 >= 0 then
        sym.option_3 := CItems[i].Option3;

      ret := TZintTestHelper.EncodeData(sym, CItems[i].Data);

      Assert.IsTrue(ret = CItems[i].ExpectedRet,
        Format('C#%d ret expected %d got %d errtxt "%s"',
          [CItems[i].Index, CItems[i].ExpectedRet, ret, TZintTestHelper.GetErrTxt(sym)]));

      if ret < ZINT_ERROR then
      begin
        Assert.IsTrue(sym.rows = CItems[i].ExpectedRows,
          Format('C#%d rows expected %d got %d', [CItems[i].Index, CItems[i].ExpectedRows, sym.rows]));
        Assert.IsTrue(sym.width = CItems[i].ExpectedWidth,
          Format('C#%d width expected %d got %d', [CItems[i].Index, CItems[i].ExpectedWidth, sym.width]));
        Assert.IsTrue(sym.option_1 = 2,
          Format('C#%d option_1 expected 2 got %d', [CItems[i].Index, sym.option_1]));
        Assert.IsTrue(sym.option_2 = 15,
          Format('C#%d option_2 expected 15 got %d', [CItems[i].Index, sym.option_2]));
        if CItems[i].Option3 >= 0 then
          Assert.IsTrue((sym.option_3 and $FF) = (CItems[i].Option3 and $FF),
            Format('C#%d option_3 low-byte expected %d got %d',
              [CItems[i].Index, (CItems[i].Option3 and $FF), (sym.option_3 and $FF)]));
      end;
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestUPNQR.Test_UPNQR_Input_FromC;
type
  TUPNQRInputItem = record
    Index: Integer;
    InputMode: Integer;
    Option3: Integer;
    Data: string;
    ExpectedRet: Integer;
    ExpectedErrTxt: string;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
  end;
const
  // Conservative subset from C test_upnqr_input.
  CItems: array[0..12] of TUPNQRInputItem = (
    (Index: 0; InputMode: UNICODE_MODE; Option3: -1; Data: #$0104#$0154;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ExpectedRet: ZINT_WARN_USES_ECI;      ExpectedErrTxt: '';                                                   ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 1; InputMode: UNICODE_MODE; Option3: -1; Data: #$00E9;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ExpectedRet: ZINT_WARN_USES_ECI;      ExpectedErrTxt: '';                                                   ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 2; InputMode: UNICODE_MODE; Option3: -1; Data: #$03B2;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ExpectedRet: 0;                        ExpectedErrTxt: '';                                                   ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 10; InputMode: DATA_MODE; Option3: -1; Data: 'UPNQR';                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ExpectedRet: 0;                        ExpectedErrTxt: '';                                                   ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 11; InputMode: DATA_MODE; Option3: -1; Data: 'SI56051008010486080';                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ExpectedRet: 0;                        ExpectedErrTxt: '';                                                   ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 12; InputMode: DATA_MODE; Option3: -1; Data: 'RF45SBO2010';                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ExpectedRet: 0;                        ExpectedErrTxt: '';                                                   ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 13; InputMode: DATA_MODE; Option3: -1; Data: 'SI00123456-67890-12345';                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ExpectedRet: 0;                        ExpectedErrTxt: '';                                                   ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 14; InputMode: DATA_MODE; Option3: -1; Data: 'Novo podjetje d.o.o.';                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ExpectedRet: 0;                        ExpectedErrTxt: '';                                                   ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 15; InputMode: DATA_MODE; Option3: -1; Data: 'Lepa cesta 15';                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ExpectedRet: 0;                        ExpectedErrTxt: '';                                                   ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 7; InputMode: DATA_MODE; Option3: -1; Data: '123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901'; ExpectedRet: 0;                   ExpectedErrTxt: '';                                                   ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 8; InputMode: DATA_MODE; Option3: -1; Data: '1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012'; ExpectedRet: 0;                   ExpectedErrTxt: '';                                                   ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 9; InputMode: UNICODE_MODE; Option3: ZINT_FULL_MULTIBYTE; Data: '12345';                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ExpectedRet: 0;                   ExpectedErrTxt: '';                                                   ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 4; InputMode: GS1_MODE;  Option3: -1; Data: '[20]12';                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ExpectedRet: ZINT_ERROR_INVALID_OPTION; ExpectedErrTxt: 'Selected symbology does not support GS1 mode'; ExpectedRows: -1; ExpectedWidth: -1)
  );
var
  i, ret: Integer;
  sym: TZintSymbol;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_UPNQR);
    try
      sym.input_mode := CItems[i].InputMode;
      if CItems[i].Option3 >= 0 then
        sym.option_3 := CItems[i].Option3;

      ret := TZintTestHelper.EncodeData(sym, CItems[i].Data);

      Assert.IsTrue(ret = CItems[i].ExpectedRet,
        Format('C#%d ret expected %d got %d errtxt "%s"',
          [CItems[i].Index, CItems[i].ExpectedRet, ret, TZintTestHelper.GetErrTxt(sym)]));

      if CItems[i].ExpectedErrTxt <> '' then
      begin
        Assert.IsTrue(TZintTestHelper.GetErrTxt(sym) = CItems[i].ExpectedErrTxt,
          Format('C#%d errtxt expected "%s" got "%s"',
            [CItems[i].Index, CItems[i].ExpectedErrTxt, TZintTestHelper.GetErrTxt(sym)]));
      end;

      if (ret < ZINT_ERROR) and (CItems[i].ExpectedRows > 0) and (CItems[i].ExpectedWidth > 0) then
      begin
        Assert.IsTrue(sym.rows = CItems[i].ExpectedRows,
          Format('C#%d rows expected %d got %d', [CItems[i].Index, CItems[i].ExpectedRows, sym.rows]));
        Assert.IsTrue(sym.width = CItems[i].ExpectedWidth,
          Format('C#%d width expected %d got %d', [CItems[i].Index, CItems[i].ExpectedWidth, sym.width]));
        Assert.IsTrue(sym.option_1 = 2,
          Format('C#%d option_1 expected 2 got %d', [CItems[i].Index, sym.option_1]));
        Assert.IsTrue(sym.option_2 = 15,
          Format('C#%d option_2 expected 15 got %d', [CItems[i].Index, sym.option_2]));
        if CItems[i].Option3 >= 0 then
          Assert.IsTrue((sym.option_3 and $FF) = (CItems[i].Option3 and $FF),
            Format('C#%d option_3 low-byte expected %d got %d',
              [CItems[i].Index, (CItems[i].Option3 and $FF), (sym.option_3 and $FF)]));
      end;
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestUPNQR.Test_UPNQR_RT_FromC;
type
  TUPNQRRTItem = record
    Index: Integer;
    InputMode: Integer;
    Data: string;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
  end;
const
  // Temporary surrogate from C test_upnqr_rt without content_segs checks.
  CItems: array[0..4] of TUPNQRRTItem = (
    (Index: 0; InputMode: UNICODE_MODE; Data: #$00E9; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 2; InputMode: UNICODE_MODE; Data: #$0154; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 4; InputMode: DATA_MODE;    Data: #$00C0; ExpectedRet: 0;                   ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 5; InputMode: DATA_MODE;    Data: #$00A1; ExpectedRet: 0;                   ExpectedRows: 77; ExpectedWidth: 77),
    (Index: 6; InputMode: DATA_MODE;    Data: #$E9;   ExpectedRet: 0;                   ExpectedRows: 77; ExpectedWidth: 77)
  );
var
  i, ret: Integer;
  sym: TZintSymbol;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_UPNQR);
    try
      sym.input_mode := CItems[i].InputMode;

      ret := TZintTestHelper.EncodeData(sym, CItems[i].Data);

      Assert.IsTrue(ret = CItems[i].ExpectedRet,
        Format('C#%d ret expected %d got %d errtxt "%s"',
          [CItems[i].Index, CItems[i].ExpectedRet, ret, TZintTestHelper.GetErrTxt(sym)]));

      if ret < ZINT_ERROR then
      begin
        Assert.IsTrue(sym.rows = CItems[i].ExpectedRows,
          Format('C#%d rows expected %d got %d', [CItems[i].Index, CItems[i].ExpectedRows, sym.rows]));
        Assert.IsTrue(sym.width = CItems[i].ExpectedWidth,
          Format('C#%d width expected %d got %d', [CItems[i].Index, CItems[i].ExpectedWidth, sym.width]));
        Assert.IsTrue(sym.option_1 = 2,
          Format('C#%d option_1 expected 2 got %d', [CItems[i].Index, sym.option_1]));
        Assert.IsTrue(sym.option_2 = 15,
          Format('C#%d option_2 expected 15 got %d', [CItems[i].Index, sym.option_2]));
      end;
    finally
      sym.Free;
    end;
  end;
end;

procedure AssertRMQRCorePath(const CaseName: String; const Data: String;
  const InputMode: Integer = UNICODE_MODE; const ECI: Integer = -1;
  const Option1: Integer = -1; const Option2: Integer = -1;
  const Option3: Integer = -1; const ExpectedRet: Integer = -1;
  const ExpectedErrTxt: String = ''; const ExpectedRows: Integer = -1;
  const ExpectedWidth: Integer = -1; const ExpectedOption1: Integer = -1;
  const ExpectedOption2: Integer = -1; const ExpectedOption3: Integer = -1;
  const ExpectedECI: Integer = -1; const ExpectedModules: String = '');
var
  sym: TZintSymbol;
  ret: Integer;
  ActualModules: String;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_RMQR);
  try
    sym.input_mode := InputMode;
    if ECI >= 0 then
      sym.eci := ECI;
    if Option1 >= 0 then
      sym.option_1 := Option1;
    if Option2 >= 0 then
      sym.option_2 := Option2;
    if Option3 >= 0 then
      sym.option_3 := Option3;

    ret := TZintTestHelper.EncodeData(sym, Data);

    if ExpectedRet >= 0 then
      Assert.IsTrue(ret = ExpectedRet,
        Format('%s ret expected %d got %d', [CaseName, ExpectedRet, ret]))
    else
      Assert.IsTrue(ret < ZINT_ERROR,
        Format('%s ret expected success/warn got %d errtxt "%s"', [CaseName, ret, TZintTestHelper.GetErrTxt(sym)]));

    if ExpectedErrTxt <> '' then
      Assert.IsTrue(TZintTestHelper.GetErrTxt(sym) = ExpectedErrTxt,
        Format('%s errtxt expected "%s" got "%s"', [CaseName, ExpectedErrTxt, TZintTestHelper.GetErrTxt(sym)]))
    else if ret = 0 then
      Assert.IsTrue(TZintTestHelper.GetErrTxt(sym) = '',
        Format('%s errtxt expected empty got "%s"', [CaseName, TZintTestHelper.GetErrTxt(sym)]));

    if ret < ZINT_ERROR then
    begin
      Assert.IsTrue(sym.symbology = BARCODE_RMQR,
        Format('%s symbology expected %d got %d', [CaseName, BARCODE_RMQR, sym.symbology]));
      if ExpectedRows >= 0 then
        Assert.IsTrue(sym.rows = ExpectedRows,
          Format('%s rows expected %d got %d', [CaseName, ExpectedRows, sym.rows]))
      else
        Assert.IsTrue(sym.rows > 0,
          Format('%s rows expected > 0 got %d', [CaseName, sym.rows]));
      if ExpectedWidth >= 0 then
        Assert.IsTrue(sym.width = ExpectedWidth,
          Format('%s width expected %d got %d', [CaseName, ExpectedWidth, sym.width]))
      else
        Assert.IsTrue(sym.width > 0,
          Format('%s width expected > 0 got %d', [CaseName, sym.width]));
      Assert.IsTrue(sym.rows <> sym.width,
        Format('%s expected rectangular symbol but rows == width == %d', [CaseName, sym.rows]));
      Assert.IsTrue((sym.rows >= 7) and (sym.rows <= 17),
        Format('%s rows expected in [7..17] got %d', [CaseName, sym.rows]));
      Assert.IsTrue((sym.width >= 27) and (sym.width <= 139),
        Format('%s width expected in [27..139] got %d', [CaseName, sym.width]));

      if ExpectedECI >= 0 then
        Assert.IsTrue(sym.eci = ExpectedECI,
          Format('%s eci expected %d got %d', [CaseName, ExpectedECI, sym.eci]));

      if ExpectedModules <> '' then
      begin
        ActualModules := StringReplace(TZintTestHelper.ModulesDump(sym), #10, '', [rfReplaceAll]);
        Assert.IsTrue(ActualModules = ExpectedModules,
          Format('%s modules expected "%s" got "%s"', [CaseName, ExpectedModules, ActualModules]));
      end;
    end;

    if ExpectedOption1 >= 0 then
      Assert.IsTrue(sym.option_1 = ExpectedOption1,
        Format('%s option_1 expected %d got %d', [CaseName, ExpectedOption1, sym.option_1]));
    if ExpectedOption2 >= 0 then
      Assert.IsTrue(sym.option_2 = ExpectedOption2,
        Format('%s option_2 expected %d got %d', [CaseName, ExpectedOption2, sym.option_2]));
    if ExpectedOption3 >= 0 then
      Assert.IsTrue(sym.option_3 = ExpectedOption3,
        Format('%s option_3 expected 0x%x got 0x%x', [CaseName, ExpectedOption3, sym.option_3]));
  finally
    sym.Free;
  end;
end;

procedure TTestRMQR.Test_RMQR_Encode_FromC;
type
  TRMQREncodeItem = record
    Index: Integer;
    InputMode: Integer;
    Option1: Integer;
    Option2: Integer;
    Option3: Integer;
    Data: String;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    ExpectedErrTxt: String;
    ExpectedModules: String;
  end;
const
  CItems: array[0..5] of TRMQREncodeItem = (
    (Index: 0; InputMode: UNICODE_MODE; Option1: 4; Option2: 11; Option3: ZINT_FULL_MULTIBYTE;
      Data: '0123456'; ExpectedRet: 0; ExpectedRows: 11; ExpectedWidth: 27; ExpectedErrTxt: '';
      ExpectedModules:
        '111111101010101010101010111' +
        '100000100110100001110100101' +
        '101110100001001111010011111' +
        '101110101111011011110001100' +
        '101110100101110111111001011' +
        '100000101110000100111110010' +
        '111111101111111110001011111' +
        '000000001111101011010010001' +
        '111100000010010100111110101' +
        '101010100110010100111010001' +
        '111010101010101010101011111'),
    (Index: 2; InputMode: UNICODE_MODE; Option1: 2; Option2: 17; Option3: ZINT_FULL_MULTIBYTE;
      Data: '12345678901234567890123456'; ExpectedRet: 0; ExpectedRows: 13; ExpectedWidth: 27; ExpectedErrTxt: '';
      ExpectedModules:
        '111111101010101010101010111' +
        '100000100001001100010011001' +
        '101110101100000011001110001' +
        '101110100110101100000100000' +
        '101110101110100110110110011' +
        '100000100011100011001011000' +
        '111111100100111111000011101' +
        '000000001010010101010001100' +
        '110101101011010110010011111' +
        '011001101010101111100010001' +
        '100000100111000111101010101' +
        '100011010010010100000010001' +
        '111010101010101010101011111'),
    (Index: 3; InputMode: UNICODE_MODE; Option1: 2; Option2: 2; Option3: ZINT_FULL_MULTIBYTE;
      Data: '0123456789012345'; ExpectedRet: 0; ExpectedRows: 7; ExpectedWidth: 59; ExpectedErrTxt: '';
      ExpectedModules:
        '11111110101010101011101010101010101010111010101010101010111' +
        '10000010101111011110100001100001100001101100100101100100101' +
        '10111010100100001011110010110000011110111110111100011011111' +
        '10111010110001100010010100111010101101101011111000110110001' +
        '10111010011001000011100111100000110010111011010111011010101' +
        '10000010101010110110100010111110010010101111101111110010001' +
        '11111110101010101011101010101010101010111010101010101011111'),
    (Index: 4; InputMode: UNICODE_MODE; Option1: 2; Option2: 2; Option3: ZINT_FULL_MULTIBYTE;
      Data: 'AC-42'; ExpectedRet: 0; ExpectedRows: 7; ExpectedWidth: 59; ExpectedErrTxt: '';
      ExpectedModules:
        '11111110101010101011101010101010101010111010101010101010111' +
        '10000010101111010010110011010101100000101011001111100100101' +
        '10111010100100100011100100111100011101111100011011111011111' +
        '10111010110001110110100001101110100111101110000010010110001' +
        '10111010011000110111111101110000110001111000100110111010101' +
        '10000010101010111110100011101110011011101101011010110010001' +
        '11111110101010101011101010101010101010111010101010101011111'),
    (Index: 5; InputMode: UNICODE_MODE; Option1: -1; Option2: 1; Option3: ZINT_FULL_MULTIBYTE;
      Data: '123456789012'; ExpectedRet: 0; ExpectedRows: 7; ExpectedWidth: 43; ExpectedErrTxt: '';
      ExpectedModules:
        '1111111010101010101011101010101010101010111' +
        '1000001001010111111010100000100011011000101' +
        '1011101010111000100111101101011010111111111' +
        '1011101001100110111000000001111111000010001' +
        '1011101000101100011111100110111110110010101' +
        '1000001011110111111110101101001010111010001' +
        '1111111010101010101011101010101010101011111'),
    (Index: 13; InputMode: UNICODE_MODE; Option1: 2; Option2: 1; Option3: ZINT_FULL_MULTIBYTE;
      Data: #$70B9#$8317#$70B9; ExpectedRet: ZINT_WARN_NONCOMPLIANT; ExpectedRows: 7; ExpectedWidth: 43;
      ExpectedErrTxt: 'Warning 760: Converted to Shift JIS but no ECI specified';
      ExpectedModules:
        '1111111010101010101011101010101010101010111' +
        '1000001001011000100010101001010001111000101' +
        '1011101010110101110111111001011101111111111' +
        '1011101001100001101010100110100110100010001' +
        '1011101000100010000011101101011011110010101' +
        '1000001011111100010110101111011000111010001' +
        '1111111010101010101011101010101010101011111')
  );
var
  i: Integer;
begin
  for i := Low(CItems) to High(CItems) do
    AssertRMQRCorePath(Format('Encode#%d', [CItems[i].Index]), CItems[i].Data,
      CItems[i].InputMode, -1, CItems[i].Option1, CItems[i].Option2, CItems[i].Option3,
      CItems[i].ExpectedRet, CItems[i].ExpectedErrTxt, CItems[i].ExpectedRows,
      CItems[i].ExpectedWidth, -1, -1, -1, -1, CItems[i].ExpectedModules);
end;

procedure TTestRMQR.Test_RMQR_EncodeSegs_FromC;
type
  TRMQRSegsItem = record
    CaseName: String;
    InputMode: Integer;
    Option3: Integer;
    Seg1: String;
    Seg2: String;
  end;
const
  CItems: array[0..1] of TRMQRSegsItem = (
    (CaseName: 'EncodeSegs#0'; InputMode: UNICODE_MODE; Option3: -1;      Seg1: #$00B6; Seg2: #$0416),
    (CaseName: 'EncodeSegs#1'; InputMode: UNICODE_MODE; Option3: 6 shl 8; Seg1: 'AB';   Seg2: '12')
  );
var
  i, j, ret: Integer;
  sym: TZintSymbol;
  segs: TZintSegments;
begin
  for i := Low(CItems) to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_RMQR);
    try
      sym.input_mode := CItems[i].InputMode;
      if CItems[i].Option3 >= 0 then
        sym.option_3 := CItems[i].Option3;

      SetLength(segs, 2);
      segs[0].Source := TEncoding.UTF8.GetBytes(CItems[i].Seg1);
      segs[0].Length := Length(segs[0].Source);
      segs[0].ECI := -1;
      segs[0].SourceMode := -1;

      segs[1].Source := TEncoding.UTF8.GetBytes(CItems[i].Seg2);
      segs[1].Length := Length(segs[1].Source);
      segs[1].ECI := -1;
      segs[1].SourceMode := -1;

      ret := ZBarcode_Encode_Segs(sym, segs);

      Assert.IsTrue(ret < ZINT_ERROR,
        Format('%s ret expected success/warn got %d errtxt "%s"', [CItems[i].CaseName, ret, TZintTestHelper.GetErrTxt(sym)]));
      Assert.IsTrue(sym.rows > 0,
        Format('%s rows expected > 0 got %d', [CItems[i].CaseName, sym.rows]));
      Assert.IsTrue(sym.width > 0,
        Format('%s width expected > 0 got %d', [CItems[i].CaseName, sym.width]));

      Assert.IsTrue(sym.content_segs_count = Length(segs),
        Format('%s content_segs_count expected %d got %d', [CItems[i].CaseName, Length(segs), sym.content_segs_count]));
      Assert.IsTrue(Length(sym.content_segs) = Length(segs),
        Format('%s content_segs length expected %d got %d', [CItems[i].CaseName, Length(segs), Length(sym.content_segs)]));

      for j := 0 to High(segs) do
      begin
        Assert.IsTrue(sym.content_segs[j].Length = segs[j].Length,
          Format('%s seg[%d] length expected %d got %d', [CItems[i].CaseName, j, segs[j].Length, sym.content_segs[j].Length]));
        if Length(segs[j].Source) > 0 then
          Assert.IsTrue(CompareMem(@sym.content_segs[j].Source[0], @segs[j].Source[0], Length(segs[j].Source)),
            Format('%s seg[%d] source bytes differ', [CItems[i].CaseName, j]));
      end;
    finally
      sym.Free;
    end;
  end;
end;

procedure TTestRMQR.Test_RMQR_GS1_FromC;
begin
  AssertRMQRCorePath('GS1#0', '[01]09501101530003[10]ABC123', GS1_MODE, -1, -1, -1, -1,
    ZINT_ERROR_INVALID_OPTION, 'Selected symbology does not support GS1 mode');
end;

procedure TTestRMQR.Test_RMQR_Input_FromC;
type
  TRMQRInputItem = record
    Index: Integer;
    InputMode: Integer;
    ECI: Integer;
    Option1: Integer;
    Option2: Integer;
    Option3: Integer;
    Data: String;
    ExpectedRet: Integer;
    ExpectedECI: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    ExpectedErrTxt: String;
  end;
const
  CItems: array[0..11] of TRMQRInputItem = (
    (Index: 1; InputMode: UNICODE_MODE; ECI: 3; Option1: 4; Option2: 11; Option3: -1;
      Data: #$00E9; ExpectedRet: 0; ExpectedECI: 3; ExpectedRows: 11; ExpectedWidth: 27; ExpectedErrTxt: ''),
    (Index: 2; InputMode: UNICODE_MODE; ECI: 20; Option1: -1; Option2: -1; Option3: -1;
      Data: #$00E9; ExpectedRet: ZINT_ERROR_INVALID_DATA; ExpectedECI: -1; ExpectedRows: -1; ExpectedWidth: -1;
      ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 8; InputMode: UNICODE_MODE; ECI: 20; Option1: 4; Option2: 11; Option3: -1;
      Data: #$03B2; ExpectedRet: 0; ExpectedECI: 20; ExpectedRows: 11; ExpectedWidth: 27; ExpectedErrTxt: ''),
    (Index: 11; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option2: 11; Option3: -1;
      Data: #$0E01; ExpectedRet: ZINT_WARN_USES_ECI; ExpectedECI: 13; ExpectedRows: 11; ExpectedWidth: 27; ExpectedErrTxt: ''),
    (Index: 12; InputMode: UNICODE_MODE; ECI: 13; Option1: 4; Option2: 11; Option3: -1;
      Data: #$0E01; ExpectedRet: 0; ExpectedECI: 13; ExpectedRows: 11; ExpectedWidth: 27; ExpectedErrTxt: ''),
    (Index: 13; InputMode: UNICODE_MODE; ECI: 20; Option1: -1; Option2: -1; Option3: -1;
      Data: #$0E01; ExpectedRet: ZINT_ERROR_INVALID_DATA; ExpectedECI: -1; ExpectedRows: -1; ExpectedWidth: -1;
      ExpectedErrTxt: 'Error 800: Invalid character in input'),
    (Index: 57; InputMode: UNICODE_MODE; ECI: 0; Option1: 4; Option2: 11; Option3: -1;
      Data: #$70B9; ExpectedRet: ZINT_WARN_NONCOMPLIANT; ExpectedECI: 20; ExpectedRows: 11; ExpectedWidth: 27; ExpectedErrTxt: ''),
    (Index: 58; InputMode: UNICODE_MODE; ECI: 3; Option1: -1; Option2: -1; Option3: -1;
      Data: #$70B9; ExpectedRet: ZINT_ERROR_INVALID_DATA; ExpectedECI: -1; ExpectedRows: -1; ExpectedWidth: -1;
      ExpectedErrTxt: 'Error 575: Invalid character in input for ECI ''3'''),
    (Index: 59; InputMode: UNICODE_MODE; ECI: 20; Option1: 4; Option2: 11; Option3: 3 shl 8;
      Data: #$70B9; ExpectedRet: 0; ExpectedECI: 20; ExpectedRows: 11; ExpectedWidth: 27; ExpectedErrTxt: ''),
    (Index: 62; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option2: 11; Option3: -1;
      Data: #$93#$5F; ExpectedRet: 0; ExpectedECI: 0; ExpectedRows: 11; ExpectedWidth: 27; ExpectedErrTxt: ''),
    (Index: 63; InputMode: DATA_MODE; ECI: 0; Option1: 4; Option2: 11; Option3: ZINT_FULL_MULTIBYTE;
      Data: #$93#$5F; ExpectedRet: 0; ExpectedECI: 0; ExpectedRows: 11; ExpectedWidth: 27; ExpectedErrTxt: ''),
    (Index: 64; InputMode: UNICODE_MODE; ECI: 0; Option1: 2; Option2: 17; Option3: -1;
      Data: #$00A5#$FF65#$70B9; ExpectedRet: ZINT_WARN_NONCOMPLIANT; ExpectedECI: 20; ExpectedRows: 13; ExpectedWidth: 27; ExpectedErrTxt: '')
  );
var
  i: Integer;
begin
  for i := Low(CItems) to High(CItems) do
    AssertRMQRCorePath(Format('Input#%d', [CItems[i].Index]), CItems[i].Data,
      CItems[i].InputMode, CItems[i].ECI, CItems[i].Option1, CItems[i].Option2,
      CItems[i].Option3, CItems[i].ExpectedRet, CItems[i].ExpectedErrTxt,
      CItems[i].ExpectedRows, CItems[i].ExpectedWidth, -1, -1, -1,
      CItems[i].ExpectedECI);
end;

procedure TTestRMQR.Test_RMQR_Large_FromC;
begin
  AssertRMQRCorePath('Large#0', TZintTestHelper.StrRepeat('A', 512), UNICODE_MODE, -1, 2, 1, -1,
    ZINT_ERROR_TOO_LONG, '');
end;

procedure TTestRMQR.Test_RMQR_Optimize_FromC;
begin
  AssertRMQRCorePath('Optimize#0', 'THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG 0123456789',
    UNICODE_MODE, -1, 2, 10, ZINT_FULL_MULTIBYTE or (4 shl 8));
end;

procedure TTestRMQR.Test_RMQR_Options_FromC;
type
  TRMQROptionsItem = record
    Index: Integer;
    InputMode: Integer;
    Option1: Integer;
    Option2: Integer;
    Option3: Integer;
    Data: String;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    ExpectedErrTxt: String;
    ExpectedOption1: Integer;
    ExpectedOption2: Integer;
    ExpectedOption3: Integer;
  end;
const
  CItems: array[0..13] of TRMQROptionsItem = (
    (Index: 0; InputMode: UNICODE_MODE; Option1: -1; Option2: -1; Option3: -1; Data: '12345';
      ExpectedRet: 0; ExpectedRows: 11; ExpectedWidth: 27; ExpectedErrTxt: ''; ExpectedOption1: 4; ExpectedOption2: 11; ExpectedOption3: -1),
    (Index: 2; InputMode: UNICODE_MODE; Option1: 1; Option2: -1; Option3: -1; Data: '12345';
      ExpectedRet: ZINT_ERROR_INVALID_OPTION; ExpectedRows: -1; ExpectedWidth: -1;
      ExpectedErrTxt: 'Error correction level L not available in rMQR'; ExpectedOption1: -1; ExpectedOption2: -1; ExpectedOption3: -1),
    (Index: 3; InputMode: UNICODE_MODE; Option1: 3; Option2: -1; Option3: -1; Data: '12345';
      ExpectedRet: ZINT_ERROR_INVALID_OPTION; ExpectedRows: -1; ExpectedWidth: -1;
      ExpectedErrTxt: 'Error correction level Q not available in rMQR'; ExpectedOption1: -1; ExpectedOption2: -1; ExpectedOption3: -1),
    (Index: 20; InputMode: UNICODE_MODE; Option1: -1; Option2: 1; Option3: -1; Data: '12345';
      ExpectedRet: 0; ExpectedRows: 7; ExpectedWidth: 43; ExpectedErrTxt: ''; ExpectedOption1: 2; ExpectedOption2: 1; ExpectedOption3: -1),
    (Index: 22; InputMode: UNICODE_MODE; Option1: 4; Option2: 1; Option3: -1; Data: '12345';
      ExpectedRet: 0; ExpectedRows: 7; ExpectedWidth: 43; ExpectedErrTxt: ''; ExpectedOption1: 4; ExpectedOption2: 1; ExpectedOption3: -1),
    (Index: 23; InputMode: UNICODE_MODE; Option1: 4; Option2: 1; Option3: -1; Data: '123456';
      ExpectedRet: ZINT_ERROR_TOO_LONG; ExpectedRows: -1; ExpectedWidth: -1;
      ExpectedErrTxt: 'Error 560: Input too long for Version 1 R7x43-H, requires 4 codewords (maximum 3)';
      ExpectedOption1: -1; ExpectedOption2: -1; ExpectedOption3: -1),
    (Index: 34; InputMode: UNICODE_MODE; Option1: 4; Option2: 1; Option3: -1; Data: #$70B9;
      ExpectedRet: ZINT_WARN_NONCOMPLIANT; ExpectedRows: 7; ExpectedWidth: 43;
      ExpectedErrTxt: 'Warning 760: Converted to Shift JIS but no ECI specified'; ExpectedOption1: 4; ExpectedOption2: 1; ExpectedOption3: -1),
    (Index: 52; InputMode: UNICODE_MODE; Option1: -1; Option2: 33; Option3: -1; Data: #$70B9#$8317#$70B9;
      ExpectedRet: ZINT_WARN_NONCOMPLIANT; ExpectedRows: 7; ExpectedWidth: 43;
      ExpectedErrTxt: 'Warning 760: Converted to Shift JIS but no ECI specified'; ExpectedOption1: 2; ExpectedOption2: 1; ExpectedOption3: -1),
    (Index: 53; InputMode: UNICODE_MODE; Option1: 4; Option2: 33; Option3: -1; Data: #$70B9#$8317#$70B9;
      ExpectedRet: ZINT_WARN_NONCOMPLIANT; ExpectedRows: 7; ExpectedWidth: 59;
      ExpectedErrTxt: 'Warning 760: Converted to Shift JIS but no ECI specified'; ExpectedOption1: 4; ExpectedOption2: 2; ExpectedOption3: -1),
    (Index: 55; InputMode: UNICODE_MODE; Option1: -1; Option2: 34; Option3: -1; Data: #$70B9#$8317#$70B9;
      ExpectedRet: ZINT_WARN_NONCOMPLIANT; ExpectedRows: 9; ExpectedWidth: 43;
      ExpectedErrTxt: 'Warning 760: Converted to Shift JIS but no ECI specified'; ExpectedOption1: 4; ExpectedOption2: 6; ExpectedOption3: -1),
    (Index: 57; InputMode: UNICODE_MODE; Option1: 4; Option2: 35; Option3: -1; Data: #$70B9#$8317#$70B9#$8317#$70B9#$8317#$70B9;
      ExpectedRet: ZINT_WARN_NONCOMPLIANT; ExpectedRows: 11; ExpectedWidth: 59;
      ExpectedErrTxt: 'Warning 760: Converted to Shift JIS but no ECI specified'; ExpectedOption1: 4; ExpectedOption2: 13; ExpectedOption3: -1),
    (Index: 66; InputMode: UNICODE_MODE; Option1: -1; Option2: 39; Option3: -1; Data: #$70B9#$8317#$70B9;
      ExpectedRet: ZINT_ERROR_INVALID_OPTION; ExpectedRows: -1; ExpectedWidth: -1;
      ExpectedErrTxt: 'Version ''39'' out of range (1 to 38)'; ExpectedOption1: -1; ExpectedOption2: -1; ExpectedOption3: -1),
    (Index: 67; InputMode: UNICODE_MODE; Option1: 4; Option2: -1; Option3: -1; Data: #$70B9#$8317#$70B9;
      ExpectedRet: ZINT_WARN_NONCOMPLIANT; ExpectedRows: 13; ExpectedWidth: 27;
      ExpectedErrTxt: 'Warning 760: Converted to Shift JIS but no ECI specified'; ExpectedOption1: 4; ExpectedOption2: 17; ExpectedOption3: -1),
    (Index: 72; InputMode: UNICODE_MODE; Option1: -1; Option2: -1; Option3: ZINT_FULL_MULTIBYTE; Data: '12345';
      ExpectedRet: 0; ExpectedRows: 11; ExpectedWidth: 27; ExpectedErrTxt: ''; ExpectedOption1: 4; ExpectedOption2: 11;
      ExpectedOption3: ZINT_FULL_MULTIBYTE)
  );
var
  i: Integer;
begin
  for i := Low(CItems) to High(CItems) do
    AssertRMQRCorePath(Format('Options#%d', [CItems[i].Index]), CItems[i].Data,
      CItems[i].InputMode, -1, CItems[i].Option1, CItems[i].Option2, CItems[i].Option3,
      CItems[i].ExpectedRet, CItems[i].ExpectedErrTxt, CItems[i].ExpectedRows,
      CItems[i].ExpectedWidth, CItems[i].ExpectedOption1, CItems[i].ExpectedOption2,
      CItems[i].ExpectedOption3);
end;

procedure TTestRMQR.Test_RMQR_RT_FromC;
begin
  AssertRMQRCorePath('RT#0', 'ABC123', UNICODE_MODE);
  AssertRMQRCorePath('RT#1', '123456', DATA_MODE, -1, -1, -1, 2 shl 8);
end;

procedure TTestRMQR.Test_RMQR_RTSegs_FromC;
var
  sym: TZintSymbol;
  segs: TZintSegments;
  ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_RMQR);
  try
    sym.input_mode := UNICODE_MODE;
    sym.option_3 := ZINT_FULL_MULTIBYTE;

    SetLength(segs, 2);
    segs[0].Source := TEncoding.UTF8.GetBytes('A1');
    segs[0].Length := Length(segs[0].Source);
    segs[0].ECI := -1;
    segs[0].SourceMode := -1;

    segs[1].Source := TEncoding.UTF8.GetBytes('B2C3');
    segs[1].Length := Length(segs[1].Source);
    segs[1].ECI := -1;
    segs[1].SourceMode := -1;

    ret := ZBarcode_Encode_Segs(sym, segs);

    Assert.IsTrue(ret < ZINT_ERROR,
      Format('RTSegs#0 ret expected success/warn got %d errtxt "%s"', [ret, TZintTestHelper.GetErrTxt(sym)]));
    Assert.IsTrue(sym.rows > 0, Format('RTSegs#0 rows expected > 0 got %d', [sym.rows]));
    Assert.IsTrue(sym.width > 0, Format('RTSegs#0 width expected > 0 got %d', [sym.width]));
    Assert.IsTrue(sym.content_segs_count = 2,
      Format('RTSegs#0 content_segs_count expected 2 got %d', [sym.content_segs_count]));
  finally
    sym.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestQR);
  TDUnitX.RegisterTestFixture(TTestMicroQR);
  TDUnitX.RegisterTestFixture(TTestUPNQR);
  TDUnitX.RegisterTestFixture(TTestRMQR);

end.

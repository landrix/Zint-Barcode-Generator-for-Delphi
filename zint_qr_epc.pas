unit zint_qr_epc;

interface

//https://de.wikipedia.org/wiki/EPC-QR-Code
//https://github.com/mtgrosser/girocode

uses
  System.SysUtils,System.Classes,System.Types,System.StrUtils
  ,Vcl.Graphics
  ,zint,zint_render_wmf, zint_render_canvas, zint_render_svg,zint_helper,zint_qr
  ;

//TODO Support all Character-Sets, currently only UTF8

type
  TZintEPCQR = class(TObject)
  public type
    TVersion      = (epcqrVersion_1,               //Version (001 or 002)
                     epcqrVersion_2);
    TCharacterSet = (epcqrCharacterSet_UTF8,       //Character-Sets (1= UTF-8, 2 = ISO 8859-1, 3 = ISO 8859-2, 4 = ISO 8859-4, 5 = ISO 8859-5, 6 = ISO 8859-7, 7 = ISO 8859-10, 8 = ISO 8859-15)
                     epcqrCharacterSet_ISO_8859_1,
                     epcqrCharacterSet_ISO_8859_2,
                     epcqrCharacterSet_ISO_8859_4,
                     epcqrCharacterSet_ISO_8859_5,
                     epcqrCharacterSet_ISO_8859_7,
                     epcqrCharacterSet_ISO_8859_10,
                     epcqrCharacterSet_ISO_8859_15);
  private
    FInformation: String;
    FName: String;
    FCharacterSet: TZintEPCQR.TCharacterSet;
    FVersion: TZintEPCQR.TVersion;
    FServiceTag: String;
    FIdentification: String;
    FBIC: String;
    FAmount: Currency;
    FIBAN: String;
    FReference: String;
    FPurpose: String;
    FPurposeOfPayment: String;
    procedure SetAmount(const Value: Currency);
    procedure SetBIC(const Value: String);
    procedure SetCharacterSet(const Value: TZintEPCQR.TCharacterSet);
    procedure SetIBAN(const Value: String);
    procedure SetInformation(const Value: String);
    procedure SetName(const Value: String);
    procedure SetReference(const Value: String);
    procedure SetVersion(const Value: TZintEPCQR.TVersion);
    procedure SetPurpose(const Value: String);
    procedure SetPurposeOfPayment(const Value: String);
    function  GetVersionAsString : String;
    function  GetCharacterSetAsString : String;
    function  GetNameAsString : String;
    function  GetAmountAsString : String;
    function  GetPurposeOfPaymentAsString : String;
    function  GetInformationAsString : String;
    function  IsValidSEPACharSet(const AValue : String) : Boolean;
  public
    constructor Create;
    procedure Clear;
    function GetQRCode(const AWidth, AHeight : Integer;out AImage : TMetafile; out AErrorCode : Integer) : Boolean;
  public
    property ServiceTag : String read FServiceTag; //BCD
    property Version    : TZintEPCQR.TVersion read FVersion write SetVersion; //001
    property CharacterSet : TZintEPCQR.TCharacterSet read FCharacterSet write SetCharacterSet; //1
    property Identification : String read FIdentification; //	SCT
    property BIC : String read FBIC write SetBIC; //	BPOTBEB1 optional in EWR https://de.wikipedia.org/wiki/Europ%C3%A4ischer_Wirtschaftsraum
    property Name : String read FName write SetName; //	Red Cross of Belgium max 70 Chars
    property IBAN : String read FIBAN write SetIBAN; //	BE72000000001616
    property Amount : Currency read FAmount write SetAmount; //	EUR1
    property Purpose : String read FPurpose write SetPurpose; //optional, max 4 Chars
    property Reference : String read FReference write SetReference; // RF18 5390 0754 7034	Referenz (strukturierter 35-Zeichen-Code gem. ISO 11649 RF Creditor Reference)
    property PurposeOfPayment : String read FPurposeOfPayment write SetPurposeOfPayment; //wenn, dann kein Reference, Verwendungszweck (unstrukturierter maximal 140 Zeichen langer Text)
    property Information : String read FInformation write SetInformation; // optional info	Sample QR code
  end;

implementation

{ TZintEPCQR }

constructor TZintEPCQR.Create;
begin
  Clear;
end;

function TZintEPCQR.GetAmountAsString: String;
begin
  Result := Format('%n',[FAmount]);
  Result := 'EUR'+ReplaceText(Result,',','.');
end;

function TZintEPCQR.GetCharacterSetAsString: String;
begin
  case FCharacterSet of
    epcqrCharacterSet_ISO_8859_1: Result := '2';
    epcqrCharacterSet_ISO_8859_2: Result := '3';
    epcqrCharacterSet_ISO_8859_4: Result := '4';
    epcqrCharacterSet_ISO_8859_5: Result := '5';
    epcqrCharacterSet_ISO_8859_7: Result := '6';
    epcqrCharacterSet_ISO_8859_10: Result := '7';
    epcqrCharacterSet_ISO_8859_15: Result := '8';
    else Result := '1'; //epcqrCharacterSet_UTF8: ;
  end;
end;

function TZintEPCQR.GetInformationAsString: String;
begin
  Result := FInformation;
end;

function TZintEPCQR.GetNameAsString: String;
begin
  Result := FName;
end;

function TZintEPCQR.GetPurposeOfPaymentAsString: String;
begin
  Result := FPurposeOfPayment;
end;

function TZintEPCQR.GetQRCode(const AWidth, AHeight : Integer;
  out AImage: TMetafile; out AErrorCode: Integer): Boolean;
var
  symbol : TZintSymbol;
  rt  : TZintRenderTargetWMF;
  wmf : TMetaFile;
  data : String;
begin
  Result := false;
  AErrorCode := -1;
  if (not IsValidSEPACharSet(FName)) or
     (not IsValidSEPACharSet(FPurposeOfPayment)) or
     (not IsValidSEPACharSet(FInformation)) then
    exit;
  if not (Length(Trim(FName)) in [1..70])  then
  begin
    AErrorCode := -2;
    exit;
  end;
  if (Length(Trim(FIBAN)) = 0) then
  begin
    AErrorCode := -3;
    exit;
  end;
  if (FAmount <= 0) then
  begin
    AErrorCode := -4;
    exit;
  end;
  if Trim(FPurpose) <> '' then
  if Length(Trim(FPurpose)) <> 4 then
  begin
    AErrorCode := -5;
    exit;
  end;
  if (Trim(FReference) <> '') and (Trim(FPurposeOfPayment) <> '') then
  begin
    AErrorCode := -6;
    exit;
  end;
  if (Trim(FReference) <> '') and (not FReference.StartsWith('RF',true)) then
  begin
    AErrorCode := -7;
    exit;
  end;
  if (Trim(FReference) = '') then
  if not (Length(Trim(FPurposeOfPayment)) in [1..140])  then
  begin
    AErrorCode := -8;
    exit;
  end;
  wmf := TMetafile.Create;
  symbol :=TZintSymbol.Create(nil);
  rt := TZintRenderTargetWMF.Create(nil);
  try
  try
    data := FServiceTag +#10+
            GetVersionAsString+#10+
            GetCharacterSetAsString+#10+
            FIdentification+#10+
            FBIC+#10+
            GetNameAsString+#10+
            FIBAN+#10+
            GetAmountAsString+#10+
            FPurpose+#10+
            FReference+#10+
            GetPurposeOfPaymentAsString+#10+
            GetInformationAsString;

    symbol.SymbolType := zsQRCODE;
    symbol.QRCodeOptions.ECCLevel := qreLevelM;
    symbol.QRCodeOptions.Size := qrsAuto;
    symbol.input_mode := UNICODE_MODE;
    symbol.Encode(data, true);
    symbol.primary := StrToArrayOfChar('');

    wmf.SetSize(AWidth,AHeight);
    rt.HexagonScale:= 1;
    rt.RenderAdjustMode:=TZintRenderAdjustMode.ramScale;
    rt.ShowText := false;
    rt.Metafile := wmf;
    rt.MinModuleWidth := 0;
    rt.MaxModuleWidth := 0;
    Symbol.Render(rt);
    AImage := wmf;
    wmf := nil;
    AErrorCode := 0;
    Result := true;
  except
    on E:Exception do
    begin
      AErrorCode := -9;
    end;
  end;
  finally
    symbol.Free;
    rt.Free;
    if Assigned(wmf) then
      wmf.Free;
  end;
end;

function TZintEPCQR.GetVersionAsString: String;
begin
  case FVersion of
    epcqrVersion_2 : Result := '002';
    else Result := '001';
  end;
end;

function TZintEPCQR.IsValidSEPACharSet(const AValue: String): Boolean;
var
  i : Integer;
begin
  Result := true;
  for i := 1 to Length(AValue) do
  begin
    if CharInSet(AValue[i],['a'..'z']) then
      continue;
    if CharInSet(AValue[i],['A'..'Z']) then
      continue;
    if CharInSet(AValue[i],['0'..'9']) then
      continue;
    if CharInSet(AValue[i],['/', '?', ':', '(', ')', '.', ',', '''', '+', '-', ' ']) then
      continue;
    Result := false;
    break;
  end;
end;

procedure TZintEPCQR.Clear;
begin
  FServiceTag := 'BCD';
  FVersion := TZintEPCQR.TVersion.epcqrVersion_2;
  FCharacterSet := TZintEPCQR.TCharacterSet.epcqrCharacterSet_UTF8;
  FIdentification := 'SCT';
  FBIC := '';
  FName := '';
  FIBAN := '';
  FAmount := 0;
  FPurpose := '';
  FReference := '';
  FPurposeOfPayment := '';
  FInformation := '';
end;

procedure TZintEPCQR.SetAmount(const Value: Currency);
begin
  FAmount := Value;
end;

procedure TZintEPCQR.SetBIC(const Value: String);
begin
  FBIC := Value;
end;

procedure TZintEPCQR.SetCharacterSet(const Value: TZintEPCQR.TCharacterSet);
begin
  FCharacterSet := Value;
end;

procedure TZintEPCQR.SetIBAN(const Value: String);
begin
  FIBAN := Value;
end;

procedure TZintEPCQR.SetInformation(const Value: String);
begin
  FInformation := Value;
end;

procedure TZintEPCQR.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TZintEPCQR.SetPurpose(const Value: String);
begin
  FPurpose := Value;
end;

procedure TZintEPCQR.SetPurposeOfPayment(const Value: String);
begin
  FPurposeOfPayment := Value;
end;

procedure TZintEPCQR.SetReference(const Value: String);
begin
  FReference := Value;
end;

procedure TZintEPCQR.SetVersion(const Value: TZintEPCQR.TVersion);
begin
  FVersion := Value;
end;

end.

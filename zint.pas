unit zint;
{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0

  Status:
    3432bc9aff311f2aea40f0e9883abfe6564c080b complete

  Notes:
    - the code of library.c is implemented here as part of TZintSymbol
}

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}


interface

uses
  Classes,
  SysUtils;

{$IF declared(TEncoding)}
  {$DEFINE UseTEncoding}
{$IFEND}

const
  ZINT_ROWS_MAX = 178;
  ZINT_COLS_MAX = 178;
  DEFAULTVALUE_OPTION_1 = -1;
  DEFAULTVALUE_OPTION_2 = 0;
  DEFAULTVALUE_OPTION_3 = 928;

type
  {$IF not declared(TBytes)}
  TBytes = array of Byte;
  {$IFEND}
  TArrayOfByte = TBytes;
  TArrayOfInteger = array of Integer;
  TArrayOfCardinal = array of Cardinal;
  TArrayOfWord = array of Word;
  TArrayOfChar = array of Char;
  TArrayOfArrayOfChar = array of array of Char;
  TArrayOfSmallInt = array of SmallInt;

  TZintSymbology = (zsCODE11,
                    zsC25MATRIX,
                    zsC25INTER,
                    zsC25IATA,
                    zsC25LOGIC,
                    zsC25IND,
                    zsCODE39,
                    zsEXCODE39,
                    zsEANX,
                    zsEAN128,
                    zsCODABAR,
                    zsCODE128,
                    zsDPLEIT,
                    zsDPIDENT,
                    zsCODE16K,
                    zsCODE49,
                    zsCODE93,
                    zsFLAT,
                    zsRSS14,
                    zsRSS_LTD,
                    zsRSS_EXP,
                    zsTELEPEN,
                    zsUPCA,
                    zsUPCE,
                    zsPOSTNET,
                    zsMSI_PLESSEY,
                    zsFIM,
                    zsLOGMARS,
                    zsPHARMA,
                    zsPZN,
                    zsPHARMA_TWO,
                    zsPDF417,
                    zsPDF417TRUNC,
                    zsMAXICODE,
                    zsQRCODE,
                    zsCODE128B,
                    zsAUSPOST,
                    zsAUSREPLY,
                    zsAUSROUTE,
                    zsAUSREDIRECT,
                    zsISBNX,
                    zsRM4SCC,
                    zsDATAMATRIX,
                    zsEAN14,
                    zsCODABLOCKF,
                    zsNVE18,
                    zsJAPANPOST,
                    zsKOREAPOST,
                    zsRSS14STACK,
                    zsRSS14STACK_OMNI,
                    zsRSS_EXPSTACK,
                    zsPLANET,
                    zsMICROPDF417,
                    zsONECODE,
                    zsPLESSEY,
                    zsTELEPEN_NUM,
                    zsITF14,
                    zsKIX,
                    zsAZTEC,
                    zsDAFT,
                    zsMICROQR,
                    zsHIBC_128,
                    zsHIBC_39,
                    zsHIBC_DM,
                    zsHIBC_QR,
                    zsHIBC_PDF,
                    zsHIBC_MICPDF,
                    zsHIBC_BLOCKF,
                    zsHIBC_AZTEC,
                    zsAZRUNE,
                    zsCODE32,
                    zsEANX_CC,
                    zsEAN128_CC,
                    zsRSS14_CC,
                    zsRSS_LTD_CC,
                    zsRSS_EXP_CC,
                    zsUPCA_CC,
                    zsUPCE_CC,
                    zsRSS14STACK_CC,
                    zsRSS14_OMNI_CC,
                    zsRSS_EXPSTACK_CC,
                    zsCHANNEL,
                    zsCODEONE,
                    zsGRIDMATRIX);

  TZintCustomRenderTarget = class;
  TZintSymbol = class;

  TZintPersistent = class(TPersistent)
  protected
    FOwner : TPersistent;
    FOnChanged: TNotifyEvent;
    function GetOwner: TPersistent; override;
    procedure Changed; virtual; // raises FOnChanged and informs the owner if it is a TZintPersistent
  public
    constructor Create(AOwner : TPersistent); virtual;
    property Owner : TPersistent read FOwner;

    property OnChange : TNotifyEvent read FOnChanged write FOnChanged;
  end;

  { TCustomZintSymbolOptions }

  TCustomZintSymbolOptions = class(TZintPersistent)
  protected
    FSymbol : TZintSymbol;
    function GetBooleanOption(AIndex : Integer) : Boolean;
    procedure SetBooleanOption(AIndex : Integer; AValue : Boolean);
  public
    constructor Create(ASymbol : TZintSymbol); reintroduce; virtual;
  end;

  TmpCheckDigitType = (cdtNone, cdtMod10, cdtMod1010, cdtMod11, cdtMod1110);

  { TZintMSIPlessyOptions }

  TZintMSIPlessyOptions = class(TCustomZintSymbolOptions)
  private
    function GetCheckDigitType: TmpCheckDigitType;
    procedure SetCheckDigitType(AValue: TmpCheckDigitType);
  published
    property CheckDigitType : TmpCheckDigitType read GetCheckDigitType write SetCheckDigitType default cdtNone;
  end;

  { TZintExtCode39Options }

  TZintExtCode39Options = class(TCustomZintSymbolOptions)
  published
    property AddCheckDigit : Boolean index 2 read GetBooleanOption write SetBooleanOption default false;
  end;

  TCompositeType = (ctAuto, ctCC_A, ctCC_B, ctCC_C);

  { TZintCompositeOptions }

  TZintCompositeOptions = class(TCustomZintSymbolOptions)
  protected
    function GetCompositeType: TCompositeType;
    procedure SetCompositeType(AValue: TCompositeType);
  published
    property CompositeType : TCompositeType read GetCompositeType write SetCompositeType default ctAuto;
  end;

  TgmSize = (gmsAuto, gms18, gms30, gms42, gms54, gms66, gms78, gms90, gms102, gms114, gms126, gms138, gms150, gms162);
  TgmErrorCorrectCapacity = (gmeccAuto, gmecc10Percent, gmecc20Percent, gmecc30Percent, gmecc40Percent, gmecc50Percent);

  { TZintGridMatrixOptions }

  TZintGridMatrixOptions = class(TCustomZintSymbolOptions)
  protected
    function GetErrorCorrectionCapacity: TgmErrorCorrectCapacity;
    function GetSize: TgmSize;
    procedure SetErrorCorrectionCapacity(AValue: TgmErrorCorrectCapacity);
    procedure SetSize(AValue: TgmSize);
  published
    property Size : TgmSize read GetSize write SetSize default gmsAuto;
    property ErrorCorrectionCapacity : TgmErrorCorrectCapacity read GetErrorCorrectionCapacity write SetErrorCorrectionCapacity default gmeccAuto;
  end;

  TpdfCheckDigitCount = -1..8;
  TpdfColumns = 0..30;

  { TZintPDF417Options }

  TZintPDF417Options = class(TCustomZintSymbolOptions)
  protected
    function GetCheckDigitCount: TpdfCheckDigitCount;
    function GetColumns: TpdfColumns;
    procedure SetCheckDigitCount(AValue: TpdfCheckDigitCount);
    procedure SetColumns(AValue: TpdfColumns);
  published
    property CheckDigitCount : TpdfCheckDigitCount read GetCheckDigitCount write SetCheckDigitCount default -1;
    property Columns : TpdfColumns read GetColumns write SetColumns default 0;
  end;

  TatErrorCorrectCapacity = (ateccAuto, atecc10Percent, atecc23Percent, atecc36Percent, atecc50Percent);
  TatSize = (atsAuto, ats15Compact, ats19Compact, ats23Compact, ats27Compact, ats19, ats23, ats27, ats31, ats37, ats41, ats45, ats49, ats53, ats57, ats61, ats67, ats71, ats75, ats79, ats83, ats87, ats91, ats95, ats101, ats105, ats109, ats113, ats117, ats121, ats125, ats131, ats135, ats139, ats143, ats147, ats151);

  { TZintAztecOptions }

  TZintAztecOptions = class(TCustomZintSymbolOptions)
  protected
    function GetErrorCorrectCapacity: TatErrorCorrectCapacity;
    function GetSize: TatSize;
    procedure SetGetErrorCorrectCapacity(AValue: TatErrorCorrectCapacity);
    procedure SetSize(AValue: TatSize);
  published
    property ErrorCorrectCapacity : TatErrorCorrectCapacity read GetErrorCorrectCapacity write SetGetErrorCorrectCapacity;
    property Size : TatSize read GetSize write SetSize;
  end;

  TmcMode = (mcmAuto, mcmMode2, mcmMode3, mcmMode4, mcmMode5, mcmMode6);

  { TZintMaxicodeOptions }

  TZintMaxicodeOptions = class(TCustomZintSymbolOptions)
  protected
    function GetMode: TmcMode;
    procedure SetMode(AValue: TmcMode);
  published
    property Mode : TmcMode read GetMode write SetMode;
  end;

//  TdmSize = (dmsAuto, dms10x10, dms12x12, dms14x14, dms16x16, dms18x18, dms20x20, dms22x22, dms24x24, dms26x26, dms32x32, dms36x36, dms40x40, dms44x44, dms48x48, dms52x52, dms64x64, dms72x72, dms80x80, dms88x88, dms96x96, dms104x104, dms120x120, dms132x132, dms144x144, dms8x18, dms8x32, dms12x26, dms12x36, dms16x36, dms16x48);
  TdmSize = (dmsAuto, dms10x10, dms12x12, dms14x14, dms16x16, dms18x18, dms20x20, dms22x22, dms24x24, dms26x26, dms32x32, dms36x36, dms40x40, dms44x44, dms48x48, dms52x52, dms64x64, dms72x72, dms80x80, dms88x88, dms96x96, dms104x104, dms120x120, dms132x132, dms144x144,
              dmr8x18, dmr8x32, dmr12x26, dmr12x36, dmr16x36, dmr16x48,
              dmre8x48, dmre8x64, dmre12x64, dmre16x64, dmre24x48, dmre24x64, dmre26x40, dmre26x48, dmre26x64);

  { TZintDatamatrixOptions }

  TZintDatamatrixOptions = class(TCustomZintSymbolOptions)
  protected
    function GetForceSquare: Boolean;
    function GetSize: TdmSize;
    procedure SetForceSquare(AValue: Boolean);
    procedure SetSize(AValue: TdmSize);
  published
    property Size : TdmSize read GetSize write SetSize default dmsAuto;
    property ForceSquare : Boolean read GetForceSquare write SetForceSquare default false;
  end;

  TqrECCLevel = (qreAuto, qreLevelL, qreLevelM, qreLevelQ, qreLevelH);
  TqrSize = (qrsAuto, qrs21, qrs25, qrs29, qrs33, qrs37, qrs41, qrs45, qrs49, qrs53, qrs57, qrs61, qrs65, qrs69, qrs73, qrs77, qrs81, qrs85, qrs89, qrs93, qrs97, qrs101, qrs105, qrs109, qrs113, qrs117, qrs121, qrs125, qrs129, qrs133, qrs137, qrs141, qrs145, qrs149, qrs153, qrs157, qrs161, qrs165, qrs169, qrs173, qrs177);

  { TZintQRCodeOptions }

  TZintQRCodeOptions = class(TCustomZintSymbolOptions)
  private
    function GetECCLevel: TqrECCLevel;
    function GetSize: TqrSize;
    procedure SetECCLevel(AValue: TqrECCLevel);
    procedure SetSize(AValue: TqrSize);
  published
    property ECCLevel : TqrECCLevel read GetECCLevel write SetECCLevel default qreAuto;
    property Size : TqrSize read GetSize write SetSize default qrsAuto;
  end;

  TmqVersion = (mqvAuto, mqv1, mqv2, mqv3, mqv4);
  TmqECCLevel = (mqeAuto, mqeL, mqeM, mqeQ, mqeH);

  { TZintMicroQROptions }

  TZintMicroQROptions = class(TCustomZintSymbolOptions)
  private
    function GetVersion: TmqVersion;
    procedure SetVersion(AValue: TmqVersion);
    function GetECCLevel: TmqECCLevel;
    procedure SetECCLevel(const AValue: TmqECCLevel);
  published
    property ECCLevel : TmqECCLevel read GetECCLevel write SetECCLevel default mqeAuto;
    property Version : TmqVersion read GetVersion write SetVersion default mqvAuto;
  end;

  Tc1Version = (c1vAuto, c1vA, c1vB, c1vC, c1vD, c1vE, c1vF, c1vG, c1vH, c1vS);

  { TZintCode1Options }

  TZintCode1Options = class(TCustomZintSymbolOptions)
  private
    function GetVersion: Tc1Version;
    procedure SetVersion(AValue: Tc1Version);
  published
    property Version : Tc1Version read GetVersion write SetVersion;
  end;

  { TZintSymbol }

  TZintSymbol = class(TZintPersistent)
  protected
    FMSIPlesseyOptions: TZintMSIPlessyOptions;
    FExtCode39Options: TZintExtCode39Options;
    FCompositeOptions : TZintCompositeOptions;
    FGridMatrixOptions : TZintGridMatrixOptions;
    FPDF417Options : TZintPDF417Options;
    FAztecOptions : TZintAztecOptions;
    FMaxicodeOptions : TZintMaxicodeOptions;
    FDatamatrixOptions : TZintDatamatrixOptions;
    FMicroQROptions : TZintMicroQROptions;
    FCode1Options : TZintCode1Options;
    FQRCodeOptions : TZintQRCodeOptions;

    function GetSymbology: TZintSymbology; virtual;
    procedure SetSymbology(const Value: TZintSymbology); virtual;

    procedure DefineProperties(Filer : TFiler); override;
    procedure LoadOption1(Reader : TReader); procedure SaveOption1(Writer : TWriter);
    procedure LoadOption2(Reader : TReader); procedure SaveOption2(Writer : TWriter);
    procedure LoadOption3(Reader : TReader); procedure SaveOption3(Writer : TWriter);
  public
    //please use the following vars *ONLY* if you *REALLY* know, what you're doing
    //otherwise use the properties of the RenderTarget or the TZintSymbol.???Options - properties
    symbology : Integer;
    whitespace_width : Integer;
    border_width : Integer;
    output_options : Integer;
    option_1 : Integer;
    option_2 : Integer;
    option_3 : Integer;
    input_mode : Integer;
    text : TArrayOfByte;
    rows : Integer;
    width : Integer;
    primary : TArrayOfChar;
    errtxt : TArrayOfChar;
    encoded_data : array[0..ZINT_ROWS_MAX - 1] of array[0..ZINT_COLS_MAX - 1] of Byte;
    row_height : array[0..ZINT_ROWS_MAX - 1] of Integer; { Largest symbol is 177x177 QR Code }

    constructor Create(AOwner : TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source : TPersistent); override;

    procedure Clear; virtual;

    procedure Encode(AData : TArrayOfByte; ALength : Integer; ARaiseExceptions : Boolean = true); overload; virtual;
    procedure Encode(AData : String; ARaiseExceptions : Boolean = true); overload; virtual;
    procedure Render(ATarget : TZintCustomRenderTarget); virtual;
  published
    property SymbolType : TZintSymbology read GetSymbology write SetSymbology;
    property MSIPlesseyOptions : TZintMSIPlessyOptions read FMSIPlesseyOptions;
    property ExtCode39Options : TZintExtCode39Options read FExtCode39Options;
    property CompositeOptions : TZintCompositeOptions read FCompositeOptions;
    property GridMatrixOptions : TZintGridMatrixOptions read FGridMatrixOptions;
    property PDF417Options : TZintPDF417Options read FPDF417Options;
    property AztecOptions : TZintAztecOptions read FAztecOptions;
    property MaxiCodeOptions : TZintMaxicodeOptions read FMaxicodeOptions;
    property DatamatrixOptions : TZintDatamatrixOptions read FDatamatrixOptions;
    property MicroQROptions : TZintMicroQROptions read FMicroQROptions;
    property Code1Option : TZintCode1Options read FCode1Options;
    property QRCodeOptions : TZintQRCodeOptions read FQRCodeOptions;
  end;

  zint_symbol = TZintSymbol;

  TZintRenderAdjustMode = (ramScale, ramInflate);

  { TZintRenderValue }

  TZintRenderValue = class(TZintPersistent)
  protected
    FTargetUnits : Single; //depends on the target; may be pixels, ...
    FModules : Single; //will be used as multiplicator with the module[height|width]

    procedure SetValue(const Index: Integer; const Value: Single); virtual;

    //these are helpers for internal use
    procedure IncTargetUnits(AValue : Single);
    procedure IncModules(AValue : Single);
    procedure DecTargetUnits(AValue : Single);
    procedure DecModules(AValue : Single);
  public
    constructor Create(AOwner : TPersistent); override;
    procedure Assign(Source : TPersistent); override;
  published
    property TargetUnits : Single index 0 read FTargetUnits write SetValue;
    property Modules : Single index 1 read FModules write SetValue;
  end;

  { TZintRenderBox }

  TZintRenderBox = class(TZintPersistent)
  protected
    FTop, FBottom, FLeft, FRight : TZintRenderValue;

    function GetSum(AIndex : Integer) : Single;
    procedure SetValue(const Index: Integer; const Value: TZintRenderValue); virtual;
  public
    constructor Create(AOwner : TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;

    procedure SetModules(AValue : Single); virtual;
    procedure SetTargetUnits(AValue : Single); virtual;
    function GetModules: Single; virtual;
    function GetTargetUnits: Single; virtual;
    procedure AddModulesToTargetUnits(AModuleWidth, AModuleHeight : Single;
                                      ATop : Boolean = true;
                                      ABottom : Boolean = true;
                                      ALeft : Boolean = true;
                                      ARight : Boolean = true); virtual;
    procedure RemoveModulesFromTargetUnits(AModuleWidth, AModuleHeight : Single;
                                           ATop : Boolean = true;
                                           ABottom : Boolean = true;
                                           ALeft : Boolean = true;
                                           ARight : Boolean = true); virtual;

    property TopAndBottomTargetUnits : Single index 0 read GetSum;
    property LeftAndRightTargetUnits : Single index 1 read GetSum;
    property TopAndBottomModules : Single index 2 read GetSum;
    property LeftAndRightModules : Single index 3 read GetSum;

  published
    property Top : TZintRenderValue     index 0 read FTop write SetValue;
    property Bottom : TZintRenderValue  index 1 read FBottom write SetValue;
    property Left : TZintRenderValue    index 2 read FLeft write SetValue;
    property Right : TZintRenderValue   index 3 read FRight write SetValue;

    property Modules : Single read GetModules write SetModules stored false;
    property TargetUnits : Single read GetTargetUnits write SetTargetUnits stored false;
  end;

  TZintRenderRect = record
    X, Y, Width, Height : Single;
  end;

  TZintHAlign = (haLeft, haCenter, haRight);
  TZintVAlign = (vaTop, vaCenter, vaBottom);

  TZintClearBackgroundParams = TZintRenderRect;

  TZintDrawRectParams = TZintRenderRect;

  TZintDrawHexagonParams = TZintRenderRect;

  TZintDrawRingParams = record
    X, Y, OuterRadius, InnerRadius : Single;
  end;

  TZintDrawTextParams = record
    X, Y, Width, Height : Single;
    Text : String;
  end;

  TZintCalcTextHeightParams = record
    Text : String;
  end;

  TZintCalcTextWidthParams = TZintCalcTextHeightParams;

  TZintEANUPCFlag = (euEAN8, euEAN13, euUPCA, euUPCE, euAddon2, euAddon5);
  TZintEANUPCFlags = set of TZintEANUPCFlag;

  { TZintCustomRenderTarget }

  TZintCustomRenderTarget = class(TZintPersistent)
  protected
    FSymbol : TZintSymbol;
    FRowHeights : Integer; //sum of all rowheights measured in modules
    FModuleWidth, FModuleHeight : Single;
    FLargeBarCount : Integer; //count of rows, which height should be maximied
    FLargeBarHeight : Single; //barheight of the rows, which height should be maximied
    FTextSpacing : TZintRenderBox;
    FHasText, FHasAddonText : Boolean;
    FText, FAddonText, FLeadingText, FTrailingText : String;
    FWhitespace : TZintRenderBox;
    FMargin, FPadding, FBorder: TZintRenderBox;
    FMarginRect, FBorderRect, FPaddingRect, FWhitespaceRect, FBarcodeRect, FTextSpacingRect, FTextRect : TZintRenderRect;
    FHexagonScale: Single;
    FTransparent: Boolean;
    FRenderAdjustMode : TZintRenderAdjustMode;
    FHeightDesired, FWidthDesired, FWidth, FHeight : Single;
    FYDesired, FXDesired, FY, FX : Single;
    FTextHeight    : Single;
    FMinModuleWidth,
    FMaxModuleWidth: Single;
    FHAlign : TZintHAlign;
    FVAlign : TZintVAlign;
    FStartTextBar : TZintRenderRect;
    FTextDone : Boolean;
    FEANUPCFlags : TZintEANUPCFlags;
    FShowText : Boolean;
    FLeadingTextWidth, FTrailingTextWidth : Single;

    procedure SetBox(const Index: Integer; const Value: TZintRenderBox); virtual;
    procedure SetHAlign(const Value: TZintHAlign); virtual;
    procedure SetHexagonScale(const Value: Single); virtual;
    procedure SetMaxModuleWidth(AValue: Single); virtual;
    procedure SetMinModuleWidth(const Value: Single); virtual;
    procedure SetRenderAdjustMode(const Value: TZintRenderAdjustMode); virtual;
    procedure SetShowText(const Value: Boolean); virtual;
    procedure SetTransparent(const Value: Boolean); virtual;
    procedure SetVAlign(const Value: TZintVAlign); virtual;

    //these functions calculates the zero-based values to absolute values based on the ...Desired-Values and FWidth & FHeight
    function CalcX(AValue : Single) : Single;
    function CalcY(AValue : Single) : Single;

    procedure AddSymbolOptions; virtual; //adds options from the symbol to this render target (border, whitespace, ...)
    procedure RemoveSymbolOptions; virtual; //removes options from this render target previously added by AddSymbolOptions
    procedure AddBoxModulesToTargetUnits; virtual;
    procedure RemoveBoxModulesFromTargetUnits; virtual;
    procedure FetchRowInfos; virtual; //search for large bars and sum up the heights of the rows
    procedure CalcSize; virtual;
    procedure CalcText; virtual;
    procedure CalcTextEANUPC; virtual;
    procedure CheckEANUPC; virtual;
    procedure CalcLargeBarHeight; virtual;
    procedure CalcBoxes; virtual;
    procedure DrawBorder; virtual;
    procedure DrawMaxiRings; virtual;
    procedure DrawMaxiModules; virtual;
    procedure DrawModules; virtual;
    procedure DrawTexts; virtual;
    procedure RenderStart; virtual;
    procedure RenderStop; virtual;
    procedure DrawStart; virtual;
    procedure DrawStop; virtual;
    procedure HandleSpecialBarsEANUPC(ABarIndex : Integer; var ABar : TZintDrawRectParams); virtual;
    procedure Inflate(const ANewWidth, ANewHeight : Single); virtual; abstract;
    procedure ClearBackground(const AParams : TZintClearBackgroundParams); virtual; abstract;
    procedure DrawRect(const AParams : TZintDrawRectParams); virtual; abstract;
    procedure DrawHexagon(const AParams : TZintDrawHexagonParams); virtual; abstract;
    procedure DrawRing(const AParams : TZintDrawRingParams); virtual; abstract;
    procedure DrawText(const AParams : TZintDrawTextParams); virtual; abstract;
    function CalcTextHeight(const AParams : TZintCalcTextHeightParams) : Single; virtual; abstract;
    function CalcTextWidth(const AParams : TZintCalcTextWidthParams) : Single; virtual; abstract;
  public
    constructor Create(AOwner : TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source : TPersistent); override;

    procedure Render(ASymbol : TZintSymbol); virtual;

    property XDesired: Single read FXDesired write FXDesired;
    property YDesired: Single read FYDesired write FYDesired;
    property HeightDesired: Single read FHeightDesired write FHeightDesired;
    property WidthDesired: Single read FWidthDesired write FWidthDesired;

    property Y : Single read FY;
    property X : Single read FX;
    property Height : Single read FHeight;
    property Width : Single read FWidth;
  published
    property RenderAdjustMode : TZintRenderAdjustMode read FRenderAdjustMode write SetRenderAdjustMode default ramScale;
    property Transparent : Boolean read FTransparent write SetTransparent default false;
    property HexagonScale : Single read FHexagonScale write SetHexagonScale;
    property Margin : TZintRenderBox      index 0 read FMargin write SetBox;
    property Padding : TZintRenderBox     index 1 read FPadding write SetBox;
    property Border : TZintRenderBox      index 2 read FBorder write SetBox;
    property Whitespace : TZintRenderBox  index 3 read FWhitespace write SetBox;
    property TextSpacing : TZintRenderBox index 4 read FTextSpacing write SetBox;
    property HAlign : TZintHAlign read FHAlign write SetHAlign default haLeft;
    property VAlign : TZintVAlign read FVAlign write SetVAlign default vaTop;
    property MinModuleWidth : Single read FMinModuleWidth write SetMinModuleWidth; //will only be applied if RenderAdjustMode = ramInflate
    property MaxModuleWidth : Single read FMaxModuleWidth write SetMaxModuleWidth;
    property ShowText : Boolean read FShowText write SetShowText default true;
  end;

const
  BARCODE_CODE11 = 1;
  BARCODE_C25MATRIX = 2;
  BARCODE_C25INTER = 3;
  BARCODE_C25IATA = 4;
  BARCODE_C25LOGIC = 6;
  BARCODE_C25IND = 7;
  BARCODE_CODE39 = 8;
  BARCODE_EXCODE39 = 9;
  BARCODE_EANX = 13;
  BARCODE_EAN128 = 16;
  BARCODE_CODABAR = 18;
  BARCODE_CODE128 = 20;
  BARCODE_DPLEIT = 21;
  BARCODE_DPIDENT = 22;
  BARCODE_CODE16K = 23;
  BARCODE_CODE49 = 24;
  BARCODE_CODE93 = 25;
  BARCODE_FLAT = 28;
  BARCODE_RSS14 = 29;
  BARCODE_RSS_LTD = 30;
  BARCODE_RSS_EXP = 31;
  BARCODE_TELEPEN = 32;
  BARCODE_UPCA = 34;
  BARCODE_UPCE = 37;
  BARCODE_POSTNET = 40;
  BARCODE_MSI_PLESSEY = 47;
  BARCODE_FIM = 49;
  BARCODE_LOGMARS = 50;
  BARCODE_PHARMA = 51;
  BARCODE_PZN = 52;
  BARCODE_PHARMA_TWO = 53;
  BARCODE_PDF417 = 55;
  BARCODE_PDF417TRUNC = 56;
  BARCODE_MAXICODE = 57;
  BARCODE_QRCODE = 58;
  BARCODE_CODE128B = 60;
  BARCODE_AUSPOST = 63;
  BARCODE_AUSREPLY = 66;
  BARCODE_AUSROUTE = 67;
  BARCODE_AUSREDIRECT = 68;
  BARCODE_ISBNX = 69;
  BARCODE_RM4SCC = 70;
  BARCODE_DATAMATRIX = 71;
  BARCODE_EAN14 = 72;
  BARCODE_CODABLOCKF = 74;
  BARCODE_NVE18 = 75;
  BARCODE_JAPANPOST = 76;
  BARCODE_KOREAPOST = 77;
  BARCODE_RSS14STACK = 79;
  BARCODE_RSS14STACK_OMNI = 80;
  BARCODE_RSS_EXPSTACK = 81;
  BARCODE_PLANET = 82;
  BARCODE_MICROPDF417 = 84;
  BARCODE_ONECODE = 85;
  BARCODE_PLESSEY = 86;

{ Tbarcode 8 codes  }
  BARCODE_TELEPEN_NUM = 87;
  BARCODE_ITF14 = 89;
  BARCODE_KIX = 90;
  BARCODE_AZTEC = 92;
  BARCODE_DAFT = 93;
  BARCODE_MICROQR = 97;

{ Tbarcode 9 codes  }
  BARCODE_HIBC_128 = 98;
  BARCODE_HIBC_39 = 99;
  BARCODE_HIBC_DM = 102;
  BARCODE_HIBC_QR = 104;
  BARCODE_HIBC_PDF = 106;
  BARCODE_HIBC_MICPDF = 108;
  BARCODE_HIBC_BLOCKF = 110;
  BARCODE_HIBC_AZTEC = 112;

{ Zint specific  }
  BARCODE_AZRUNE = 128;
  BARCODE_CODE32 = 129;
  BARCODE_EANX_CC = 130;
  BARCODE_EAN128_CC = 131;
  BARCODE_RSS14_CC = 132;
  BARCODE_RSS_LTD_CC = 133;
  BARCODE_RSS_EXP_CC = 134;
  BARCODE_UPCA_CC = 135;
  BARCODE_UPCE_CC = 136;
  BARCODE_RSS14STACK_CC = 137;
  BARCODE_RSS14_OMNI_CC = 138;
  BARCODE_RSS_EXPSTACK_CC = 139;
  BARCODE_CHANNEL = 140;
  BARCODE_CODEONE = 141;
  BARCODE_GRIDMATRIX = 142;

type
  TZintSymbologyInfoEntry = record
    DisplayName : String;
    Symbology : TZintSymbology;
  end;

const
  ZintSymbologyInfos : array[0..83] of TZintSymbologyInfoEntry =
     ((DisplayName : 'Code 11'; Symbology : zsCODE11),
      (DisplayName : 'Standard Code 2 of 5'; Symbology : zsC25MATRIX),
      (DisplayName : 'Interleaved 2 of 5'; Symbology : zsC25INTER),
      (DisplayName : 'Code 2 of 5 IATA'; Symbology : zsC25IATA),
      (DisplayName : 'Code 2 of 5 Data Logic'; Symbology : zsC25LOGIC),
      (DisplayName : 'Code 2 of 5 Industrial'; Symbology : zsC25IND),
      (DisplayName : 'Code 3 of 9 (Code 39)'; Symbology : zsCODE39),
      (DisplayName : 'Extended Code 3 of 9 (Code 39+)'; Symbology : zsEXCODE39),
      (DisplayName : 'EAN'; Symbology : zsEANX),
      (DisplayName : 'GS1-128 (UCC.EAN-128)'; Symbology : zsEAN128),
      (DisplayName : 'Codabar'; Symbology : zsCODABAR),
      (DisplayName : 'Code 128 (automatic subset switching)'; Symbology : zsCODE128),
      (DisplayName : 'Deutsche Post Leitcode'; Symbology : zsDPLEIT),
      (DisplayName : 'Deutsche Post Identcode'; Symbology : zsDPIDENT),
      (DisplayName : 'Code 16K'; Symbology : zsCODE16K),
      (DisplayName : 'Code 49'; Symbology : zsCODE49),
      (DisplayName : 'Code 93'; Symbology : zsCODE93),
      (DisplayName : 'Flattermarken'; Symbology : zsFLAT),
      (DisplayName : 'GS1 DataBar-14'; Symbology : zsRSS14),
      (DisplayName : 'GS1 DataBar Limited'; Symbology : zsRSS_LTD),
      (DisplayName : 'GS1 DataBar Extended'; Symbology : zsRSS_EXP),
      (DisplayName : 'Telepen Alpha'; Symbology : zsTELEPEN),
      (DisplayName : 'UPC A'; Symbology : zsUPCA),
      (DisplayName : 'UPC E'; Symbology : zsUPCE),
      (DisplayName : 'PostNet'; Symbology : zsPOSTNET),
      (DisplayName : 'MSI Plessey'; Symbology : zsMSI_PLESSEY),
      (DisplayName : 'FIM'; Symbology : zsFIM),
      (DisplayName : 'LOGMARS'; Symbology : zsLOGMARS),
      (DisplayName : 'Pharmacode One-Track'; Symbology : zsPHARMA),
      (DisplayName : 'PZN'; Symbology : zsPZN),
      (DisplayName : 'Pharmacode Two-Track'; Symbology : zsPHARMA_TWO),
      (DisplayName : 'PDF417'; Symbology : zsPDF417),
      (DisplayName : 'PDF417 Truncated'; Symbology : zsPDF417TRUNC),
      (DisplayName : 'Maxicode'; Symbology : zsMAXICODE),
      (DisplayName : 'QR Code'; Symbology : zsQRCODE),
      (DisplayName : 'Code 128 (Subset B)'; Symbology : zsCODE128B),
      (DisplayName : 'Australia Post Standard Customer'; Symbology : zsAUSPOST),
      (DisplayName : 'Australia Post Reply Paid'; Symbology : zsAUSREPLY),
      (DisplayName : 'Australia Post Routing'; Symbology : zsAUSROUTE),
      (DisplayName : 'Australia Post Redirection'; Symbology : zsAUSREDIRECT),
      (DisplayName : 'ISBN (EAN-13 with verification stage)'; Symbology : zsISBNX),
      (DisplayName : 'Royal Mail 4 State (RM4SCC)'; Symbology : zsRM4SCC),
      (DisplayName : 'Data Matrix'; Symbology : zsDATAMATRIX),
      (DisplayName : 'EAN-14'; Symbology : zsEAN14),
      (DisplayName : 'CODABLOCKF'; Symbology : zsCODABLOCKF),
      (DisplayName : 'NVE-18'; Symbology : zsNVE18),
      (DisplayName : 'Japanese Postal Code'; Symbology : zsJAPANPOST),
      (DisplayName : 'Korea Post'; Symbology : zsKOREAPOST),
      (DisplayName : 'GS1 DataBar-14 Stacked'; Symbology : zsRSS14STACK),
      (DisplayName : 'GS1 DataBar-14 Stacked Omnidirectional'; Symbology : zsRSS14STACK_OMNI),
      (DisplayName : 'GS1 DataBar Expanded Stacked'; Symbology : zsRSS_EXPSTACK),
      (DisplayName : 'PLANET'; Symbology : zsPLANET),
      (DisplayName : 'MicroPDF417'; Symbology : zsMICROPDF417),
      (DisplayName : 'USPS OneCode'; Symbology : zsONECODE),
      (DisplayName : 'Plessey Code'; Symbology : zsPLESSEY),
      (DisplayName : 'Telepen Numeric'; Symbology : zsTELEPEN_NUM),
      (DisplayName : 'ITF-14'; Symbology : zsITF14),
      (DisplayName : 'Dutch Post KIX Code'; Symbology : zsKIX),
      (DisplayName : 'Aztec Code'; Symbology : zsAZTEC),
      (DisplayName : 'DAFT Code'; Symbology : zsDAFT),
      (DisplayName : 'Micro QR Code'; Symbology : zsMICROQR),
      (DisplayName : 'HIBC Code 128'; Symbology : zsHIBC_128),
      (DisplayName : 'HIBC Code 39'; Symbology : zsHIBC_39),
      (DisplayName : 'HIBC Data Matrix'; Symbology : zsHIBC_DM),
      (DisplayName : 'HIBC QR Code'; Symbology : zsHIBC_QR),
      (DisplayName : 'HIBC PDF417'; Symbology : zsHIBC_PDF),
      (DisplayName : 'HIBC MicroPDF417'; Symbology : zsHIBC_MICPDF),
      (DisplayName : 'HIBC_BLOCKF'; Symbology : zsHIBC_BLOCKF),
      (DisplayName : 'HIBC Aztec Code'; Symbology : zsHIBC_AZTEC),
      (DisplayName : 'Aztec Runes'; Symbology : zsAZRUNE),
      (DisplayName : 'Code 32'; Symbology : zsCODE32),
      (DisplayName : 'Composite Symbol with EAN linear component'; Symbology : zsEANX_CC),
      (DisplayName : 'Composite Symbol with GS1-128 linear component'; Symbology : zsEAN128_CC),
      (DisplayName : 'Composite Symbol with GS1 DataBar-14 linear component'; Symbology : zsRSS14_CC),
      (DisplayName : 'Composite Symbol with GS1 DataBar Limited component'; Symbology : zsRSS_LTD_CC),
      (DisplayName : 'Composite Symbol with GS1 DataBar Extended component'; Symbology : zsRSS_EXP_CC),
      (DisplayName : 'Composite Symbol with UPC A linear component'; Symbology : zsUPCA_CC),
      (DisplayName : 'Composite Symbol with UPC E linear component'; Symbology : zsUPCE_CC),
      (DisplayName : 'Composite Symbol with GS1 DataBar-14 Stacked component'; Symbology : zsRSS14STACK_CC),
      (DisplayName : 'Composite Symbol with GS1 DataBar-14 Stacked Omnidirectional component'; Symbology : zsRSS14_OMNI_CC),
      (DisplayName : 'Composite Symbol with GS1 DataBar Expanded Stacked component'; Symbology : zsRSS_EXPSTACK_CC),
      (DisplayName : 'Channel Code'; Symbology : zsCHANNEL),
      (DisplayName : 'Code One'; Symbology : zsCODEONE),
      (DisplayName : 'Grid Matrix'; Symbology : zsGRIDMATRIX));


  BARCODE_BIND = 2;
  BARCODE_BOX = 4;
  READER_INIT = 16;

  DATA_MODE = 0;
  UNICODE_MODE = 1;
  GS1_MODE = 2;
  KANJI_MODE = 3;
  SJIS_MODE = 4;

  DM_SQUARE = 100;
  DM_DMRE   = 101;

  ZWARN_INVALID_OPTION = 2;
  ZERROR_TOO_LONG = 5;
  ZERROR_INVALID_DATA = 6;
  ZERROR_INVALID_CHECK = 7;
  ZERROR_INVALID_OPTION = 8;
  ZERROR_ENCODING_PROBLEM = 9;

  //These are the functions from library.c
  function gs1_compliant(_symbology : Integer) : Integer;
  procedure error_tag(var error_string : TArrayOfChar; error_number : Integer);
  function hibc(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
  function extended_charset(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
  function reduced_charset(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
  function ZBarcode_Encode(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;

  function SymbologyToInt(ASymbology : TZintSymbology) : Integer;
  function IntToSymbology(ASymbology : Integer) : TZintSymbology;

implementation

uses zint_common, zint_helper, zint_dmatrix,
  zint_code128, zint_gs1, zint_2of5,
  zint_aztec, zint_qr, zint_upcean,
  zint_maxicode, zint_auspost, zint_code, zint_medical,
  zint_code16k, zint_code49, zint_pdf417, zint_composite, zint_gridmtx,
  zint_plessey, zint_code1, zint_telepen, zint_postal, zint_imail, zint_rss;

const
  TECHNETIUM = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%';

  EDesiredWithTooSmall = 'The desired width is too small.';
  EDesiredHeightTooSmall = 'The desired height is too small.';

function SymbologyToInt(ASymbology : TZintSymbology) : Integer;
begin
  case ASymbology of
    zsCODE11 : Result := BARCODE_CODE11;
    zsC25MATRIX : Result := BARCODE_C25MATRIX;
    zsC25INTER : Result := BARCODE_C25INTER;
    zsC25IATA : Result := BARCODE_C25IATA;
    zsC25LOGIC : Result := BARCODE_C25LOGIC;
    zsC25IND : Result := BARCODE_C25IND;
    zsCODE39 : Result := BARCODE_CODE39;
    zsEXCODE39 : Result := BARCODE_EXCODE39;
    zsEANX : Result := BARCODE_EANX;
    zsEAN128 : Result := BARCODE_EAN128;
    zsCODABAR : Result := BARCODE_CODABAR;
    zsCODE128 : Result := BARCODE_CODE128;
    zsDPLEIT : Result := BARCODE_DPLEIT;
    zsDPIDENT : Result := BARCODE_DPIDENT;
    zsCODE16K : Result := BARCODE_CODE16K;
    zsCODE49 : Result := BARCODE_CODE49;
    zsCODE93 : Result := BARCODE_CODE93;
    zsFLAT : Result := BARCODE_FLAT;
    zsRSS14 : Result := BARCODE_RSS14;
    zsRSS_LTD : Result := BARCODE_RSS_LTD;
    zsRSS_EXP : Result := BARCODE_RSS_EXP;
    zsTELEPEN : Result := BARCODE_TELEPEN;
    zsUPCA : Result := BARCODE_UPCA;
    zsUPCE : Result := BARCODE_UPCE;
    zsPOSTNET : Result := BARCODE_POSTNET;
    zsMSI_PLESSEY : Result := BARCODE_MSI_PLESSEY;
    zsFIM : Result := BARCODE_FIM;
    zsLOGMARS : Result := BARCODE_LOGMARS;
    zsPHARMA : Result := BARCODE_PHARMA;
    zsPZN : Result := BARCODE_PZN;
    zsPHARMA_TWO : Result := BARCODE_PHARMA_TWO;
    zsPDF417 : Result := BARCODE_PDF417;
    zsPDF417TRUNC : Result := BARCODE_PDF417TRUNC;
    zsMAXICODE : Result := BARCODE_MAXICODE;
    zsQRCODE : Result := BARCODE_QRCODE;
    zsCODE128B : Result := BARCODE_CODE128B;
    zsAUSPOST : Result := BARCODE_AUSPOST;
    zsAUSREPLY : Result := BARCODE_AUSREPLY;
    zsAUSROUTE : Result := BARCODE_AUSROUTE;
    zsAUSREDIRECT : Result := BARCODE_AUSREDIRECT;
    zsISBNX : Result := BARCODE_ISBNX;
    zsRM4SCC : Result := BARCODE_RM4SCC;
    zsDATAMATRIX : Result := BARCODE_DATAMATRIX;
    zsEAN14 : Result := BARCODE_EAN14;
    zsCODABLOCKF : Result := BARCODE_CODABLOCKF;
    zsNVE18 : Result := BARCODE_NVE18;
    zsJAPANPOST : Result := BARCODE_JAPANPOST;
    zsKOREAPOST : Result := BARCODE_KOREAPOST;
    zsRSS14STACK : Result := BARCODE_RSS14STACK;
    zsRSS14STACK_OMNI : Result := BARCODE_RSS14STACK_OMNI;
    zsRSS_EXPSTACK : Result := BARCODE_RSS_EXPSTACK;
    zsPLANET : Result := BARCODE_PLANET;
    zsMICROPDF417 : Result := BARCODE_MICROPDF417;
    zsONECODE : Result := BARCODE_ONECODE;
    zsPLESSEY : Result := BARCODE_PLESSEY;
    zsTELEPEN_NUM : Result := BARCODE_TELEPEN_NUM;
    zsITF14 : Result := BARCODE_ITF14;
    zsKIX : Result := BARCODE_KIX;
    zsAZTEC : Result := BARCODE_AZTEC;
    zsDAFT : Result := BARCODE_DAFT;
    zsMICROQR : Result := BARCODE_MICROQR;
    zsHIBC_128 : Result := BARCODE_HIBC_128;
    zsHIBC_39 : Result := BARCODE_HIBC_39;
    zsHIBC_DM : Result := BARCODE_HIBC_DM;
    zsHIBC_QR : Result := BARCODE_HIBC_QR;
    zsHIBC_PDF : Result := BARCODE_HIBC_PDF;
    zsHIBC_MICPDF : Result := BARCODE_HIBC_MICPDF;
    zsHIBC_BLOCKF : Result := BARCODE_HIBC_BLOCKF;
    zsHIBC_AZTEC : Result := BARCODE_HIBC_AZTEC;
    zsAZRUNE : Result := BARCODE_AZRUNE;
    zsCODE32 : Result := BARCODE_CODE32;
    zsEANX_CC : Result := BARCODE_EANX_CC;
    zsEAN128_CC : Result := BARCODE_EAN128_CC;
    zsRSS14_CC : Result := BARCODE_RSS14_CC;
    zsRSS_LTD_CC : Result := BARCODE_RSS_LTD_CC;
    zsRSS_EXP_CC : Result := BARCODE_RSS_EXP_CC;
    zsUPCA_CC : Result := BARCODE_UPCA_CC;
    zsUPCE_CC : Result := BARCODE_UPCE_CC;
    zsRSS14STACK_CC : Result := BARCODE_RSS14STACK_CC;
    zsRSS14_OMNI_CC : Result := BARCODE_RSS14_OMNI_CC;
    zsRSS_EXPSTACK_CC : Result := BARCODE_RSS_EXPSTACK_CC;
    zsCHANNEL : Result := BARCODE_CHANNEL;
    zsCODEONE : Result := BARCODE_CODEONE;
    zsGRIDMATRIX : Result := BARCODE_GRIDMATRIX;
  end;
end;

function IntToSymbology(ASymbology : Integer) : TZintSymbology;
begin
  case ASymbology of
    BARCODE_CODE11 : Result := zsCODE11;
    BARCODE_C25MATRIX : Result := zsC25MATRIX;
    BARCODE_C25INTER : Result := zsC25INTER;
    BARCODE_C25IATA : Result := zsC25IATA;
    BARCODE_C25LOGIC : Result := zsC25LOGIC;
    BARCODE_C25IND : Result := zsC25IND;
    BARCODE_CODE39 : Result := zsCODE39;
    BARCODE_EXCODE39 : Result := zsEXCODE39;
    BARCODE_EANX : Result := zsEANX;
    BARCODE_EAN128 : Result := zsEAN128;
    BARCODE_CODABAR : Result := zsCODABAR;
    BARCODE_CODE128 : Result := zsCODE128;
    BARCODE_DPLEIT : Result := zsDPLEIT;
    BARCODE_DPIDENT : Result := zsDPIDENT;
    BARCODE_CODE16K : Result := zsCODE16K;
    BARCODE_CODE49 : Result := zsCODE49;
    BARCODE_CODE93 : Result := zsCODE93;
    BARCODE_FLAT : Result := zsFLAT;
    BARCODE_RSS14 : Result := zsRSS14;
    BARCODE_RSS_LTD : Result := zsRSS_LTD;
    BARCODE_RSS_EXP : Result := zsRSS_EXP;
    BARCODE_TELEPEN : Result := zsTELEPEN;
    BARCODE_UPCA : Result := zsUPCA;
    BARCODE_UPCE : Result := zsUPCE;
    BARCODE_POSTNET : Result := zsPOSTNET;
    BARCODE_MSI_PLESSEY : Result := zsMSI_PLESSEY;
    BARCODE_FIM : Result := zsFIM;
    BARCODE_LOGMARS : Result := zsLOGMARS;
    BARCODE_PHARMA : Result := zsPHARMA;
    BARCODE_PZN : Result := zsPZN;
    BARCODE_PHARMA_TWO : Result := zsPHARMA_TWO;
    BARCODE_PDF417 : Result := zsPDF417;
    BARCODE_PDF417TRUNC : Result := zsPDF417TRUNC;
    BARCODE_MAXICODE : Result := zsMAXICODE;
    BARCODE_QRCODE : Result := zsQRCODE;
    BARCODE_CODE128B : Result := zsCODE128B;
    BARCODE_AUSPOST : Result := zsAUSPOST;
    BARCODE_AUSREPLY : Result := zsAUSREPLY;
    BARCODE_AUSROUTE : Result := zsAUSROUTE;
    BARCODE_AUSREDIRECT : Result := zsAUSREDIRECT;
    BARCODE_ISBNX : Result := zsISBNX;
    BARCODE_RM4SCC : Result := zsRM4SCC;
    BARCODE_DATAMATRIX : Result := zsDATAMATRIX;
    BARCODE_EAN14 : Result := zsEAN14;
    BARCODE_CODABLOCKF : Result := zsCODABLOCKF;
    BARCODE_NVE18 : Result := zsNVE18;
    BARCODE_JAPANPOST : Result := zsJAPANPOST;
    BARCODE_KOREAPOST : Result := zsKOREAPOST;
    BARCODE_RSS14STACK : Result := zsRSS14STACK;
    BARCODE_RSS14STACK_OMNI : Result := zsRSS14STACK_OMNI;
    BARCODE_RSS_EXPSTACK : Result := zsRSS_EXPSTACK;
    BARCODE_PLANET : Result := zsPLANET;
    BARCODE_MICROPDF417 : Result := zsMICROPDF417;
    BARCODE_ONECODE : Result := zsONECODE;
    BARCODE_PLESSEY : Result := zsPLESSEY;
    BARCODE_TELEPEN_NUM : Result := zsTELEPEN_NUM;
    BARCODE_ITF14 : Result := zsITF14;
    BARCODE_KIX : Result := zsKIX;
    BARCODE_AZTEC : Result := zsAZTEC;
    BARCODE_DAFT : Result := zsDAFT;
    BARCODE_MICROQR : Result := zsMICROQR;
    BARCODE_HIBC_128 : Result := zsHIBC_128;
    BARCODE_HIBC_39 : Result := zsHIBC_39;
    BARCODE_HIBC_DM : Result := zsHIBC_DM;
    BARCODE_HIBC_QR : Result := zsHIBC_QR;
    BARCODE_HIBC_PDF : Result := zsHIBC_PDF;
    BARCODE_HIBC_MICPDF : Result := zsHIBC_MICPDF;
    BARCODE_HIBC_BLOCKF : Result := zsHIBC_BLOCKF;
    BARCODE_HIBC_AZTEC : Result := zsHIBC_AZTEC;
    BARCODE_AZRUNE : Result := zsAZRUNE;
    BARCODE_CODE32 : Result := zsCODE32;
    BARCODE_EANX_CC : Result := zsEANX_CC;
    BARCODE_EAN128_CC : Result := zsEAN128_CC;
    BARCODE_RSS14_CC : Result := zsRSS14_CC;
    BARCODE_RSS_LTD_CC : Result := zsRSS_LTD_CC;
    BARCODE_RSS_EXP_CC : Result := zsRSS_EXP_CC;
    BARCODE_UPCA_CC : Result := zsUPCA_CC;
    BARCODE_UPCE_CC : Result := zsUPCE_CC;
    BARCODE_RSS14STACK_CC : Result := zsRSS14STACK_CC;
    BARCODE_RSS14_OMNI_CC : Result := zsRSS14_OMNI_CC;
    BARCODE_RSS_EXPSTACK_CC : Result := zsRSS_EXPSTACK_CC;
    BARCODE_CHANNEL : Result := zsCHANNEL;
    BARCODE_CODEONE : Result := zsCODEONE;
    BARCODE_GRIDMATRIX : Result := zsGRIDMATRIX;

  end;
end;

{ TZintRenderValue }

procedure TZintRenderValue.Assign(Source : TPersistent);
var
  SourceRV : TZintRenderValue;
begin
  if Source is TZintRenderValue then
  begin
    SourceRV := TZintRenderValue(Source);
    FModules := SourceRV.Modules;
    FTargetUnits := SourceRV.TargetUnits;
    Changed;
  end
  else
    inherited;
end;

constructor TZintRenderValue.Create(AOwner : TPersistent);
begin
  inherited;

  Modules := 0;
  TargetUnits := 0;
end;

procedure TZintRenderValue.IncTargetUnits(AValue: Single);
begin
  FTargetUnits := FTargetUnits + AValue;
end;

procedure TZintRenderValue.SetValue(const Index: Integer; const Value: Single);
begin
  case Index of
    0 : FTargetUnits := Value;
    1 : FModules := Value;
  end;
  Changed;
end;

procedure TZintRenderValue.IncModules(AValue: Single);
begin
  FModules := FModules + AValue;
end;

procedure TZintRenderValue.DecTargetUnits(AValue: Single);
begin
  FTargetUnits := FTargetUnits - AValue;
end;

procedure TZintRenderValue.DecModules(AValue: Single);
begin
  FModules := FModules - AValue;
end;

{ TZintRenderBox }

function TZintRenderBox.GetModules: Single;
begin
  Result := (GetSum(2) + GetSum(3)) / 4;
end;

function TZintRenderBox.GetSum(AIndex: Integer): Single;
begin
  case AIndex of
    0: Result := FTop.TargetUnits + FBottom.TargetUnits;
    1: Result := FLeft.TargetUnits + FRight.TargetUnits;
    2: Result := FTop.Modules + FBottom.Modules;
    3: Result := FLeft.Modules + FRight.Modules;
    else
      Result := 0;
  end;
end;

function TZintRenderBox.GetTargetUnits: Single;
begin
  Result := (GetSum(0) + GetSum(1)) / 4;
end;

procedure TZintRenderBox.Assign(Source: TPersistent);
var
  SourceRB : TZintRenderBox;
begin
  if Source is TZintRenderBox then
  begin
    SourceRB := TZintRenderBox(Source);
    FTop.Assign(SourceRB.Top);
    FBottom.Assign(SourceRB.Bottom);
    FLeft.Assign(SourceRB.Left);
    FRight.Assign(SourceRB.Right);
  end
  else
    inherited;
end;

constructor TZintRenderBox.Create(AOwner : TPersistent);
begin
  inherited;

  FTop := TZintRenderValue.Create(Self);
  FBottom := TZintRenderValue.Create(Self);
  FLeft := TZintRenderValue.Create(Self);
  FRight := TZintRenderValue.Create(Self);
end;

destructor TZintRenderBox.Destroy;
begin
  FTop.Free;
  FBottom.Free;
  FLeft.Free;
  FRight.Free;

  inherited;
end;

procedure TZintRenderBox.SetModules(AValue: Single);
begin
  Top.Modules := AValue;
  Bottom.Modules := AValue;
  Left.Modules := AValue;
  Right.Modules := AValue;
end;

procedure TZintRenderBox.SetTargetUnits(AValue: Single);
begin
  Top.TargetUnits := AValue;
  Bottom.TargetUnits := AValue;
  Left.TargetUnits := AValue;
  Right.TargetUnits := AValue;
end;

procedure TZintRenderBox.SetValue(const Index: Integer;
  const Value: TZintRenderValue);
begin
  case Index of
    0 : FTop.Assign(Value);
    1 : FBottom.Assign(Value);
    2 : FLeft.Assign(Value);
    3 : FRight.Assign(Value);
  end;
end;

procedure TZintRenderBox.AddModulesToTargetUnits(AModuleWidth,
  AModuleHeight: Single; ATop, ABottom, ALeft, ARight : Boolean);
begin
  //the modules stays untouched, because we need them later to rollback this action
  if ATop then Top.IncTargetUnits(Top.Modules * AModuleHeight);
  if ABottom then Bottom.IncTargetUnits(Bottom.Modules * AModuleHeight);
  if ALeft then Left.IncTargetUnits(Left.Modules * AModuleWidth);
  if ARight then Right.IncTargetUnits(Right.Modules * AModuleWidth);
end;

procedure TZintRenderBox.RemoveModulesFromTargetUnits(AModuleWidth,
  AModuleHeight: Single; ATop, ABottom, ALeft, ARight : Boolean);
begin
  //rollback what we've done in TransferFromModulesToTargetUnits
  if ATop then Top.DecTargetUnits(Top.Modules * AModuleHeight);
  if ABottom then Bottom.DecTargetUnits(Bottom.Modules * AModuleHeight);
  if ALeft then Left.DecTargetUnits(Left.Modules * AModuleWidth);
  if ARight then Right.DecTargetUnits(Right.Modules * AModuleWidth);
end;

{ TZintCode1Options }

function TZintCode1Options.GetVersion: Tc1Version;
begin
  case FSymbol.option_2 of
    1: Result := c1vA;
    2: Result := c1vB;
    3: Result := c1vC;
    4: Result := c1vD;
    5: Result := c1vE;
    6: Result := c1vF;
    7: Result := c1vG;
    8: Result := c1vH;
    9: Result := c1vS;
    else
      Result := c1vAuto;
  end;
end;

procedure TZintCode1Options.SetVersion(AValue: Tc1Version);
begin
  case AValue of
    c1vAuto : FSymbol.option_2 := DEFAULTVALUE_OPTION_2;
    c1vA : FSymbol.option_2 := 1;
    c1vB : FSymbol.option_2 := 2;
    c1vC : FSymbol.option_2 := 3;
    c1vD : FSymbol.option_2 := 4;
    c1vE : FSymbol.option_2 := 5;
    c1vF : FSymbol.option_2 := 6;
    c1vG : FSymbol.option_2 := 7;
    c1vH : FSymbol.option_2 := 8;
    c1vS : FSymbol.option_2 := 9;
  end;
  Changed;
end;

{ TZintMicroQROptions }

function TZintMicroQROptions.GetECCLevel: TmqECCLevel;
begin
  case FSymbol.option_1 of
    1: Result := mqeL;
    2: Result := mqeM;
    3: Result := mqeQ;
    4: Result := mqeH;
    else
      Result :=mqeAuto;
  end;
end;

function TZintMicroQROptions.GetVersion: TmqVersion;
begin
  case FSymbol.option_2 of
    1: Result := mqv1;
    2: Result := mqv2;
    3: Result := mqv3;
    4: Result := mqv4;
    else
      Result :=mqvAuto;
  end;
end;

procedure TZintMicroQROptions.SetECCLevel(const AValue: TmqECCLevel);
begin
  case AValue of
    mqeAuto : FSymbol.option_1 := DEFAULTVALUE_OPTION_1;
    mqeL : FSymbol.option_1 := 1;
    mqeM : FSymbol.option_1 := 2;
    mqeQ : FSymbol.option_1 := 3;
    mqeH : FSymbol.option_1 := 4;
  end;
  Changed;
end;

procedure TZintMicroQROptions.SetVersion(AValue: TmqVersion);
begin
  case AValue of
    mqvAuto : FSymbol.option_2 := DEFAULTVALUE_OPTION_2;
    mqv1 : FSymbol.option_2 := 1;
    mqv2 : FSymbol.option_2 := 2;
    mqv3 : FSymbol.option_2 := 3;
    mqv4 : FSymbol.option_2 := 4;
  end;
  Changed;
end;

{ TZintQRCodeOptions }

function TZintQRCodeOptions.GetECCLevel: TqrECCLevel;
begin
  case FSymbol.option_1 of
    1 : Result := qreLevelL;
    2 : Result := qreLevelM;
    3 : Result := qreLevelQ;
    4 : Result := qreLevelH;
    else
      Result := qreAuto;
  end;
end;

function TZintQRCodeOptions.GetSize: TqrSize;
begin
  case FSymbol.option_2 of
    1 : Result := qrs21;
    2 : Result := qrs25;
    3 : Result := qrs29;
    4 : Result := qrs33;
    5 : Result := qrs37;
    6 : Result := qrs41;
    7 : Result := qrs45;
    8 : Result := qrs49;
    9 : Result := qrs53;
    10 : Result := qrs57;
    11 : Result := qrs61;
    12 : Result := qrs65;
    13 : Result := qrs69;
    14 : Result := qrs73;
    15 : Result := qrs77;
    16 : Result := qrs81;
    17 : Result := qrs85;
    18 : Result := qrs89;
    19 : Result := qrs93;
    20 : Result := qrs97;
    21 : Result := qrs101;
    22 : Result := qrs105;
    23 : Result := qrs109;
    24 : Result := qrs113;
    25 : Result := qrs117;
    26 : Result := qrs121;
    27 : Result := qrs125;
    28 : Result := qrs129;
    29 : Result := qrs133;
    30 : Result := qrs137;
    31 : Result := qrs141;
    32 : Result := qrs145;
    33 : Result := qrs149;
    34 : Result := qrs153;
    35 : Result := qrs157;
    36 : Result := qrs161;
    37 : Result := qrs165;
    38 : Result := qrs169;
    39 : Result := qrs173;
    40 : Result := qrs177;
    else
      Result := qrsAuto;
  end;
end;

procedure TZintQRCodeOptions.SetECCLevel(AValue: TqrECCLevel);
begin
  case AValue of
    qreAuto : FSymbol.option_1 := DEFAULTVALUE_OPTION_1;
    qreLevelL : FSymbol.option_1 := 1;
    qreLevelM : FSymbol.option_1 := 2;
    qreLevelQ : FSymbol.option_1 := 3;
    qreLevelH : FSymbol.option_1 := 4;
  end;
  Changed;
end;

procedure TZintQRCodeOptions.SetSize(AValue: TqrSize);
begin
  case AValue of
    qrsAuto : FSymbol.option_2 := DEFAULTVALUE_OPTION_2;
    qrs21 : FSymbol.option_2 := 1;
    qrs25 : FSymbol.option_2 := 2;
    qrs29 : FSymbol.option_2 := 3;
    qrs33 : FSymbol.option_2 := 4;
    qrs37 : FSymbol.option_2 := 5;
    qrs41 : FSymbol.option_2 := 6;
    qrs45 : FSymbol.option_2 := 7;
    qrs49 : FSymbol.option_2 := 8;
    qrs53 : FSymbol.option_2 := 9;
    qrs57 : FSymbol.option_2 := 10;
    qrs61 : FSymbol.option_2 := 11;
    qrs65 : FSymbol.option_2 := 12;
    qrs69 : FSymbol.option_2 := 13;
    qrs73 : FSymbol.option_2 := 14;
    qrs77 : FSymbol.option_2 := 15;
    qrs81 : FSymbol.option_2 := 16;
    qrs85 : FSymbol.option_2 := 17;
    qrs89 : FSymbol.option_2 := 18;
    qrs93 : FSymbol.option_2 := 19;
    qrs97 : FSymbol.option_2 := 20;
    qrs101 : FSymbol.option_2 := 21;
    qrs105 : FSymbol.option_2 := 22;
    qrs109 : FSymbol.option_2 := 23;
    qrs113 : FSymbol.option_2 := 24;
    qrs117 : FSymbol.option_2 := 25;
    qrs121 : FSymbol.option_2 := 26;
    qrs125 : FSymbol.option_2 := 27;
    qrs129 : FSymbol.option_2 := 28;
    qrs133 : FSymbol.option_2 := 29;
    qrs137 : FSymbol.option_2 := 30;
    qrs141 : FSymbol.option_2 := 31;
    qrs145 : FSymbol.option_2 := 32;
    qrs149 : FSymbol.option_2 := 33;
    qrs153 : FSymbol.option_2 := 34;
    qrs157 : FSymbol.option_2 := 35;
    qrs161 : FSymbol.option_2 := 36;
    qrs165 : FSymbol.option_2 := 37;
    qrs169 : FSymbol.option_2 := 38;
    qrs173 : FSymbol.option_2 := 39;
    qrs177 : FSymbol.option_2 := 40;
  end;
  Changed;
end;

{ TZintDatamatrixOptions }

function TZintDatamatrixOptions.GetForceSquare: Boolean;
begin
  Result := FSymbol.option_3 = DM_SQUARE;
end;

function TZintDatamatrixOptions.GetSize: TdmSize;
begin
  case FSymbol.option_2 of
    1 : Result := dms10x10;
    2 : Result := dms12x12;
    3 : Result := dms14x14;
    4 : Result := dms16x16;
    5 : Result := dms18x18;
    6 : Result := dms20x20;
    7 : Result := dms22x22;
    8 : Result := dms24x24;
    9 : Result := dms26x26;
    10 : Result := dms32x32;
    11 : Result := dms36x36;
    12 : Result := dms40x40;
    13 : Result := dms44x44;
    14 : Result := dms48x48;
    15 : Result := dms52x52;
    16 : Result := dms64x64;
    17 : Result := dms72x72;
    18 : Result := dms80x80;
    19 : Result := dms88x88;
    20 : Result := dms96x96;
    21 : Result := dms104x104;
    22 : Result := dms120x120;
    23 : Result := dms132x132;
    24 : Result := dms144x144;
    25 : Result := dmr8x18;
    26 : Result := dmr8x32;
    27 : Result := dmr12x26;
    28 : Result := dmr12x36;
    29 : Result := dmr16x36;
    30 : Result := dmr16x48;
    31 : Result := dmre8x48;
    32 : Result := dmre8x64;
    33 : Result := dmre12x64;
    34 : Result := dmre16x64;
    35 : Result := dmre24x48;
    36 : Result := dmre24x64;
    37 : Result := dmre26x40;
    38 : Result := dmre26x48;
    39 : Result := dmre26x64;
    else
      Result := dmsAuto;
  end;
end;

procedure TZintDatamatrixOptions.SetForceSquare(AValue: Boolean);
begin
  if AValue then
    FSymbol.option_3 := DM_SQUARE
  else
    FSymbol.option_3 := DEFAULTVALUE_OPTION_3;
  Changed;
end;

procedure TZintDatamatrixOptions.SetSize(AValue: TdmSize);
begin
  case AValue of
    dmsAuto : FSymbol.option_2 := DEFAULTVALUE_OPTION_2;
    dms10x10 : FSymbol.option_2 := 1;
    dms12x12 : FSymbol.option_2 := 2;
    dms14x14 : FSymbol.option_2 := 3;
    dms16x16 : FSymbol.option_2 := 4;
    dms18x18 : FSymbol.option_2 := 5;
    dms20x20 : FSymbol.option_2 := 6;
    dms22x22 : FSymbol.option_2 := 7;
    dms24x24 : FSymbol.option_2 := 8;
    dms26x26 : FSymbol.option_2 := 9;
    dms32x32 : FSymbol.option_2 := 10;
    dms36x36 : FSymbol.option_2 := 11;
    dms40x40 : FSymbol.option_2 := 12;
    dms44x44 : FSymbol.option_2 := 13;
    dms48x48 : FSymbol.option_2 := 14;
    dms52x52 : FSymbol.option_2 := 15;
    dms64x64 : FSymbol.option_2 := 16;
    dms72x72 : FSymbol.option_2 := 17;
    dms80x80 : FSymbol.option_2 := 18;
    dms88x88 : FSymbol.option_2 := 19;
    dms96x96 : FSymbol.option_2 := 20;
    dms104x104 : FSymbol.option_2 := 21;
    dms120x120 : FSymbol.option_2 := 22;
    dms132x132 : FSymbol.option_2 := 23;
    dms144x144 : FSymbol.option_2 := 24;
    dmr8x18 : FSymbol.option_2 := 25;
    dmr8x32 : FSymbol.option_2 := 26;
    dmr12x26 : FSymbol.option_2 := 27;
    dmr12x36 : FSymbol.option_2 := 28;
    dmr16x36 : FSymbol.option_2 := 29;
    dmr16x48 : FSymbol.option_2 := 30;
    dmre8x48 : FSymbol.option_2 := 31;
    dmre8x64 : FSymbol.option_2 := 32;
    dmre12x64 : FSymbol.option_2 := 33;
    dmre16x64 : FSymbol.option_2 := 34;
    dmre24x48 : FSymbol.option_2 := 35;
    dmre24x64 : FSymbol.option_2 := 36;
    dmre26x40 : FSymbol.option_2 := 37;
    dmre26x48 : FSymbol.option_2 := 38;
    dmre26x64 : FSymbol.option_2 := 39;
  end;
  Changed;
end;

{ TZintMaxicodeOptions }

function TZintMaxicodeOptions.GetMode: TmcMode;
begin
  case FSymbol.option_1 of
    2 : Result := mcmMode2;
    3 : Result := mcmMode3;
    4 : Result := mcmMode4;
    5 : Result := mcmMode5;
    6 : Result := mcmMode6;
    else
      Result := mcmAuto;
  end;
end;

procedure TZintMaxicodeOptions.SetMode(AValue: TmcMode);
begin
  case AValue of
    mcmAuto : FSymbol.option_1 := DEFAULTVALUE_OPTION_1;
    mcmMode2 : FSymbol.option_1 := 2;
    mcmMode3 : FSymbol.option_1 := 3;
    mcmMode4 : FSymbol.option_1 := 4;
    mcmMode5 : FSymbol.option_1 := 5;
    mcmMode6 : FSymbol.option_1 := 6;
  end;
  Changed;
end;

{ TZintAztecOptions }

function TZintAztecOptions.GetErrorCorrectCapacity: TatErrorCorrectCapacity;
begin
  case FSymbol.option_1 of
    1 : Result := atecc10Percent;
    2 : Result := atecc23Percent;
    3 : Result := atecc36Percent;
    4 : Result := atecc50Percent;
    else
      Result := ateccAuto;
  end;
end;

function TZintAztecOptions.GetSize: TatSize;
begin
  case FSymbol.option_2 of
    1 : Result := ats15Compact;
    2 : Result := ats19Compact;
    3 : Result := ats23Compact;
    4 : Result := ats27Compact;
    5 : Result := ats19;
    6 : Result := ats23;
    7 : Result := ats27;
    8 : Result := ats31;
    9 : Result := ats37;
    10 : Result := ats41;
    11 : Result := ats45;
    12 : Result := ats49;
    13 : Result := ats53;
    14 : Result := ats57;
    15 : Result := ats61;
    16 : Result := ats67;
    17 : Result := ats71;
    18 : Result := ats75;
    19 : Result := ats79;
    20 : Result := ats83;
    21 : Result := ats87;
    22 : Result := ats91;
    23 : Result := ats95;
    24 : Result := ats101;
    25 : Result := ats105;
    26 : Result := ats109;
    27 : Result := ats113;
    28 : Result := ats117;
    29 : Result := ats121;
    30 : Result := ats125;
    31 : Result := ats131;
    32 : Result := ats135;
    33 : Result := ats139;
    34 : Result := ats143;
    35 : Result := ats147;
    36 : Result := ats151;
    else
      Result := atsAuto;
  end;
end;

procedure TZintAztecOptions.SetGetErrorCorrectCapacity(
  AValue: TatErrorCorrectCapacity);
begin
  case AValue of
    ateccAuto : FSymbol.option_1 := DEFAULTVALUE_OPTION_1;
    atecc10Percent : FSymbol.option_1 := 1;
    atecc23Percent : FSymbol.option_1 := 2;
    atecc36Percent : FSymbol.option_1 := 3;
    atecc50Percent : FSymbol.option_1 := 4;
  end;
  Changed;
end;

procedure TZintAztecOptions.SetSize(AValue: TatSize);
begin
  case AValue of
    atsAuto : FSymbol.option_2 := DEFAULTVALUE_OPTION_2;
    ats15Compact : FSymbol.option_2 := 1;
    ats19Compact : FSymbol.option_2 := 2;
    ats23Compact : FSymbol.option_2 := 3;
    ats27Compact : FSymbol.option_2 := 4;
    ats19 : FSymbol.option_2 := 5;
    ats23 : FSymbol.option_2 := 6;
    ats27 : FSymbol.option_2 := 7;
    ats31 : FSymbol.option_2 := 8;
    ats37 : FSymbol.option_2 := 9;
    ats41 : FSymbol.option_2 := 10;
    ats45 : FSymbol.option_2 := 11;
    ats49 : FSymbol.option_2 := 12;
    ats53 : FSymbol.option_2 := 13;
    ats57 : FSymbol.option_2 := 14;
    ats61 : FSymbol.option_2 := 15;
    ats67 : FSymbol.option_2 := 16;
    ats71 : FSymbol.option_2 := 17;
    ats75 : FSymbol.option_2 := 18;
    ats79 : FSymbol.option_2 := 19;
    ats83 : FSymbol.option_2 := 20;
    ats87 : FSymbol.option_2 := 21;
    ats91 : FSymbol.option_2 := 22;
    ats95 : FSymbol.option_2 := 23;
    ats101 : FSymbol.option_2 := 24;
    ats105 : FSymbol.option_2 := 25;
    ats109 : FSymbol.option_2 := 26;
    ats113 : FSymbol.option_2 := 27;
    ats117 : FSymbol.option_2 := 28;
    ats121 : FSymbol.option_2 := 29;
    ats125 : FSymbol.option_2 := 30;
    ats131 : FSymbol.option_2 := 31;
    ats135 : FSymbol.option_2 := 32;
    ats139 : FSymbol.option_2 := 33;
    ats143 : FSymbol.option_2 := 34;
    ats147 : FSymbol.option_2 := 35;
    ats151 : FSymbol.option_2 := 36;
  end;
  Changed;
end;

{ TZintPDF417Options }

function TZintPDF417Options.GetCheckDigitCount: TpdfCheckDigitCount;
begin
  if (FSymbol.option_1 >= Low(TpdfCheckDigitCount)) and (FSymbol.option_1 <= High(TpdfCheckDigitCount)) then
    Result := FSymbol.option_1
  else
    Result := Low(TpdfCheckDigitCount);
end;

function TZintPDF417Options.GetColumns: TpdfColumns;
begin
  if (FSymbol.option_2 >= Low(TpdfColumns)) and (FSymbol.option_2 <= High(TpdfColumns)) then
    Result := FSymbol.option_2
  else
    Result := Low(TpdfColumns);
end;

procedure TZintPDF417Options.SetCheckDigitCount(AValue: TpdfCheckDigitCount);
begin
  FSymbol.option_1 := AValue;
  Changed;
end;

procedure TZintPDF417Options.SetColumns(AValue: TpdfColumns);
begin
  FSymbol.option_2 := AValue;
  Changed;
end;

{ TZintGridMatrixOptions }

function TZintGridMatrixOptions.GetErrorCorrectionCapacity: TgmErrorCorrectCapacity;
begin
  case FSymbol.option_1 of
    1: Result := gmecc10Percent;
    2: Result := gmecc20Percent;
    3: Result := gmecc30Percent;
    4: Result := gmecc40Percent;
    5: Result := gmecc50Percent;
    else
      Result := gmeccAuto;
  end;
end;

function TZintGridMatrixOptions.GetSize: TgmSize;
begin
  case FSymbol.option_2 of
    1: Result := gms18;
    2: Result := gms30;
    3: Result := gms42;
    4: Result := gms54;
    5: Result := gms66;
    6: Result := gms78;
    7: Result := gms90;
    8: Result := gms102;
    9: Result := gms114;
    10: Result := gms126;
    11: Result := gms138;
    12: Result := gms150;
    13: Result := gms162
    else
      Result := gmsAuto;
  end;
end;

procedure TZintGridMatrixOptions.SetErrorCorrectionCapacity(
  AValue: TgmErrorCorrectCapacity);
begin
  case AValue of
    gmeccAuto : FSymbol.option_1 := DEFAULTVALUE_OPTION_1;
    gmecc10Percent : FSymbol.option_1 := 1;
    gmecc20Percent : FSymbol.option_1 := 2;
    gmecc30Percent : FSymbol.option_1 := 3;
    gmecc40Percent : FSymbol.option_1 := 4;
    gmecc50Percent : FSymbol.option_1 := 5;
  end;
  Changed;
end;

procedure TZintGridMatrixOptions.SetSize(AValue: TgmSize);
begin
  case AValue of
    gmsAuto : FSymbol.option_2 := DEFAULTVALUE_OPTION_2;
    gms18 : FSymbol.option_2 := 1;
    gms30 : FSymbol.option_2 := 2;
    gms42 : FSymbol.option_2 := 3;
    gms54 : FSymbol.option_2 := 4;
    gms66 : FSymbol.option_2 := 5;
    gms78 : FSymbol.option_2 := 6;
    gms90 : FSymbol.option_2 := 7;
    gms102 : FSymbol.option_2 := 8;
    gms114 : FSymbol.option_2 := 9;
    gms126 : FSymbol.option_2 := 10;
    gms138 : FSymbol.option_2 := 11;
    gms150 : FSymbol.option_2 := 12;
    gms162 : FSymbol.option_2 := 13;
  end;
  Changed;
end;

{ TZintCompositeOptions }

function TZintCompositeOptions.GetCompositeType: TCompositeType;
begin
  case FSymbol.option_1 of
    1: Result := ctCC_A;
    2: Result := ctCC_B;
    3: Result := ctCC_C;
    else
      Result := ctAuto;
  end;
end;

procedure TZintCompositeOptions.SetCompositeType(AValue: TCompositeType);
begin
  case AValue of
    ctAuto : FSymbol.option_1 := DEFAULTVALUE_OPTION_1;
    ctCC_A : FSymbol.option_1 := 1;
    ctCC_B : FSymbol.option_1 := 2;
    ctCC_C : FSymbol.option_1 := 3;
  end;
  Changed;
end;

{ TZintMSIPlessyOptions }

function TZintMSIPlessyOptions.GetCheckDigitType: TmpCheckDigitType;
begin
  case FSymbol.option_2 of
    0: Result := cdtNone;
    1: Result := cdtMod10;
    2: Result := cdtMod1010;
    3: Result := cdtMod11;
    4: Result := cdtMod1110;
    else
      Result := cdtNone;
  end;
end;

procedure TZintMSIPlessyOptions.SetCheckDigitType(AValue: TmpCheckDigitType);
begin
  case AValue of
    cdtNone : FSymbol.option_2 := DEFAULTVALUE_OPTION_2;
    cdtMod10 : FSymbol.option_2 := 1;
    cdtMod1010 : FSymbol.option_2 := 2;
    cdtMod11 : FSymbol.option_2 := 3;
    cdtMod1110 : FSymbol.option_2 := 4;
  end;
  Changed;
end;

{ TCustomZintSymbolOptions }

function TCustomZintSymbolOptions.GetBooleanOption(AIndex: Integer): Boolean;
begin
  case AIndex of
    1 : Result := FSymbol.option_1 <> 0;
    2 : Result := FSymbol.option_2 <> 0;
    3 : Result := FSymbol.option_3 <> 0;
    else
      Result := false;
  end;
end;

procedure TCustomZintSymbolOptions.SetBooleanOption(AIndex: Integer;
  AValue: Boolean);
var
  v : Integer;
begin
  if AValue then v := 1 else v := 0;

  case AIndex of
    1 :
    begin
      if FSymbol.option_1 <> v then
      begin
        FSymbol.option_1 := v;
        Changed;
      end;
    end;
    2 :
    begin
      if FSymbol.option_2 <> v then
      begin
        FSymbol.option_2 := v;
        Changed;
      end;
    end;
    3 :
    begin
      if FSymbol.option_3 <> v then
      begin
        FSymbol.option_3 := v;
        Changed;
      end;
    end;
  end;
end;

constructor TCustomZintSymbolOptions.Create(ASymbol: TZintSymbol);
begin
  inherited Create(ASymbol);

  FSymbol := ASymbol;
end;

{ TZintSymbol }

procedure TZintSymbol.Assign(Source: TPersistent);
var
  SourceZS : TZintSymbol;
begin
  if Source is TZintSymbol then
  begin
    SourceZS := TZintSymbol(Source);
    symbology := SourceZS.symbology;
    whitespace_width := SourceZS.whitespace_width;
    border_width := SourceZS.border_width;
    output_options := SourceZS.output_options;
    option_1 := SourceZS.option_1;
    option_2 := SourceZS.option_2;
    option_3 := SourceZS.option_3;
    input_mode := SourceZS.input_mode;
    text := SourceZS.text;
    rows := SourceZS.rows;
    width := SourceZS.width;
    primary := SourceZS.primary;
    errtxt := SourceZS.errtxt;
    encoded_data := SourceZS.encoded_data;
    row_height := SourceZS.row_height;

    Changed;
  end
  else
    inherited;
end;

procedure TZintSymbol.Clear;
var
  i, j : Integer;
begin
	for i := 0 to rows - 1 do
		for j := 0 to width - 1 do
			unset_module(Self, i, j);

  for i := Low(row_height) to High(row_height) do
    row_height[i] := 0;

	rows := 0;
	width := 0;
	ustrcpy(text, '');
	strcpy(errtxt, '');
end;

constructor TZintSymbol.Create(AOwner : TPersistent);
begin
  inherited;

  SetLength(text, 128);
  SetLength(primary, 128);
  SetLength(errtxt, 100);

  symbology := BARCODE_CODE128;
	whitespace_width := 0;
	border_width := 0;
  output_options := 0;
	rows := 0;
	width := 0;
	option_1 := -1;
	option_2 := 0;
	option_3 := 928; // PDF_MAX
	input_mode := DATA_MODE;
	strcpy(primary, '');
  ustrcpy(text, '');
  strcpy(errtxt, '');

  FMSIPlesseyOptions := TZintMSIPlessyOptions.Create(Self);
  FExtCode39Options := TZintExtCode39Options.Create(Self);
  FCompositeOptions := TZintCompositeOptions.Create(Self);
  FGridMatrixOptions := TZintGridMatrixOptions.Create(Self);
  FPDF417Options := TZintPDF417Options.Create(Self);
  FAztecOptions := TZintAztecOptions.Create(Self);
  FMaxicodeOptions := TZintMaxicodeOptions.Create(Self);
  FDatamatrixOptions := TZintDatamatrixOptions.Create(Self);
  FMicroQROptions := TZintMicroQROptions.Create(Self);
  FCode1Options := TZintCode1Options.Create(Self);
  FQRCodeOptions := TZintQRCodeOptions.Create(Self);
end;

procedure TZintSymbol.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('option_1', {$IFDEF FPC}@{$ENDIF}LoadOption1, {$IFDEF FPC}@{$ENDIF}SaveOption1, option_1 <> DEFAULTVALUE_OPTION_1);
  Filer.DefineProperty('option_2', {$IFDEF FPC}@{$ENDIF}LoadOption2, {$IFDEF FPC}@{$ENDIF}SaveOption2, option_2 <> DEFAULTVALUE_OPTION_2);
  Filer.DefineProperty('option_3', {$IFDEF FPC}@{$ENDIF}LoadOption3, {$IFDEF FPC}@{$ENDIF}SaveOption3, option_3 <> DEFAULTVALUE_OPTION_3);
end;

destructor TZintSymbol.Destroy;
begin
  Clear;

  FMSIPlesseyOptions.Free;
  FExtCode39Options.Free;
  FCompositeOptions.Free;
  FGridMatrixOptions.Free;
  FPDF417Options.Free;
  FAztecOptions.Free;
  FMaxicodeOptions.Free;
  FDatamatrixOptions.Free;
  FMicroQROptions.Free;
  FCode1Options.Free;
  FQRCodeOptions.Free;
  
  inherited;
end;

procedure TZintSymbol.Encode(AData: TArrayOfByte; ALength : Integer; ARaiseExceptions : Boolean);
begin
  if (ZBarcode_Encode(Self, AData, ALength) >= ZERROR_TOO_LONG) then
  begin
    if ARaiseExceptions then
      raise Exception.Create(PChar(@self.errtxt[0]));
  end;
end;

procedure TZintSymbol.Encode(AData: String; ARaiseExceptions: Boolean);
var
  b : TArrayOfByte;

  {$IFDEF UseTEncoding}
  e : TEncoding;
  {$ENDIF}
begin
  if (input_mode and UNICODE_MODE) <> 0 then
  begin
  {$IFDEF UseTEncoding}
    {$IFDEF FPC}
    e := TEncoding.ANSI;
    {$ELSE}
    e := TEncoding.UTF8;
    {$ENDIF}
    b := e.GetBytes(AData);
    SetLength(b, Length(b) + 1);
    b[High(b)] := 0;
  {$ELSE}            
    b := StrToArrayOfByte(UTF8Encode(AData));
  {$ENDIF}
  end
  else
    b := StrToArrayOfByte(AData);

  Encode(b, ustrlen(b), ARaiseExceptions);
end;

function TZintSymbol.GetSymbology: TZintSymbology;
begin
  Result := IntToSymbology(symbology);
end;

procedure TZintSymbol.LoadOption1(Reader: TReader);
begin
  option_1 := Reader.ReadInteger;
end;

procedure TZintSymbol.LoadOption2(Reader: TReader);
begin
  option_2 := Reader.ReadInteger;
end;

procedure TZintSymbol.LoadOption3(Reader: TReader);
begin
  option_3 := Reader.ReadInteger;
end;

procedure error_tag(var error_string : TArrayOfChar; error_number : Integer);
var
  error_buffer : TArrayOfChar;
begin
  SetLength(error_buffer, 100);

  if (error_number <> 0) then
  begin
    strcpy(error_buffer, error_string);

    if (error_number > 4) then
      strcpy(error_string, 'error: ')
    else
      strcpy(error_string, 'warning: ');

    concat(error_string, error_buffer);
  end;
end;

function hibc(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  counter, error_number, i : Integer;
  to_process, temp : TArrayOfChar;
  check_digit : Char;
begin
  SetLength(to_process, 40); SetLength(temp, 2);

	if (_length > 36) then
  begin
		strcpy(symbol.errtxt, 'Data too long for HIBC LIC');
		result := ZERROR_TOO_LONG; exit;
	end;
	to_upper(source);
	error_number := is_sane(TECHNETIUM, source, _length);
	if (error_number = ZERROR_INVALID_DATA) then
  begin
		strcpy(symbol.errtxt, 'Invalid characters in data');
		result := error_number; exit;
	end;

	strcpy(to_process, '+');
	counter := 41;
	for i := 0 to _length - 1 do
		Inc(counter, posn(TECHNETIUM, source[i])) ;

	counter := counter mod 43;

	if (counter < 10) then
  begin
		check_digit := itoc(counter);
	end
  else
  begin
		if (counter < 36) then
    begin
			check_digit := Chr((counter - 10) + Ord('A'));
		end
    else
    begin
			case counter of
				36: check_digit := '-';
				37: check_digit := '.';
				38: check_digit := ' ';
				39: check_digit := '$';
				40: check_digit := '/';
				41: check_digit := '+';
				42: check_digit := '%';
				else check_digit := ' '; { Keep compiler happy }
			end;
		end;
	end;

	temp[0] := check_digit;
  temp[1] := #0;
  concat(to_process, source);
	concat(to_process, temp);
  _length := strlen(to_process);

	case symbol.symbology of
		BARCODE_HIBC_128:
    begin
			error_number := code_128(symbol, ArrayOfCharToArrayOfByte(to_process), _length);
      ustrcpy(symbol.text, '*');
      uconcat(symbol.text, to_process);
      uconcat(symbol.text, '*');
    end;
    BARCODE_HIBC_39:
    begin
			symbol.option_2 := 0;
			error_number := c39(symbol, ArrayOfCharToArrayOfByte(to_process), _length);
      ustrcpy(symbol.text, '*');
      uconcat(symbol.text, to_process);
      uconcat(symbol.text, '*');
    end;
    BARCODE_HIBC_DM:
			error_number := dmatrix(symbol, ArrayOfCharToArrayOfByte(to_process), _length);
		BARCODE_HIBC_QR:
			error_number := qr_code(symbol, ArrayOfCharToArrayOfByte(to_process), _length);
		BARCODE_HIBC_PDF:
			error_number := pdf417enc(symbol, ArrayOfCharToArrayOfByte(to_process), _length);
		BARCODE_HIBC_MICPDF:
			error_number := micro_pdf417(symbol, ArrayOfCharToArrayOfByte(to_process), _length);
		BARCODE_HIBC_AZTEC:
			error_number := aztec(symbol, ArrayOfCharToArrayOfByte(to_process), _length);
	end;

	Result := error_number; exit;
end;

function gs1_compliant(_symbology : Integer) : Integer;
{ Returns 1 if symbology supports GS1 data }
begin
  result := 0;

	case _symbology of
		BARCODE_EAN128,
		BARCODE_RSS_EXP,
		BARCODE_RSS_EXPSTACK,
		BARCODE_EANX_CC,
		BARCODE_EAN128_CC,
		BARCODE_RSS14_CC,
		BARCODE_RSS_LTD_CC,
		BARCODE_RSS_EXP_CC,
		BARCODE_UPCA_CC,
		BARCODE_UPCE_CC,
		BARCODE_RSS14STACK_CC,
		BARCODE_RSS14_OMNI_CC,
		BARCODE_RSS_EXPSTACK_CC,
		BARCODE_CODE16K,
		BARCODE_AZTEC,
		BARCODE_DATAMATRIX,
		BARCODE_CODEONE,
		BARCODE_CODE49,
		BARCODE_QRCODE:
			result := 1;
	end;
end;

function extended_charset(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  error_number : Integer;
begin
  error_number := 0;

	{ These are the "elite" standards which can support multiple character sets }
	case symbol.symbology of
	  BARCODE_QRCODE: error_number := qr_code(symbol, source, _length);
	 	BARCODE_MICROQR: error_number := microqr(symbol, source, _length);
		BARCODE_GRIDMATRIX: error_number := grid_matrix(symbol, source, _length);
	end;

	Result := error_number; exit;
end;

function reduced_charset(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
{ These are the "norm" standards which only support Latin-1 at most }
var
  error_number : Integer;
  preprocessed : TArrayOfByte;
begin
  SetLength(preprocessed, _length + 1);
  error_number := 0;

	if (symbol.symbology = BARCODE_CODE16K) then
  begin
		symbol.whitespace_width := 16;
		symbol.border_width := 2;
		symbol.output_options := BARCODE_BIND;
	end;

	if (symbol.symbology = BARCODE_ITF14) then
  begin
		symbol.whitespace_width := 20;
		symbol.border_width := 8;
		symbol.output_options := BARCODE_BOX;
	end;

	case symbol.input_mode of
		DATA_MODE,
		GS1_MODE:
			preprocessed := source;
		UNICODE_MODE:
    begin
			error_number := latin1_process(symbol, source, preprocessed, _length);
			if (error_number <> 0) then begin result := error_number; exit; end;
    end;
	end;

	case symbol.symbology of
		BARCODE_C25MATRIX: error_number := matrix_two_of_five(symbol, preprocessed, _length);
		BARCODE_C25IND: error_number := industrial_two_of_five(symbol, preprocessed, _length);
		BARCODE_C25INTER: error_number := interleaved_two_of_five(symbol, preprocessed, _length);
		BARCODE_C25IATA: error_number := iata_two_of_five(symbol, preprocessed, _length);
		BARCODE_C25LOGIC: error_number := logic_two_of_five(symbol, preprocessed, _length);
		BARCODE_DPLEIT: error_number := dpleit(symbol, preprocessed, _length);
		BARCODE_DPIDENT: error_number := dpident(symbol, preprocessed, _length);
		BARCODE_UPCA: error_number := eanx(symbol, preprocessed, _length);
		BARCODE_UPCE: error_number := eanx(symbol, preprocessed, _length);
		BARCODE_EANX: error_number := eanx(symbol, preprocessed, _length);
		BARCODE_EAN128: error_number := ean_128(symbol, preprocessed, _length);
		BARCODE_CODE39: error_number := c39(symbol, preprocessed, _length);
		BARCODE_PZN: error_number := pharmazentral(symbol, preprocessed, _length);
		BARCODE_EXCODE39: error_number := ec39(symbol, preprocessed, _length);
		BARCODE_CODABAR: error_number := codabar(symbol, preprocessed, _length);
		BARCODE_CODE93: error_number := c93(symbol, preprocessed, _length);
	  BARCODE_LOGMARS: error_number := c39(symbol, preprocessed, _length);
		BARCODE_CODE128: error_number := code_128(symbol, preprocessed, _length);
		BARCODE_CODE128B: error_number := code_128(symbol, preprocessed, _length);
		BARCODE_NVE18: error_number := nve_18(symbol, preprocessed, _length);
		BARCODE_CODE11: error_number := code_11(symbol, preprocessed, _length);
		BARCODE_MSI_PLESSEY: error_number := msi_handle(symbol, preprocessed, _length);
		BARCODE_TELEPEN: error_number := telepen(symbol, preprocessed, _length);
		BARCODE_TELEPEN_NUM: error_number := telepen_num(symbol, preprocessed, _length);
		BARCODE_PHARMA: error_number := pharma_one(symbol, preprocessed, _length);
		BARCODE_PLESSEY: error_number := plessey(symbol, preprocessed, _length);
		BARCODE_ITF14: error_number := itf14(symbol, preprocessed, _length);
		BARCODE_FLAT: error_number := flattermarken(symbol, preprocessed, _length);
		BARCODE_FIM: error_number := fim(symbol, preprocessed, _length);
		BARCODE_POSTNET: error_number := post_plot(symbol, preprocessed, _length);
		BARCODE_PLANET: error_number := planet_plot(symbol, preprocessed, _length);
		BARCODE_RM4SCC: error_number := royal_plot(symbol, preprocessed, _length);
		BARCODE_AUSPOST: error_number := australia_post(symbol, preprocessed, _length);
		BARCODE_AUSREPLY: error_number := australia_post(symbol, preprocessed, _length);
		BARCODE_AUSROUTE: error_number := australia_post(symbol, preprocessed, _length);
		BARCODE_AUSREDIRECT: error_number := australia_post(symbol, preprocessed, _length);
		BARCODE_CODE16K: error_number := code16k(symbol, preprocessed, _length);
		BARCODE_PHARMA_TWO: error_number := pharma_two(symbol, preprocessed, _length);
		BARCODE_ONECODE: error_number := imail(symbol, preprocessed, _length);
		BARCODE_ISBNX: error_number := eanx(symbol, preprocessed, _length);
		BARCODE_RSS14: error_number := rss14(symbol, preprocessed, _length);
		BARCODE_RSS14STACK: error_number := rss14(symbol, preprocessed, _length);
		BARCODE_RSS14STACK_OMNI: error_number := rss14(symbol, preprocessed, _length);
		BARCODE_RSS_LTD: error_number := rsslimited(symbol, preprocessed, _length);
		BARCODE_RSS_EXP: error_number := rssexpanded(symbol, preprocessed, _length);
		BARCODE_RSS_EXPSTACK: error_number := rssexpanded(symbol, preprocessed, _length);
		BARCODE_EANX_CC: error_number := composite(symbol, preprocessed, _length);
		BARCODE_EAN128_CC: error_number := composite(symbol, preprocessed, _length);
		BARCODE_RSS14_CC: error_number := composite(symbol, preprocessed, _length);
		BARCODE_RSS_LTD_CC: error_number := composite(symbol, preprocessed, _length);
		BARCODE_RSS_EXP_CC: error_number := composite(symbol, preprocessed, _length);
		BARCODE_UPCA_CC: error_number := composite(symbol, preprocessed, _length);
		BARCODE_UPCE_CC: error_number := composite(symbol, preprocessed, _length);
		BARCODE_RSS14STACK_CC: error_number := composite(symbol, preprocessed, _length);
		BARCODE_RSS14_OMNI_CC: error_number := composite(symbol, preprocessed, _length);
		BARCODE_RSS_EXPSTACK_CC: error_number := composite(symbol, preprocessed, _length);
		BARCODE_KIX: error_number := kix_code(symbol, preprocessed, _length);
		BARCODE_CODE32: error_number := code32(symbol, preprocessed, _length);
		BARCODE_DAFT: error_number := daft_code(symbol, preprocessed, _length);
		BARCODE_EAN14: error_number := ean_14(symbol, preprocessed, _length);
		BARCODE_AZRUNE: error_number := aztec_runes(symbol, preprocessed, _length);
		BARCODE_KOREAPOST: error_number := korea_post(symbol, preprocessed, _length);
		BARCODE_HIBC_128: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_39: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_DM: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_QR: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_PDF: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_MICPDF: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_AZTEC: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_JAPANPOST: error_number := japan_post(symbol, preprocessed, _length);
		BARCODE_CODE49: error_number := code_49(symbol, preprocessed, _length);
		BARCODE_CHANNEL: error_number := channel_code(symbol, preprocessed, _length);
		BARCODE_CODEONE: error_number := code_one(symbol, preprocessed, _length);
		BARCODE_DATAMATRIX: error_number := dmatrix(symbol, preprocessed, _length);
		BARCODE_PDF417: error_number := pdf417enc(symbol, preprocessed, _length);
		BARCODE_PDF417TRUNC: error_number := pdf417enc(symbol, preprocessed, _length);
		BARCODE_MICROPDF417: error_number := micro_pdf417(symbol, preprocessed, _length);
		BARCODE_MAXICODE: error_number := maxicode(symbol, preprocessed, _length);
		BARCODE_AZTEC: error_number := aztec(symbol, preprocessed, _length);
  end;

	result := error_number; exit;
end;

procedure TZintSymbol.Render(ATarget : TZintCustomRenderTarget);
begin
  ATarget.Render(Self);
end;

procedure TZintSymbol.SaveOption1(Writer: TWriter);
begin
  Writer.WriteInteger(option_1);
end;

procedure TZintSymbol.SaveOption2(Writer: TWriter);
begin
  Writer.WriteInteger(option_2);
end;

procedure TZintSymbol.SaveOption3(Writer: TWriter);
begin
  Writer.WriteInteger(option_3);
end;

procedure TZintSymbol.SetSymbology(const Value: TZintSymbology);
begin
  symbology := SymbologyToInt(Value);
  Changed;
end;

function ZBarcode_Encode(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  error_number, error_buffer, i : Integer;
  local_source : TArrayOfByte;
begin
  error_number := 0;

  if _length = 0 then
    _length := ustrlen(source);

	if (_length = 0) then
  begin
		strcpy(symbol.errtxt, 'No input data');
		error_tag(symbol.errtxt, ZERROR_INVALID_DATA);
		Result := ZERROR_INVALID_DATA; exit;
	end;

  SetLength(local_source, _length + 1);

	{ First check the symbology field }
	if (symbol.symbology < 1) then begin strcpy(symbol.errtxt, 'Symbology out of range, using Code 128'); symbol.symbology := BARCODE_CODE128; error_number := ZWARN_INVALID_OPTION; end;

	{ symbol.symbologys 1 to 86 are defined by tbarcode }
	if (symbol.symbology = 5) then begin symbol.symbology := BARCODE_C25MATRIX; end;
	if ((symbol.symbology >= 10) and (symbol.symbology <= 12)) then begin symbol.symbology := BARCODE_EANX; end;
	if ((symbol.symbology = 14) or (symbol.symbology = 15)) then begin symbol.symbology := BARCODE_EANX; end;
	if (symbol.symbology = 17) then begin symbol.symbology := BARCODE_UPCA; end;
	if (symbol.symbology = 19) then begin strcpy(symbol.errtxt, 'Codabar 18 not supported, using Codabar'); symbol.symbology := BARCODE_CODABAR; error_number := ZWARN_INVALID_OPTION; end;
	if (symbol.symbology = 26) then begin symbol.symbology := BARCODE_UPCA; end;
	if (symbol.symbology = 27) then begin strcpy(symbol.errtxt, 'UPCD1 not supported'); error_number := ZERROR_INVALID_OPTION; end;
	if (symbol.symbology = 33) then begin symbol.symbology := BARCODE_EAN128; end;
	if ((symbol.symbology = 35) or (symbol.symbology = 36)) then begin symbol.symbology := BARCODE_UPCA; end;
	if ((symbol.symbology = 38) or (symbol.symbology = 39)) then begin symbol.symbology := BARCODE_UPCE; end;
	if ((symbol.symbology >= 41) and (symbol.symbology <= 45)) then begin symbol.symbology := BARCODE_POSTNET; end;
	if (symbol.symbology = 46) then begin symbol.symbology := BARCODE_PLESSEY; end;
	if (symbol.symbology = 48) then begin symbol.symbology := BARCODE_NVE18; end;
	if (symbol.symbology = 54) then begin strcpy(symbol.errtxt, 'General Parcel Code not supported, using Code 128'); symbol.symbology := BARCODE_CODE128; error_number := ZWARN_INVALID_OPTION; end;
	if ((symbol.symbology = 59) or (symbol.symbology = 61)) then begin symbol.symbology := BARCODE_CODE128; end;
	if (symbol.symbology = 62) then begin symbol.symbology := BARCODE_CODE93; end;
	if ((symbol.symbology = 64) or (symbol.symbology = 65)) then begin symbol.symbology := BARCODE_AUSPOST; end;
	if (symbol.symbology = 73) then begin strcpy(symbol.errtxt, 'Codablock E not supported'); error_number := ZERROR_INVALID_OPTION; end;
	if (symbol.symbology = 78) then begin symbol.symbology := BARCODE_RSS14; end;
	if (symbol.symbology = 83) then begin symbol.symbology := BARCODE_PLANET; end;
	if (symbol.symbology = 88) then begin symbol.symbology := BARCODE_EAN128; end;
	if (symbol.symbology = 91) then begin strcpy(symbol.errtxt, 'Symbology out of range, using Code 128'); symbol.symbology := BARCODE_CODE128; error_number := ZWARN_INVALID_OPTION; end;
	if ((symbol.symbology >= 94) and (symbol.symbology <= 96)) then begin strcpy(symbol.errtxt, 'Symbology out of range, using Code 128'); symbol.symbology := BARCODE_CODE128; error_number := ZWARN_INVALID_OPTION; end;
	if (symbol.symbology = 100) then begin symbol.symbology := BARCODE_HIBC_128; end;
	if (symbol.symbology = 101) then begin symbol.symbology := BARCODE_HIBC_39; end;
	if (symbol.symbology = 103) then begin symbol.symbology := BARCODE_HIBC_DM; end;
	if (symbol.symbology = 105) then begin symbol.symbology := BARCODE_HIBC_QR; end;
	if (symbol.symbology = 107) then begin symbol.symbology := BARCODE_HIBC_PDF; end;
	if (symbol.symbology = 109) then begin symbol.symbology := BARCODE_HIBC_MICPDF; end;
	if (symbol.symbology = 111) then begin symbol.symbology := BARCODE_HIBC_BLOCKF; end;
	if ((symbol.symbology >= 113) and (symbol.symbology <= 127)) then begin strcpy(symbol.errtxt, 'Symbology out of range, using Code 128'); symbol.symbology := BARCODE_CODE128; error_number := ZWARN_INVALID_OPTION; end;
	{ Everything from 128 up is Zint-specific }
	if (symbol.symbology >= 143) then begin strcpy(symbol.errtxt, 'Symbology out of range, using Code 128'); symbol.symbology := BARCODE_CODE128; error_number := ZWARN_INVALID_OPTION; end;
	if ((symbol.symbology = BARCODE_CODABLOCKF) or (symbol.symbology = BARCODE_HIBC_BLOCKF)) then begin strcpy(symbol.errtxt, 'Codablock F not supported'); error_number := ZERROR_INVALID_OPTION; end;

	if (error_number > 4) then
  begin
		error_tag(symbol.errtxt, error_number);
		result := error_number; exit;
	end
  else
		error_buffer := error_number;

	if ((symbol.input_mode < 0) or (symbol.input_mode > 2)) then begin symbol.input_mode := DATA_MODE; end;

	if (symbol.input_mode = GS1_MODE) then
  begin
		for i := 0 to _length - 1 do
    begin
			if (source[i] = 0) then
      begin
				strcpy(symbol.errtxt, 'NULL characters not permitted in GS1 mode');
				result := ZERROR_INVALID_DATA; exit;
			end;
		end;
		if (gs1_compliant(symbol.symbology) = 1) then
    begin
			error_number := ugs1_verify(symbol, source, _length, local_source);
			if (error_number <> 0) then begin result := error_number; exit; end;
			_length := ustrlen(local_source);
		end
    else
    begin
			strcpy(symbol.errtxt, 'Selected symbology does not support GS1 mode');
			result := ZERROR_INVALID_OPTION; exit;
    end;
  end
  else
		local_source := source;

	case symbol.symbology of
		BARCODE_QRCODE,
		BARCODE_MICROQR,
		BARCODE_GRIDMATRIX:
			error_number := extended_charset(symbol, local_source, _length);
    else
			error_number := reduced_charset(symbol, local_source, _length);
	end;

	if ((symbol.symbology = BARCODE_CODE128) or (symbol.symbology = BARCODE_CODE128B)) then
  begin
		for i := 0 to _length - 1 do
    begin
			if (local_source[i] = 0) then
				symbol.text[i] := 32
      else
				symbol.text[i] := local_source[i];
		end;
    symbol.text[_length] := 0;
	end;

	if (error_number = 0) then
		error_number := error_buffer;

	error_tag(symbol.errtxt, error_number);
	result := error_number; exit;
end;

{ TZintCustomRenderTarget }

function TZintCustomRenderTarget.CalcX(AValue: Single): Single;
begin
  case FHAlign of
    haLeft : Result := FXDesired + AValue;
    haCenter : Result := FXDesired + (FWidthDesired - FWidth) / 2 + AValue;
    haRight : Result := FXDesired + FWidthDesired - FWidth + AValue;
    else
      Result := 0; //keep the compiler happy
  end;
end;

function TZintCustomRenderTarget.CalcY(AValue: Single): Single;
begin
  case FVAlign of
    vaTop : Result := FYDesired + AValue;
    vaCenter : Result := FYDesired + (FHeightDesired - FHeight) / 2 + AValue;
    vaBottom : Result := FYDesired + FHeightDesired - FHeight + AValue;
    else
      Result := 0; //keep the compiler happy
  end;
end;

procedure TZintCustomRenderTarget.AddSymbolOptions;
begin
  FWhitespace.Left.IncModules(FSymbol.whitespace_width);
  FWhitespace.Right.IncModules(FSymbol.whitespace_width);

  if FSymbol.output_options and (BARCODE_BIND or BARCODE_BOX) <> 0 then
  begin
    FBorder.Top.IncModules(FSymbol.border_width);
    FBorder.Bottom.IncModules(FSymbol.border_width);
  end;

  if FSymbol.output_options and BARCODE_BOX <> 0 then
  begin
    FBorder.Left.IncModules(FSymbol.border_width);
    FBorder.Right.IncModules(FSymbol.border_width);
  end;
end;

procedure TZintCustomRenderTarget.Assign(Source : TPersistent);
var
  SourceRT : TZintCustomRenderTarget;
begin
  if Source is TZintCustomRenderTarget then
  begin
    SourceRT := TZintCustomRenderTarget(Source);
    FXDesired := SourceRT.XDesired;
    FYDesired := SourceRT.YDesired;
    FWidthDesired := SourceRT.WidthDesired;
    FHeightDesired := SourceRT.HeightDesired;
    FRenderAdjustMode := SourceRT.RenderAdjustMode;
    FTransparent := SourceRT.Transparent;
    FHexagonScale := SourceRT.HexagonScale;
    FMargin.Assign(SourceRT.Margin);
    FBorder.Assign(SourceRT.Border);
    FPadding.Assign(SourceRT.Padding);
    FWhitespace.Assign(SourceRT.Whitespace);
    FTextSpacing.Assign(SourceRT.TextSpacing);
    FHAlign := SourceRT.HAlign;
    FVAlign := SourceRT.VAlign;
    FMinModuleWidth := SourceRT.MinModuleWidth;
    FShowText := SourceRT.ShowText;
    Changed;
  end
  else
    inherited;
end;

procedure TZintCustomRenderTarget.RemoveSymbolOptions;
begin
  FWhitespace.Left.DecModules(FSymbol.whitespace_width);
  FWhitespace.Right.DecModules(FSymbol.whitespace_width);

  if FSymbol.output_options and (BARCODE_BIND or BARCODE_BOX) <> 0 then
  begin
    FBorder.Top.DecModules(FSymbol.border_width);
    FBorder.Bottom.DecModules(FSymbol.border_width);
  end;

  if FSymbol.output_options and BARCODE_BOX <> 0 then
  begin
    FBorder.Left.DecModules(FSymbol.border_width);
    FBorder.Right.DecModules(FSymbol.border_width);
  end;
end;

procedure TZintCustomRenderTarget.AddBoxModulesToTargetUnits;
begin
  FMargin.AddModulesToTargetUnits(FModuleWidth, FModuleHeight);
  FBorder.AddModulesToTargetUnits(FModuleWidth, FModuleHeight);
  FPadding.AddModulesToTargetUnits(FModuleWidth, FModuleHeight);
  FWhitespace.AddModulesToTargetUnits(FModuleWidth, FModuleHeight);
  //i'm sorry, but left and right textspace can only be set in targetunits
  FTextSpacing.AddModulesToTargetUnits(FModuleWidth, FModuleHeight, true, true, false, false);
end;

procedure TZintCustomRenderTarget.RemoveBoxModulesFromTargetUnits;
begin
  FMargin.RemoveModulesFromTargetUnits(FModuleWidth, FModuleHeight);
  FBorder.RemoveModulesFromTargetUnits(FModuleWidth, FModuleHeight);
  FPadding.RemoveModulesFromTargetUnits(FModuleWidth, FModuleHeight);
  FWhitespace.RemoveModulesFromTargetUnits(FModuleWidth, FModuleHeight);
  FTextSpacing.RemoveModulesFromTargetUnits(FModuleWidth, FModuleHeight, true, true, false, false);
end;

procedure TZintCustomRenderTarget.FetchRowInfos;
var
  idx : Integer;
begin
  FRowHeights := 0;
  FLargeBarCount := 0;

  for idx := 0 to FSymbol.rows - 1 do
  begin
    FRowHeights := FRowHeights + FSymbol.row_height[idx];
    if FSymbol.row_height[idx] = 0 then
      Inc(FLargeBarCount)
  end;
end;

procedure TZintCustomRenderTarget.CalcSize;
var
  BarcodeSpace,
  Modules,
  ExtraModules: Single;
  FModuleHWRatio : Single;

  function CalcModulesWidth : Single;
  begin
    Result := FSymbol.width +
              FWhitespace.LeftAndRightModules +
              FPadding.LeftAndRightModules +
              FBorder.LeftAndRightModules +
              FMargin.LeftAndRightModules;
  end;

begin
  if FSymbol.symbology = BARCODE_MAXICODE then
    FModuleHWRatio := 2 / sqrt(3)
  else
    FModuleHWRatio := 1;

  FWidth := FWidthDesired;
  Modules := CalcModulesWidth;

  BarcodeSpace := (FWidthDesired -
                   FWhitespace.LeftAndRightTargetUnits -
                   FPadding.LeftAndRightTargetUnits -
                   FBorder.LeftAndRightTargetUnits -
                   FMargin.LeftAndRightTargetUnits);

  if BarcodeSpace <= 0 then //if the desired width is too small ...
  begin
    if FRenderAdjustMode <> ramInflate then //... and we can't inflate the image ...
      raise Exception.Create(EDesiredWithTooSmall) //... then we can go home ;)
    else
    begin
      FWidth := FMargin.LeftAndRightTargetUnits +
                FBorder.LeftAndRightTargetUnits +
                FPadding.LeftAndRightTargetUnits +
                FWhitespace.LeftAndRightTargetUnits;
      if FMinModuleWidth > 0 then //if there is a MinModuleWidth, we care about it, in order to waive an Inflate()
        BarcodeSpace := Modules * FMinModuleWidth
      else
        BarcodeSpace := Modules;
      FWidth := FWidth + BarcodeSpace;
      Inflate(FWidth, FHeightDesired);
      FWidthDesired := FWidth;
    end;
  end;

  FModuleWidth := BarcodeSpace / Modules;

  if (FMaxModuleWidth > 0) and
     (FModuleWidth > FMaxModuleWidth) then
  begin
    FWidth := FWidth * (FMaxModuleWidth / FModuleWidth);
    FModuleWidth := FMaxModuleWidth;
  end;

  //if there is a minimum ModuleWidth defined, we have to care about it
  if (FMinModuleWidth > 0) and
     (FModuleWidth < FMinModuleWidth) and
     (FRenderAdjustMode = ramInflate) then
  begin
    FWidth := FWidth * (FMinModuleWidth / FModuleWidth);
    Inflate(FWidth, FHeightDesired);
    FWidthDesired := FWidth;
    FModuleWidth := FMinModuleWidth;
  end;

  //lets go on with the height
  FModuleHeight := FModuleWidth * FModuleHWRatio;

  //we need 2 vars, because Maxicode has a special height calculation
  Modules := FRowHeights + FLargeBarCount;
  ExtraModules := FWhitespace.TopAndBottomModules +
                  FPadding.TopAndBottomModules +
                  FBorder.TopAndBottomModules +
                  FMargin.TopAndBottomModules;
  if FHasText and FShowText then
    ExtraModules := ExtraModules +
                    FTextSpacing.TopAndBottomModules;

  BarcodeSpace := FHeightDesired -
                  FWhitespace.TopAndBottomTargetUnits -
                  FPadding.TopAndBottomTargetUnits -
                  FBorder.TopAndBottomTargetUnits -
                  FMargin.TopAndBottomTargetUnits;
  if FHasText and FShowText then
    BarcodeSpace := BarcodeSpace -
                    FTextSpacing.TopAndBottomTargetUnits -
                    FTextHeight;

  //calc the minium height
  if FSymbol.symbology = BARCODE_MAXICODE then
  begin
    FHeight := Modules * FModuleHeight * 0.75 +
               FModuleHeight * 0.25 +
               ExtraModules * FModuleHeight +
               FWhitespace.TopAndBottomTargetUnits +
               FPadding.TopAndBottomTargetUnits +
               FBorder.TopAndBottomTargetUnits +
               FMargin.TopAndBottomTargetUnits;
  end
  else
  begin
    FHeight := (Modules + ExtraModules) * FModuleHeight +
               FWhitespace.TopAndBottomTargetUnits +
               FPadding.TopAndBottomTargetUnits +
               FBorder.TopAndBottomTargetUnits +
               FMargin.TopAndBottomTargetUnits;
    if FHasText and FShowText then
      FHeight := FHeight +
                 FTextHeight +
                 FTextSpacing.TopAndBottomTargetUnits;
  end;

  if BarcodeSpace <= 0 then //if the desired height is too small ...
  begin
    if FRenderAdjustMode <> ramInflate then //... and we can't inflate the image ...
      raise Exception.Create(EDesiredHeightTooSmall) //... then we can go home ;)
    else
    begin
      Inflate(FWidth, FHeight);
      FHeightDesired := FHeight;
      BarcodeSpace := Modules * FModuleHeight;
    end;
  end;

  if FHeight > FHeightDesired then
  begin
    case FRenderAdjustMode of
      ramInflate:
      begin
        Inflate(FWidth, FHeight);
        FHeightDesired := FHeight;
      end;
      ramScale:
      begin
        //starting from the height, we have to recalc the width
        if FSymbol.symbology = BARCODE_MAXICODE then
          FModuleHeight := BarcodeSpace / (Modules * 0.75 + 0.25 + ExtraModules)
        else
          FModuleHeight := BarcodeSpace / (Modules + ExtraModules);
        FModuleWidth := FModuleHeight / FModuleHWRatio;
        Modules := CalcModulesWidth;
        FWidth := Modules * FModuleWidth +
                  FWhitespace.LeftAndRightTargetUnits +
                  FPadding.LeftAndRightTargetUnits +
                  FBorder.LeftAndRightTargetUnits +
                  FMargin.LeftAndRightTargetUnits;
        FHeight := FHeightDesired;
      end;
    end;
  end
  else
  if (FSymbol.symbology <> BARCODE_MAXICODE) and (FLargeBarCount > 0) then
    FHeight := FHeightDesired;
end;

procedure TZintCustomRenderTarget.CalcText;
var
  idx : Integer;
  CTHP : TZintCalcTextHeightParams;
  {$IFDEF UseTEncoding}
  e : TEncoding;
  b : TArrayOfByte;
  {$ENDIF}
begin
  FHasText := ustrlen(FSymbol.text) > 0;
  if FHasText then
  begin
   if (FSymbol.input_mode and UNICODE_MODE) <> 0 then
   begin
    {$IFDEF UseTEncoding}
      {$IFDEF FPC}
        e := TEncoding.ANSI;
      {$ELSE}
        e := TEncoding.UTF8;
      {$ENDIF}
        b:=FSymbol.text;
        setlength(b, ustrlen(FSymbol.text));
        FText:=e.GetString(b);
    {$ELSE}
      FText := UTF8Decode(ArrayOfByteToString(FSymbol.text));
    {$ENDIF}
   end
   else
     FText := ArrayOfByteToString(FSymbol.text);
  end
  else
    FText := '';

  idx := Pos('+', FText);
  FHasAddonText := (is_extendable(FSymbol.symbology) <> 0) and (idx > 0);
  if FHasAddonText then
  begin
    FAddonText := Copy(FText, idx + 1, Length(FText) - idx);
    FText := Copy(FText, 1, idx - 1);
  end
  else
    FAddonText := '';

  if FShowText and FHasText then
  begin
    CTHP.Text := FText;
    FTextHeight := CalcTextHeight(CTHP);
  end
  else
  begin
    FTextHeight := 0;
  end;
end;

procedure TZintCustomRenderTarget.CalcTextEANUPC;
var
  CTWP : TZintCalcTextWidthParams;
begin
  if (euEAN13 in FEANUPCFlags) or
     (euUPCA in FEANUPCFlags) or
     (euUPCE in FEANUPCFlags) then
  begin
    FLeadingText := Copy(FText, 1, 1);
    CTWP.Text := FLeadingText;
    FLeadingTextWidth := CalcTextWidth(CTWP);
    if FWhitespace.Left.TargetUnits < FLeadingTextWidth + FTextSpacing.Right.TargetUnits then
    begin
      FWhitespace.Left.TargetUnits := FLeadingTextWidth + FTextSpacing.Right.TargetUnits;
    end;
  end
  else
    FLeadingTextWidth := 0;

  if (euUPCA in FEANUPCFlags) or
     (euUPCE in FEANUPCFlags) then
  begin
    FTrailingText := Copy(FText, Length(FText), 1);
    CTWP.Text := FTrailingText;
    FTrailingTextWidth := CalcTextWidth(CTWP);
    //if there is no addon, we have to increase the whitespace (maybe)
    if (not ((euAddon2 in FEANUPCFlags) or (euAddon5 in FEANUPCFlags))) and
       (FWhitespace.Right.TargetUnits < FTrailingTextWidth + FTextSpacing.Left.TargetUnits) then
    begin
      FWhitespace.Right.TargetUnits := FTrailingTextWidth + FTextSpacing.Left.TargetUnits;
    end;
  end
  else
    FTrailingTextWidth := 0;
end;

procedure TZintCustomRenderTarget.CheckEANUPC;
begin
  FEANUPCFlags := [];

  if ((FSymbol.symbology in [BARCODE_EANX, BARCODE_UPCA, BARCODE_UPCE]) and (FSymbol.rows = 1)) or
     (FSymbol.symbology in [BARCODE_EANX_CC, BARCODE_ISBNX, BARCODE_UPCA_CC, BARCODE_UPCE_CC]) then
  begin
    if FHasText then
    begin
      case FSymbol.symbology of
        BARCODE_EANX, BARCODE_EANX_CC, BARCODE_ISBNX:
        begin
          if Length(FText) = 8 then Include(FEANUPCFlags, euEAN8);
          if Length(FText) = 13 then Include(FEANUPCFlags, euEAN13);
        end;
        BARCODE_UPCA, BARCODE_UPCA_CC: Include(FEANUPCFlags, euUPCA);
        BARCODE_UPCE, BARCODE_UPCE_CC: Include(FEANUPCFlags, euUPCE);
      end;
    end;
    if FHasAddonText then
    begin
      if Length(FAddonText) = 2 then Include(FEANUPCFlags, euAddon2);
      if Length(FAddonText) = 5 then Include(FEANUPCFlags, euAddon5);
    end;
  end;
end;

procedure TZintCustomRenderTarget.CalcLargeBarHeight;
begin
  if FLargeBarCount > 0 then
    FLargeBarHeight := (FBarcodeRect.Height - FRowHeights * FModuleHeight) / FLargeBarCount;
end;

procedure TZintCustomRenderTarget.CalcBoxes;
begin
  FX := CalcX(0);
  FY := CalcY(0);

  FMarginRect.X := FX;
  FMarginRect.Y := FY;
  FMarginRect.Width := FWidth;
  FMarginRect.Height := FHeight;

  FBorderRect.X := FMarginRect.X + FMargin.Left.TargetUnits;
  FBorderRect.Y := FMarginRect.Y + FMargin.Top.TargetUnits;
  FBorderRect.Width := FMarginRect.Width - FMargin.Left.TargetUnits - FMargin.Right.TargetUnits;
  FBorderRect.Height := FMarginRect.Height - FMargin.Top.TargetUnits - FMargin.Bottom.TargetUnits;

  FPaddingRect.X := FBorderRect.X + FBorder.Left.TargetUnits;
  FPaddingRect.Y := FBorderRect.Y + FBorder.Top.TargetUnits;
  FPaddingRect.Width := FBorderRect.Width - FBorder.Left.TargetUnits - FBorder.Right.TargetUnits;
  FPaddingRect.Height := FBorderRect.Height - FBorder.Top.TargetUnits - FBorder.Bottom.TargetUnits;

  FWhitespaceRect.X := FPaddingRect.X + FPadding.Left.TargetUnits;
  FWhitespaceRect.Y := FPaddingRect.Y + FPadding.Top.TargetUnits;
  FWhitespaceRect.Width := FPaddingRect.Width - FPadding.Left.TargetUnits - FPadding.Right.TargetUnits;
  FWhitespaceRect.Height := FPaddingRect.Height - FPadding.Top.TargetUnits - FPadding.Bottom.TargetUnits -
                           FTextHeight - FTextSpacing.Bottom.TargetUnits - FTextSpacing.Top.TargetUnits;

  FTextSpacingRect.X := FWhitespaceRect.X;
  FTextSpacingRect.Y := FWhitespaceRect.Y + FWhitespaceRect.Height;
  FTextSpacingRect.Width := FWhitespaceRect.Width;
  FTextSpacingRect.Height := FTextSpacing.Top.TargetUnits + FTextHeight + FTextSpacing.Bottom.TargetUnits;

  FTextRect.X := FTextSpacingRect.X + FTextSpacing.Left.TargetUnits;
  FTextRect.Y := FTextSpacingRect.Y + FTextSpacing.Top.TargetUnits;
  FTextRect.Width := FTextSpacingRect.Width - FTextSpacing.Left.TargetUnits - FTextSpacing.Right.TargetUnits;
  FTextRect.Height := FTextSpacingRect.Height - FTextSpacing.Top.TargetUnits - FTextSpacing.Bottom.TargetUnits;

  FBarcodeRect.X := FWhitespaceRect.X + FWhitespace.Left.TargetUnits;
  FBarcodeRect.Y := FWhitespaceRect.Y + FWhitespace.Top.TargetUnits;
  FBarcodeRect.Width := FWhitespaceRect.Width - FWhitespace.Left.TargetUnits - FWhitespace.Right.TargetUnits;
  FBarcodeRect.Height := FWhitespaceRect.Height - FWhitespace.Top.TargetUnits - FWhitespace.Bottom.TargetUnits;
end;

procedure TZintCustomRenderTarget.DrawBorder;
var
  DRP : TZintDrawRectParams;
begin
  if FBorder.Top.TargetUnits > 0 then
  begin
    DRP.X := FBorderRect.X;
    DRP.Y := FBorderRect.Y;
    DRP.Width := FBorderRect.Width;
    DRP.Height := FBorder.Top.TargetUnits;
    DrawRect(DRP);
  end;

  if FBorder.Bottom.TargetUnits > 0 then
  begin
    DRP.X := FBorderRect.X;
    DRP.Y := FBorderRect.Y + FBorderRect.Height - FBorder.Bottom.TargetUnits;
    DRP.Width := FBorderRect.Width;
    DRP.Height := FBorder.Bottom.TargetUnits;
    DrawRect(DRP);
  end;

  if FBorder.Left.TargetUnits > 0 then
  begin
    DRP.X := FBorderRect.X;
    DRP.Y := FBorderRect.Y;
    DRP.Width := FBorder.Left.TargetUnits;
    DRP.Height := FBorderRect.Height;
    DrawRect(DRP);
  end;

  if FBorder.Right.TargetUnits > 0 then
  begin
    DRP.X := FBorderRect.X + FBorderRect.Width - FBorder.Right.TargetUnits;
    DRP.Y := FBorderRect.Y;
    DRP.Width := FBorder.Right.TargetUnits;
    DRP.Height := FBorderRect.Height;
    DrawRect(DRP);
  end;
end;

procedure TZintCustomRenderTarget.DrawMaxiRings;
var
  DRP : TZintDrawRingParams;
  LineWidth : Single;
  OuterRadius : Single;
begin
  LineWidth := FBarcodeRect.Height / 40;
  OuterRadius := (11 * FModuleHeight * 0.75 - FModuleHeight * 0.25) / 2;

  DRP.X := FBarcodeRect.X + FBarcodeRect.Width / 2 - FModuleWidth / 2;
  DRP.Y := FBarcodeRect.Y + FBarcodeRect.Height / 2;

  DRP.OuterRadius := OuterRadius - 0 * LineWidth;
  DRP.InnerRadius := OuterRadius - 1 * LineWidth;
  DrawRing(DRP);

  DRP.OuterRadius := OuterRadius - 2 * LineWidth;
  DRP.InnerRadius := OuterRadius - 3 * LineWidth;
  DrawRing(DRP);

  DRP.OuterRadius := OuterRadius - 4 * LineWidth;
  DRP.InnerRadius := OuterRadius - 5 * LineWidth;
  DrawRing(DRP);
end;

procedure TZintCustomRenderTarget.DrawMaxiModules;
var
  row, col : Integer;
  LX, LY : Single;
  DHP : TZintDrawHexagonParams;
begin
  DHP.Width := FModuleWidth;
  DHP.Height := FModuleHeight;

  LY := FBarcodeRect.Y + FModuleHeight * 0.5;
  for row := 0 to FSymbol.rows - 1 do
  begin
    LX := FBarcodeRect.X + FModuleWidth * 0.5;
    for col := 0 to FSymbol.width - 1 do
    begin
      if module_is_set(FSymbol, row, col) <> 0 then
      begin
        DHP.Y := LY;

        if (row and 1) <> 0 then
          DHP.X := LX + FModuleWidth * 0.5
        else
          DHP.X := LX;

        DrawHexagon(DHP);
      end;
      LX := LX + FModuleWidth;
    end;
    LY := LY + FModuleHeight * 0.75;
  end;
end;

procedure TZintCustomRenderTarget.DrawModules;
var
  row, col : Integer;
  block_width : Integer;
  isspace : Boolean;
  LX,LY : Single;
  DRP : TZintDrawRectParams;
  BarHeight : Single;
  BarIndex : Integer;
begin
  LY := FBarcodeRect.Y;

  for row := 0 to FSymbol.rows - 1 do
  begin
    BarIndex := 0;
    LX := FBarcodeRect.X;
    col := 0;
    isspace := module_is_set(FSymbol, row, col) = 0;

    if FSymbol.row_height[row] = 0 then
      BarHeight := FLargeBarHeight
    else
      BarHeight := FSymbol.row_height[row] * FModuleWidth;

    if (row > 0) and ((FSymbol.output_options and (BARCODE_BIND or BARCODE_BIND)) <> 0) and
       (is_stackable(FSymbol.symbology) <> 0) then
    begin
      DRP.X := LX;
      DRP.Y := LY - (FSymbol.border_width * FModuleWidth) / 2;
      DRP.Width := FBarcodeRect.Width;
      DRP.Height := FSymbol.border_width * FModuleWidth;
      DrawRect(DRP);
    end;

    repeat
      block_width := 0;

      repeat
        Inc(block_width);
      until not (module_is_set(FSymbol, row, col + block_width) = module_is_set(FSymbol, row, col));

      if not isspace then
      begin
        DRP.X := LX;
        DRP.Y := LY;
        DRP.Width := block_width * FModuleWidth;
        DRP.Height := BarHeight;

        if FShowText and FHasText and (row = FSymbol.rows - 1) and (FEANUPCFlags <> []) then
          HandleSpecialBarsEANUPC(BarIndex, DRP);

        DrawRect(DRP);
        Inc(BarIndex)
      end;

      Inc(col, block_width);
      LX := LX + block_width * FModuleWidth;
      isspace := isspace xor true;
    until col >= FSymbol.width;

    LY := LY + BarHeight;
  end;
end;

procedure TZintCustomRenderTarget.DrawTexts;
var
  DTP : TZintDrawTextParams;
begin
  if FShowText and FHasText and (not FTextDone) then
  begin
    DTP.X := FTextRect.X;
    DTP.Y := FTextRect.Y;
    DTP.Width := FTextRect.Width;
    DTP.Height := FTextRect.Height;
    DTP.Text := FText;

    DrawText(DTP);
  end;
end;

procedure TZintCustomRenderTarget.RenderStart;
begin
end;

procedure TZintCustomRenderTarget.RenderStop;
begin
end;

procedure TZintCustomRenderTarget.SetBox(const Index: Integer;
  const Value: TZintRenderBox);
begin
  case Index of
    0 : FMargin.Assign(Value);
    1 : FBorder.Assign(Value);
    2 : FPadding.Assign(Value);
    3 : FWhitespace.Assign(Value);
    4 : FTextSpacing.Assign(Value);
  end;
  Changed;
end;

procedure TZintCustomRenderTarget.SetHAlign(const Value: TZintHAlign);
begin
  if Value <> FHAlign then
  begin
    FHAlign := Value;
    Changed;
  end;
end;

procedure TZintCustomRenderTarget.SetHexagonScale(const Value: Single);
begin
  if Value <> FHexagonScale then
  begin
    FHexagonScale := Value;
    Changed;
  end;
end;

procedure TZintCustomRenderTarget.SetMaxModuleWidth(AValue: Single);
begin
  if AValue <> FMaxModuleWidth then
  begin
    FMaxModuleWidth:=AValue;
    changed;
  end;
end;

procedure TZintCustomRenderTarget.SetMinModuleWidth(const Value: Single);
begin
  if Value <> FMinModuleWidth then
  begin
    FMinModuleWidth := Value;
    Changed;
  end;
end;

procedure TZintCustomRenderTarget.SetRenderAdjustMode(
  const Value: TZintRenderAdjustMode);
begin
  if Value <> FRenderAdjustMode then
  begin
    FRenderAdjustMode := Value;
    Changed;
  end;
end;

procedure TZintCustomRenderTarget.SetShowText(const Value: Boolean);
begin
  if Value <> FShowText then
  begin
    FShowText := Value;
    Changed;
  end;
end;

procedure TZintCustomRenderTarget.SetTransparent(const Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

procedure TZintCustomRenderTarget.SetVAlign(const Value: TZintVAlign);
begin
  if Value <> FVAlign then
  begin
    FVAlign := Value;
    Changed;
  end;
end;

procedure TZintCustomRenderTarget.DrawStart;
begin
end;

procedure TZintCustomRenderTarget.DrawStop;
begin
end;

procedure TZintCustomRenderTarget.HandleSpecialBarsEANUPC(ABarIndex : Integer; var ABar : TZintDrawRectParams);
var
  DTP : TZintDrawTextParams;
begin
  FTextDone := true;

  //guardbars are longer then the others
  if ((euEAN8 in FEANUPCFlags) and (ABarIndex in [0,1,10,11,20,21])) or
     ((euEAN8 in FEANUPCFlags) and (ABarIndex > 21)) or
     ((euEAN13 in FEANUPCFlags) and (ABarIndex in [0,1,14,15,28,29])) or
     ((euEAN13 in FEANUPCFlags) and (ABarIndex > 29)) or
     ((euUPCA in FEANUPCFlags) and (ABarIndex in [0,1,2,3,14,15,26,27,28,29])) or
     ((euUPCA in FEANUPCFlags) and (ABarIndex > 29)) or
     ((euUPCE in FEANUPCFlags) and (ABarIndex in [0,1,14,15,16])) or
     ((euUPCE in FEANUPCFlags) and (ABarIndex > 16))then
    ABar.Height := ABar.Height + FWhitespace.Bottom.TargetUnits + FTextSpacing.Top.TargetUnits + FTextHeight / 2;

  //addon-bars need space above for the text
  if ((euEAN8 in FEANUPCFlags) and (ABarIndex > 21)) or
     ((euEAN13 in FEANUPCFlags) and (ABarIndex > 29)) or
     ((euUPCA in FEANUPCFlags) and (ABarIndex > 29)) or
     ((euUPCE in FEANUPCFlags) and (ABarIndex > 16)) then
  begin
    ABar.Y := ABar.Y + FTextHeight + FTextSpacing.Top.TargetUnits + FTextSpacing.Bottom.TargetUnits;
    ABar.Height := ABar.Height - FTextHeight - FTextSpacing.Top.TargetUnits - FTextSpacing.Bottom.TargetUnits;
  end;

  //add leading text
  if (ABarIndex = 0) and
     ((euEAN13 in FEANUPCFlags) or
      (euUPCA in FEANUPCFlags) or
      (euUPCE in FEANUPCFlags)) then
  begin
    DTP.X := ABar.X - FTextSpacing.Right.TargetUnits - FLeadingTextWidth;
    DTP.Y := FTextRect.Y;
    DTP.Width := FLeadingTextWidth;
    DTP.Height := FTextRect.Height;
    DTP.Text := FLeadingText;
    DrawText(DTP);
  end;

  //add trailing text
  if ((euUPCA in FEANUPCFlags) and (ABarIndex = 29)) or
     ((euUPCE in FEANUPCFlags) and (ABarIndex = 16)) then
  begin
    DTP.X := ABar.X + ABar.Width + FTextSpacing.Left.TargetUnits;
    DTP.Y := FTextRect.Y;
    DTP.Width := FTrailingTextWidth;
    DTP.Height := FTextRect.Height;
    DTP.Text := FTrailingText;
    DrawText(DTP);
  end;

  //draw the main text under the barcode
  if ((euEAN8 in FEANUPCFlags) and (ABarIndex in [10, 20])) or
     ((euEAN13 in FEANUPCFlags) and (ABarIndex in [14, 28])) or
     ((euUPCA in FEANUPCFlags) and (ABarIndex in [14, 26])) or
     ((euUPCE in FEANUPCFlags) and (ABarIndex = 14)) then
  begin
    DTP.X := FStartTextBar.X + FStartTextBar.Width + FTextSpacing.Left.TargetUnits;
    DTP.Y := FTextRect.Y;
    DTP.Height := FTextRect.Height;
    DTP.Width := ABar.X - FStartTextBar.X + FStartTextBar.Width - FTextSpacing.Left.TargetUnits - FTextSpacing.Right.TargetUnits;
    if euEAN8 in FEANUPCFlags then
    begin
      case ABarIndex of
        10: DTP.Text := Copy(FText, 1, 4);
        20: DTP.Text := Copy(FText, 5, 4);
      end;
    end
    else
    if (euEAN13 in FEANUPCFlags) then
    begin
      case ABarIndex of
        14: DTP.Text := Copy(FText, 2, 6);
        28: DTP.Text := Copy(FText, 8, 6);
      end;
    end
    else
    if (euUPCA in FEANUPCFlags) then
    begin
      case ABarIndex of
        14: DTP.Text := Copy(FText, 2, 5);
        26: DTP.Text := Copy(FText, 7, 5);
      end;
    end
    else
    if (euUPCE in FEANUPCFlags) then
    begin
      DTP.Text := Copy(FText, 2, 6);
    end;
    DrawText(DTP);
  end;

  if ((euEAN8 in FEANUPCFlags) and (ABarIndex = 28) and (euAddon2 in FEANUPCFlags)) or
     ((euEAN8 in FEANUPCFlags) and (ABarIndex = 36)) or
     ((euEAN13 in FEANUPCFlags) and (ABarIndex = 36) and (euAddon2 in FEANUPCFlags)) or
     ((euEAN13 in FEANUPCFlags) and (ABarIndex = 44)) or
     ((euUPCA in FEANUPCFlags) and (ABarIndex = 36) and (euAddon2 in FEANUPCFlags)) or
     ((euUPCA in FEANUPCFlags) and (ABarIndex = 44)) or
     ((euUPCE in FEANUPCFlags) and (ABarIndex = 23) and (euAddon2 in FEANUPCFlags)) or
     ((euUPCE in FEANUPCFlags) and (ABarIndex = 32)) then
  begin
    DTP.X := FStartTextBar.X;
    DTP.Y := FBarcodeRect.Y + FTextSpacing.Top.TargetUnits;
    DTP.Height := FTextHeight;
    DTP.Width := ABar.X + ABar.Width - FStartTextBar.X;
    DTP.Text := FAddonText;
    DrawText(DTP);
  end;

  if ((euEAN8 in FEANUPCFlags) and (ABarIndex in [1, 11, 22])) or
     ((euEAN13 in FEANUPCFlags) and (ABarIndex in [1, 15, 30])) or
     ((euUPCA in FEANUPCFlags) and (ABarIndex in [3, 15, 30])) or
     ((euUPCE in FEANUPCFlags) and (ABarIndex in [1, 17])) then
    FStartTextBar := ABar;
end;

constructor TZintCustomRenderTarget.Create(AOwner : TPersistent);
begin
  inherited;

  FTransparent:=False;
  FHexagonScale := 0.9;
  FMargin := TZintRenderBox.Create(Self);
  FBorder := TZintRenderBox.Create(Self);
  FPadding := TZintRenderBox.Create(Self);
  FWhitespace := TZintRenderBox.Create(Self);
  FTextSpacing := TZintRenderBox.Create(Self);
  FShowText := true;
  FHAlign := haLeft;
  FVAlign := vaTop;
  FMinModuleWidth := 0;
  FMaxModuleWidth := 0;
  FRenderAdjustMode := ramScale;
end;

destructor TZintCustomRenderTarget.Destroy;
begin
  FreeAndNil(FMargin);
  FreeAndNil(FBorder);
  FreeAndNil(FPadding);
  FreeAndNil(FWhitespace);
  FreeAndNil(FTextSpacing);
  inherited;
end;

procedure TZintCustomRenderTarget.Render(ASymbol: TZintSymbol);
var
  CBP : TZintClearBackgroundParams;
begin
  FSymbol := ASymbol;
  FTextDone := false;

  RenderStart;
  AddSymbolOptions;
  FetchRowInfos;
  CalcText;
  CheckEANUPC;
  CalcTextEANUPC;
  CalcSize;
  AddBoxModulesToTargetUnits;
  CalcBoxes;
  CalcLargeBarHeight;

  DrawStart;

  if not FTransparent then
  begin
    CBP.X := FXDesired;
    CBP.Y := FYDesired;
    CBP.Width := FWidthDesired;
    CBP.Height := FHeightDesired;
    ClearBackground(CBP);
  end;

  DrawBorder;

  if FSymbol.symbology = BARCODE_MAXICODE then
  begin
    DrawMaxiRings;
    DrawMaxiModules;
  end
  else
  begin
    DrawModules;
    if FHasText then
      DrawTexts;
  end;

  DrawStop;
  RemoveBoxModulesFromTargetUnits;
  RemoveSymbolOptions;
  RenderStop;
end;

{ TZintPersistent }

procedure TZintPersistent.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);

  if FOwner is TZintPersistent then
    TZintPersistent(FOwner).Changed;
end;

constructor TZintPersistent.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
end;

function TZintPersistent.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

end.


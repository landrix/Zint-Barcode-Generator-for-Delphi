unit zint_render_svg;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  zint, Classes, SysUtils, StrUtils;

type
  TZintSVGColor = String;
  TZintSVGFont = String;

  TZintSVGDocOption = (doSingleFile, //A brand new SVG Document is created including header etc. -- existing StringList content is deleted
                       doInsertIntoExisting, //Barcode will be inserted into existing SVG document before the closing </svg> tag
                       doAppendFlat  //Barcode information will be written in SVG Format without and opening/closing tags
                       );
  { TZintRenderTargetSVG }

  TZintRenderTargetSVG = class(TZintCustomRenderTarget)
  private
    FFontHeight: Single;
  protected
    FSVGFile : TStringList;
    FFormatSettings : TFormatSettings;
    FDocOption : TZintSVGDocOption;
    FIncludeDocInfo : Boolean;
    FFGColor: TZintSVGColor;
    FBGColor: TZintSVGColor;
    FFont: TZintSVGFont;

    
    procedure DrawStart; override;
    procedure DrawStop; override;
    procedure ClearBackground(const AParams : TZintClearBackgroundParams); override;
    procedure DrawRect(const AParams : TZintDrawRectParams); override;
    procedure DrawHexagon(const AParams : TZintDrawHexagonParams); override;
    procedure DrawRing(const AParams : TZintDrawRingParams); override;
    procedure DrawText(const AParams: TZintDrawTextParams); override;
    function CalcTextHeight(const AParams : TZintCalcTextHeightParams) : Single; override;
    function CalcTextWidth(const AParams : TZintCalcTextWidthParams) : Single; override;
  public
    constructor Create(ASVGFile: TStringList); reintroduce; virtual;
    procedure Render(ASymbol : TZintSymbol); override;
    property ForegroundColor : TZintSVGColor read FFGColor write FFGColor;
    property DocOption : TZintSVGDocOption read FDocOption write FDocOption;
    property BackgroundColor : TZintSVGColor read FBGColor write FBGColor;
    property Font: TZintSVGFont read FFont write FFont;
    property FontHeight: Single read FFontHeight write FFontHeight;
  end;

implementation

uses
  Types, zint_helper;


{ TZintRenderTargetSVG }

function TZintRenderTargetSVG.CalcTextHeight(
  const AParams: TZintCalcTextHeightParams): Single;
begin
  Result:=FFontHeight;
end;

function TZintRenderTargetSVG.CalcTextWidth(
  const AParams: TZintCalcTextWidthParams): Single;
begin
  Result:=FFontHeight*Length(AParams.Text); //we guess that;
end;

procedure TZintRenderTargetSVG.ClearBackground(
  const AParams: TZintClearBackgroundParams);
begin
  FSVGFile.Append(
          Format('<rect x="%f" y="%f" width="%f" height="%f" style="fill:%s;stroke-width:0"/>',
                      [FX,
                       FY,
                       FWidth,
                       FHeight,
                       FBGColor],FFormatSettings)
          );

end;

constructor TZintRenderTargetSVG.Create(ASVGFile: TStringList);
begin
  inherited Create(nil);
  FSVGFile:=ASVGFile;
  FDocOption:=doSingleFile;
  FFGColor:='black';
  FBGColor:='white';
  FFont:='sans-serif';
  FFontHeight:=11;
  FFormatSettings.DecimalSeparator:='.';
  FFormatSettings.ThousandSeparator:=#0;
end;

procedure TZintRenderTargetSVG.DrawHexagon(
  const AParams: TZintDrawHexagonParams);
begin
   FSVGFile.Append(
        Format('<polygon fill="%s" points="%f,%f %f,%f %f,%f %f,%f %f,%f %f,%f" />',
                 [FFGColor,
                  AParams.x-(AParams.width/2), AParams.y - AParams.height/4,
                  AParams.x-(AParams.width/2), AParams.y + AParams.height/4,
                  AParams.x -(AParams.width/2) + sqrt(3) * AParams.height / 4, AParams.y + AParams.height / 2,
                  AParams.x -(AParams.width/2) + sqrt(3) * AParams.height / 2, AParams.y + AParams.height / 4,
                  AParams.x -(AParams.width/2) + sqrt(3) * AParams.height / 2, AParams.y - AParams.height / 4,
                  AParams.x -(AParams.width/2) + sqrt(3) * AParams.height / 4, AParams.y - AParams.height / 2], FFormatSettings)
                  );
end;

procedure TZintRenderTargetSVG.DrawRect(const AParams: TZintDrawRectParams);
begin
  FSVGFile.Append(
        Format('<rect x="%f" y="%f" width="%f" height="%f" style="fill:%s;stroke-width:0"/>',
                      [AParams.x,
                        AParams.y,
                        AParams.Width,
                        AParams.Height,
                       FFGColor],FFormatSettings)
  );
end;

procedure TZintRenderTargetSVG.DrawRing(const AParams: TZintDrawRingParams);
begin
  FSVGFile.Append(Format('<circle cx="%f" cy="%f" r="%f" stroke="%s" stroke-width="%f" fill="none"/>',
                      [AParams.x,
                       AParams.y,
                       AParams.InnerRadius + (AParams.OuterRadius - AParams.InnerRadius) / 2,
                       FFGColor,
                       AParams.OuterRadius-AParams.InnerRadius],FFormatSettings));

end;

procedure TZintRenderTargetSVG.DrawStart;
begin
  case FDocOption of
    doSingleFile: begin
                    FSVGFile.Text:=
                      format('<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="%f" height="%f">',[FWidth, FHeight], FFormatSettings);
                  end;
    doInsertIntoExisting: begin
                            FSVGFile.Text:=ReplaceText(FSVGFile.Text, '</svg>', '');
                          end;
  end;

  FSVGFile.Append('<g><title>' + ArrayOfByteToString(FSymbol.text) + '</title><desc>Barcode generated using Zint</desc>');
end;

procedure TZintRenderTargetSVG.DrawStop;
begin
  FSVGFile.Append('</g>');
  if FDocOption<>doAppendFlat then
    FSVGFile.Append('</svg>');
end;

procedure TZintRenderTargetSVG.DrawText(const AParams: TZintDrawTextParams);
begin
  FSVGFile.Append(
   Format('<text x="%f" y="%f" fill="%s" font-family="%s" font-size="%f" style="text-anchor:middle">%s</text>',
               [AParams.x, AParams.y+AParams.Height/2, FFGColor, FFont, AParams.Height, AParams.Text], FFormatSettings)
               );


end;

procedure TZintRenderTargetSVG.Render(ASymbol: TZintSymbol);
begin
  inherited;
end;

end.

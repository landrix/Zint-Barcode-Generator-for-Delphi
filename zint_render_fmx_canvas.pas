unit zint_render_fmx_canvas;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0
}

interface

uses
  zint, FMX.Types, System.UITypes, System.Classes, FMX.Graphics,
  System.Math.Vectors,System.Types;

type

  { TZintCanvasRenderTarget }

  TZintCanvasRenderTarget = class(TZintCustomRenderTarget)
  private
    procedure SetCanvas(const Value: TCanvas);
  protected
    FCanvas : TCanvas;
    FFGColor: TAlphaColor;
    FBGColor: TAlphaColor;
    FFont: TFont;
    procedure ClearBackground(const AParams : TZintClearBackgroundParams); override;
    procedure DrawRect(const AParams : TZintDrawRectParams); override;
    procedure DrawHexagon(const AParams : TZintDrawHexagonParams); override;
    procedure DrawRing(const AParams : TZintDrawRingParams); override;
    procedure DrawText(const AParams: TZintDrawTextParams); override;
    function CalcTextHeight(const AParams : TZintCalcTextHeightParams) : Single; override;
    function CalcTextWidth(const AParams : TZintCalcTextWidthParams) : Single; override;
    procedure SetFont(const Value: TFont);
    procedure DrawStart; override;
    procedure DrawStop; override;
  public
    constructor Create(AOwner : TPersistent); override;
    property Canvas : TCanvas read FCanvas write SetCanvas;
    destructor Destroy; override;
  published
    property ForegroundColor : TAlphaColor read FFGColor write FFGColor;
    property BackgroundColor : TAlphaColor read FBGColor write FBGColor;
    property Font: TFont read FFont write SetFont;
  end;

implementation

{ TZintCanvasRenderTarget }

procedure TZintCanvasRenderTarget.ClearBackground(const AParams : TZintClearBackgroundParams);
begin
  FCanvas.Fill.Color:=FBGColor;
  FCanvas.Fill.Kind:=TBrushKind.Solid;
  FCanvas.Stroke.Kind:=TBrushKind.None;

  FCanvas.FillRect(RectF(FX,
                         FY,
                         FWidth,
                         FHeight), 0, 0, [], 1);
end;

procedure TZintCanvasRenderTarget.DrawRect(const AParams : TZintDrawRectParams);
begin
  FCanvas.Stroke.Kind:=TBrushKind.Solid;
  FCanvas.Stroke.Color:=ffgcolor;

  FCanvas.Fill.Color:=FFGColor;
  FCanvas.Fill.Kind:=TBrushKind.Solid;

  FCanvas.FillRect(RectF(AParams.x,
                        AParams.y,
                        AParams.X + AParams.Width,
                        AParams.y + AParams.Height), 0, 0, [], 1);

  FCanvas.DrawRect(RectF(AParams.x,
                        AParams.y,
                        AParams.X + AParams.Width,
                        AParams.y + AParams.Height), 0, 0, [], 1);
end;

procedure TZintCanvasRenderTarget.DrawHexagon(const AParams : TZintDrawHexagonParams);
var
  Points : TPolygon;
begin
  FCanvas.Fill.Color:=ffgcolor;
  FCanvas.Fill.Kind:=TBrushKind.Solid;

  SetLength(Points, 6);
  Points[0] := PointF(AParams.x-(AParams.width/2), AParams.y - AParams.height / 4);
  Points[1] := PointF(AParams.x-(AParams.width/2), AParams.y + AParams.height / 4);
  Points[2] := PointF(AParams.x -(AParams.width/2) + sqrt(3) * AParams.height / 4, AParams.y + AParams.height / 2);
  Points[3] := PointF(AParams.x -(AParams.width/2) + sqrt(3) * AParams.height / 2, AParams.y + AParams.height / 4);
  Points[4] := PointF(AParams.x -(AParams.width/2) + sqrt(3) * AParams.height / 2, AParams.y - AParams.height / 4);
  Points[5] := PointF(AParams.x -(AParams.width/2) + sqrt(3) * AParams.height / 4, AParams.y - AParams.height / 2);

  FCanvas.FillPolygon(Points, 1);
end;

procedure TZintCanvasRenderTarget.DrawRing(const AParams : TZintDrawRingParams);
var
  Radius : Single;
begin
    FCanvas.Stroke.Kind:=TBrushKind.Solid;
    FCanvas.Stroke.Color:=ffgcolor;

    FCanvas.Fill.Kind:=TBrushKind.None;

    FCanvas.Stroke.Thickness:=AParams.OuterRadius-AParams.InnerRadius;
    Radius:=AParams.InnerRadius+FCanvas.Stroke.Thickness/2;

    FCanvas.DrawEllipse(RectF(AParams.X-radius,
                        AParams.y-radius,
                        AParams.x + radius,
                        AParams.y + radius),1);
end;

procedure TZintCanvasRenderTarget.DrawStart;
begin
  inherited;
  FCanvas.BeginScene;
end;

procedure TZintCanvasRenderTarget.DrawStop;
begin
  inherited;
  FCanvas.EndScene;
end;

procedure TZintCanvasRenderTarget.DrawText(const AParams: TZintDrawTextParams);
begin
    FCanvas.Font.Assign(FFont);
    FCanvas.Fill.Kind:=TBrushKind.Solid;
    FCanvas.Fill.Color:=FFGColor;

    FCanvas.Font.Size:=AParams.Height;
    FCanvas.FillText(RectF(AParams.x,
                           AParams.y,
                           AParams.x+AParams.width,
                           AParams.y+AParams.Height),
                         AParams.text, false,1, [], TTextAlign.Center);
end;

function TZintCanvasRenderTarget.CalcTextHeight(const AParams : TZintCalcTextHeightParams) : Single;
begin
  FCanvas.Font.Assign(FFont);
  Result:=FCanvas.TextHeight(AParams.Text);
end;

function TZintCanvasRenderTarget.CalcTextWidth(const AParams : TZintCalcTextWidthParams) : Single;
begin
  FCanvas.Font.Assign(FFont);
  Result:=FCanvas.TextWidth(AParams.Text);
end;

procedure TZintCanvasRenderTarget.SetCanvas(const Value: TCanvas);
begin
  if Assigned(Value) then
  begin
    FWidthDesired:=Value.Width;
    FHeightDesired:=Value.Height;
  end;
  FCanvas := Value;
end;

procedure TZintCanvasRenderTarget.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

constructor TZintCanvasRenderTarget.Create(AOwner : TPersistent);
begin
  inherited;

  FFont:=TFont.Create;
  FFGColor := TAlphaColorRec.Black;
  FBGColor := TAlphaColorRec.White;
end;

destructor TZintCanvasRenderTarget.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

end.

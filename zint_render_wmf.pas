unit zint_render_wmf;

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
  zint, zint_render_canvas, Graphics, SysUtils;

type
  TZintRenderTargetWMF = class(TZintRenderTargetCanvas)
  protected
    FMetafile : TMetafile;
    procedure SetWMF(const Value: TMetafile); virtual;
    function CalcTextWidth(const AParams : TZintCalcTextWidthParams) : Single; override;
    procedure Inflate(const ANewWidth, ANewHeight : Single); override;
    procedure CreateCanvas;
  public
    procedure Render(ASymbol : TZintSymbol); override;

    property Metafile : TMetafile read FMetafile write SetWMF;
  end;

implementation

{ TZintRenderTargetWMF }

function TZintRenderTargetWMF.CalcTextWidth(
  const AParams: TZintCalcTextWidthParams): Single;
begin
  Result:=inherited CalcTextWidth(AParams);

  if Length(AParams.Text)=1 then
    Result:=Result * 2;
end;

procedure TZintRenderTargetWMF.Inflate(const ANewWidth, ANewHeight: Single);
begin
  if Assigned(FCanvas) then
    FreeAndNil(FCanvas);

  FMetafile.SetSize(Round(ANewWidth), Round(ANewHeight));

  CreateCanvas;
end;

procedure TZintRenderTargetWMF.CreateCanvas;
begin
  FCanvas:=TMetafileCanvas.Create(FMetafile, 0);
  FCanvas.Font.Assign(FFont);
end;

procedure TZintRenderTargetWMF.Render(ASymbol: TZintSymbol);
begin
  CreateCanvas;
  try
    inherited;
  finally
    FCanvas.Free;
  end;
end;

procedure TZintRenderTargetWMF.SetWMF(const Value: TMetafile);
begin
  if Assigned(Value) then
  begin
    WidthDesired := Value.Width;
    HeightDesired := Value.Height;
  end;
  FMetafile := Value;
end;

end.

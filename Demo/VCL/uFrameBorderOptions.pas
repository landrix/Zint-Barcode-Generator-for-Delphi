unit uFrameBorderOptions;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  zint;

type
  TFrameBorderOptions = class(TFrame)
    edTop: TEdit;
    edLeft: TEdit;
    edRight: TEdit;
    edBottom: TEdit;
    cbUnits: TComboBox;
    cbAllSame: TCheckBox;
    procedure edBottomChange(Sender: TObject);
    procedure cbAllSameClick(Sender: TObject);
    procedure edBottomKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FOnChange : TNotifyEvent;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure UpdateRenderBox(ARenderBox: TZintRenderBox);
  end;

implementation

{$R *.dfm}

procedure TFrameBorderOptions.cbAllSameClick(Sender: TObject);
begin
  if cbAllSame.Checked then
  begin
    edBottom.Text:=edTop.Text;
    edLeft.Text:=edTop.Text;
    edRight.Text:=edTop.Text;
  end;

  edBottom.Enabled:=not cbAllSame.Checked;
  edLeft.Enabled:=not cbAllSame.Checked;
  edRight.Enabled:=not cbAllSame.Checked;
end;

procedure TFrameBorderOptions.edBottomChange(Sender: TObject);
begin
  cbAllSameClick(Sender);

  if Assigned(FOnChange) then
    FOnChange(Self)
end;

procedure TFrameBorderOptions.edBottomKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  val : Single;
begin
  if ssShift in Shift then
    val := 0.5
  else
  if ssCtrl in Shift then
    val := 10
  else
    val := 1;

  if Key = VK_UP then
    TEdit(Sender).Text := FloatToStr(StrToFloatDef(TEdit(Sender).Text, 0) + val)
  else
  if Key = VK_DOWN then
    TEdit(Sender).Text := FloatToStr(StrToFloatDef(TEdit(Sender).Text, 0) - val)
end;

procedure TFrameBorderOptions.UpdateRenderBox(ARenderBox: TZintRenderBox);
begin
  if Assigned(ARenderBox) then
  begin
    if cbUnits.ItemIndex=0 then
    begin
      ARenderBox.SetTargetUnits(0);
      ARenderBox.Top.Modules:=StrToFloatDef(edTop.Text, 0);
      ARenderBox.Left.Modules:=StrToFloatDef(edLeft.Text, 0);
      ARenderBox.Bottom.Modules:=StrToFloatDef(edBottom.Text, 0);
      ARenderBox.Right.Modules:=StrToFloatDef(edRight.Text, 0);
    end
    else
    begin
      ARenderBox.SetModules(0);
      ARenderBox.Top.TargetUnits:=StrToFloatDef(edTop.Text, 0);
      ARenderBox.Left.TargetUnits:=StrToFloatDef(edLeft.Text, 0);
      ARenderBox.Bottom.TargetUnits:=StrToFloatDef(edBottom.Text, 0);
      ARenderBox.Right.TargetUnits:=StrToFloatDef(edRight.Text, 0);
    end;
  end;
end;

end.

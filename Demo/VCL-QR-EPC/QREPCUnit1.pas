unit QREPCUnit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs
  ,zint_qr_epc, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    procedure Button1Click(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  epcqr : TZintEPCQR;
  wmf : TMetafile;
  errorCode : Integer;
begin
  Image1.Picture.Bitmap.ReleaseHandle;

  epcqr := TZintEPCQR.Create;
  try
    epcqr.Name := Edit1.Text;
    epcqr.IBAN := Edit2.Text;
    epcqr.BIC := Edit3.Text;
    epcqr.Amount := StrToCurrDef(Edit4.Text,0);
    epcqr.PurposeOfPayment := Edit5.Text;
    if epcqr.GetQRCode(Image1.Width,Image1.Height,wmf,errorCode) then
      Image1.Picture.Graphic:=wmf
    else
      ShowMessage('Error: '+IntToStr(errorCode));
  finally
    epcqr.Free;
  end;
end;

end.

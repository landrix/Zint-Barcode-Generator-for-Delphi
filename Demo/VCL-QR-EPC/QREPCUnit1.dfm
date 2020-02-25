object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'EPC-QR Code'
  ClientHeight = 313
  ClientWidth = 727
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    727
    313)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 430
    Top = 16
    Width = 280
    Height = 280
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object Label1: TLabel
    Left = 16
    Top = 20
    Width = 141
    Height = 13
    Caption = 'Payee / Zahlungsempfaenger'
  end
  object Label2: TLabel
    Left = 16
    Top = 44
    Width = 24
    Height = 13
    Caption = 'IBAN'
  end
  object Label3: TLabel
    Left = 16
    Top = 68
    Width = 66
    Height = 13
    Caption = 'BIC (optional)'
  end
  object Label4: TLabel
    Left = 16
    Top = 95
    Width = 166
    Height = 13
    Caption = 'Payment amount / Zahlungsbetrag'
  end
  object Label5: TLabel
    Left = 16
    Top = 122
    Width = 201
    Height = 13
    Caption = 'Purpose of payment / Verwendungszweck'
  end
  object Button1: TButton
    Left = 16
    Top = 272
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Generate'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 224
    Top = 16
    Width = 200
    Height = 21
    MaxLength = 70
    TabOrder = 1
    Text = 'Supplier GmbH'
  end
  object Edit2: TEdit
    Left = 224
    Top = 40
    Width = 200
    Height = 21
    TabOrder = 2
    Text = 'DE76500105179227861124'
  end
  object Edit3: TEdit
    Left = 224
    Top = 64
    Width = 200
    Height = 21
    TabOrder = 3
    Text = 'INGDDEFFXXX'
  end
  object Edit4: TEdit
    Left = 224
    Top = 91
    Width = 200
    Height = 21
    TabOrder = 4
    Text = '120,55'
  end
  object Edit5: TEdit
    Left = 223
    Top = 118
    Width = 200
    Height = 21
    MaxLength = 140
    TabOrder = 5
    Text = 'Invoice 0815'
  end
end

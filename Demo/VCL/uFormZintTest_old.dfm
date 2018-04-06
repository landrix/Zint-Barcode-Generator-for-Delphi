object Form46: TForm46
  Left = 0
  Top = 0
  Caption = 'ZintTest'
  ClientHeight = 509
  ClientWidth = 777
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object imgResult: TImage
    AlignWithMargins = True
    Left = 3
    Top = 231
    Width = 771
    Height = 259
    Align = alClient
    AutoSize = True
    Center = True
    ParentShowHint = False
    Proportional = True
    ShowHint = False
    ExplicitLeft = -2
    ExplicitTop = 175
    ExplicitHeight = 312
  end
  object lblError: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 496
    Width = 771
    Height = 13
    Align = alBottom
    Alignment = taCenter
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 3
  end
  object Splitter1: TSplitter
    Left = 0
    Top = 225
    Width = 777
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 169
    ExplicitWidth = 321
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 777
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object edData: TEdit
      Left = 130
      Top = 8
      Width = 174
      Height = 21
      TabOrder = 0
      Text = '123456'
      OnChange = edDataChange
    end
    object comType: TComboBox
      Left = 310
      Top = 8
      Width = 297
      Height = 21
      Style = csDropDownList
      Sorted = True
      TabOrder = 1
      OnChange = comTypeChange
    end
    object btPrint: TButton
      Left = 613
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Print'
      DropDownMenu = pumPrint
      Style = bsSplitButton
      TabOrder = 2
      OnClick = btPrintClick
    end
    object btSVG: TButton
      Left = 694
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Save SVG'
      TabOrder = 3
      OnClick = btSVGClick
    end
    object edPrimary: TEdit
      Left = 4
      Top = 8
      Width = 120
      Height = 21
      TabOrder = 4
      OnChange = edDataChange
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 35
    Width = 777
    Height = 190
    ActivePage = TabSheet1
    Align = alTop
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'General Settings'
      DesignSize = (
        769
        162)
      object Label3: TLabel
        Left = 279
        Top = 6
        Width = 97
        Height = 13
        Caption = 'Render adjust Mode'
      end
      object Label4: TLabel
        Left = 279
        Top = 33
        Width = 152
        Height = 13
        Caption = 'Maxicode Hexagon Scale (float)'
      end
      object Label1: TLabel
        Left = 279
        Top = 60
        Width = 53
        Height = 13
        Caption = 'Horiz. align'
      end
      object Label2: TLabel
        Left = 279
        Top = 87
        Width = 49
        Height = 13
        Caption = 'Vert. align'
      end
      object Label5: TLabel
        Left = 483
        Top = 79
        Width = 21
        Height = 13
        Caption = 'Bars'
      end
      object Label6: TLabel
        Left = 483
        Top = 100
        Width = 59
        Height = 13
        Caption = 'Background '
      end
      object Label7: TLabel
        Left = 279
        Top = 114
        Width = 87
        Height = 13
        Caption = 'Min. Modulewidth:'
      end
      object Label8: TLabel
        Left = 279
        Top = 141
        Width = 91
        Height = 13
        Caption = 'Max. Modulewidth:'
      end
      object ButtonFont: TButton
        Left = 480
        Top = 3
        Width = 208
        Height = 44
        Caption = 'change Text Font'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = ButtonFontClick
      end
      object cbHRT: TCheckBox
        Left = 480
        Top = 53
        Width = 162
        Height = 17
        Caption = 'show human readable text'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = edDataChange
      end
      object cbRAM: TComboBox
        Left = 382
        Top = 3
        Width = 92
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 2
        Text = 'Scale'
        OnChange = edDataChange
        Items.Strings = (
          'Scale'
          'Inflate Image')
      end
      object edMHS: TEdit
        Left = 437
        Top = 30
        Width = 37
        Height = 21
        TabOrder = 3
        Text = '1'
        OnChange = edDataChange
      end
      object PageControl2: TPageControl
        Left = 3
        Top = 3
        Width = 270
        Height = 157
        ActivePage = TabSheet2
        Anchors = [akLeft, akTop, akBottom]
        TabOrder = 4
        object TabSheet2: TTabSheet
          Caption = 'Margin'
          inline fboMargin: TFrameBorderOptions
            Left = 0
            Top = 0
            Width = 262
            Height = 129
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 262
            ExplicitHeight = 129
          end
        end
        object TabSheet3: TTabSheet
          Caption = 'Padding'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
        object TabSheet4: TTabSheet
          Caption = 'Border'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
        object TabSheet5: TTabSheet
          Caption = 'TextSpacing'
          ImageIndex = 3
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
        object TabSheet6: TTabSheet
          Caption = 'Whitespace'
          ImageIndex = 4
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
      end
      object comHAlign: TComboBox
        Left = 338
        Top = 57
        Width = 136
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 5
        Text = 'Left'
        OnChange = edDataChange
        Items.Strings = (
          'Left'
          'Center'
          'Right')
      end
      object comVAlign: TComboBox
        Left = 338
        Top = 84
        Width = 136
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 6
        Text = 'Top'
        OnChange = edDataChange
        Items.Strings = (
          'Top'
          'Center'
          'Bottom')
      end
      object ColorBox1: TColorBox
        Left = 543
        Top = 76
        Width = 145
        Height = 22
        TabOrder = 7
        OnChange = edDataChange
      end
      object ColorBox2: TColorBox
        Left = 543
        Top = 97
        Width = 145
        Height = 22
        NoneColorColor = clWhite
        Selected = clWhite
        TabOrder = 8
        OnChange = edDataChange
      end
      object edMiMW: TEdit
        Left = 382
        Top = 111
        Width = 95
        Height = 21
        TabOrder = 9
        Text = '0'
        OnChange = edDataChange
      end
      object edMaMW: TEdit
        Left = 382
        Top = 138
        Width = 95
        Height = 21
        TabOrder = 10
        Text = '0'
        OnChange = edDataChange
      end
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 384
    Top = 256
  end
  object SaveDialog1: TSaveDialog
    Left = 464
    Top = 256
  end
  object pumPrint: TPopupMenu
    Left = 568
    Top = 256
  end
end

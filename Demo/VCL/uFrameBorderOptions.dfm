object FrameBorderOptions: TFrameBorderOptions
  Left = 0
  Top = 0
  Width = 249
  Height = 139
  Align = alClient
  TabOrder = 0
  object edTop: TEdit
    Left = 80
    Top = 3
    Width = 33
    Height = 21
    TabOrder = 0
    TextHint = 'Top'
    OnChange = edBottomChange
    OnKeyDown = edBottomKeyDown
  end
  object edLeft: TEdit
    Left = 3
    Top = 30
    Width = 33
    Height = 21
    TabOrder = 1
    TextHint = 'Left'
    OnChange = edBottomChange
    OnKeyDown = edBottomKeyDown
  end
  object edRight: TEdit
    Left = 151
    Top = 30
    Width = 33
    Height = 21
    TabOrder = 2
    TextHint = 'Right'
    OnChange = edBottomChange
    OnKeyDown = edBottomKeyDown
  end
  object edBottom: TEdit
    Left = 80
    Top = 57
    Width = 33
    Height = 21
    TabOrder = 3
    TextHint = 'Bottom'
    OnChange = edBottomChange
    OnKeyDown = edBottomKeyDown
  end
  object cbUnits: TComboBox
    Left = 42
    Top = 30
    Width = 103
    Height = 21
    Style = csDropDownList
    ItemIndex = 1
    TabOrder = 4
    Text = 'x Target Units'
    OnChange = edBottomChange
    Items.Strings = (
      'x Modules'
      'x Target Units')
  end
  object cbAllSame: TCheckBox
    Left = 119
    Top = 7
    Width = 97
    Height = 17
    Caption = 'all the same'
    TabOrder = 5
    OnClick = cbAllSameClick
  end
end

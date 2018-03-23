object FrameOptions: TFrameOptions
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  TabStop = True
  object ValueListEditor1: TValueListEditor
    Left = 0
    Top = 0
    Width = 451
    Height = 304
    Align = alClient
    TabOrder = 0
    OnGetPickList = ValueListEditor1GetPickList
    OnValidate = ValueListEditor1Validate
    ColWidths = (
      150
      295)
  end
end

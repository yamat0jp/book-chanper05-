object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 711
  ClientWidth = 884
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  TextHeight = 15
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 884
    Height = 25
    ActionManager = ActionManager1
    Caption = 'ActionToolBar1'
    Color = clMenuBar
    ColorMap.DisabledFontColor = 10461087
    ColorMap.HighlightColor = clWhite
    ColorMap.BtnSelectedFont = clWhite
    ColorMap.SelectedFontColor = clWhite
    ColorMap.UnusedColor = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    Spacing = 0
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = Action2
            Caption = '&Close'
          end>
      end
      item
        Items = <
          item
            Action = Action2
            Caption = '&Close'
          end>
        ActionBar = ActionToolBar1
      end>
    Left = 304
    Top = 224
    StyleName = 'Platform Default'
    object Action2: TAction
      Caption = 'Close'
      OnExecute = Action2Execute
    end
    object Action1: TAction
      Caption = 'Q'
    end
  end
end

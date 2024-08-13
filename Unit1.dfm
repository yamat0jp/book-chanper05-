object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
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
      end
      item
        Items = <
          item
            Action = Action1
            Caption = '&Action1'
          end
          item
            Action = Action2
            Caption = '&Close'
          end
          item
            Action = Action3
            Caption = 'Ac&tion3'
          end
          item
            Action = Action4
            Caption = 'Act&ion4'
          end
          item
            Action = Action5
            Caption = #12514#12531#12486#12459#12523#12525'(&Z)'
          end
          item
            Action = Action6
            Caption = 'T&D'
          end>
        ActionBar = ActionToolBar1
      end>
    Left = 288
    Top = 240
    StyleName = 'Platform Default'
    object Action1: TAction
      Caption = 'Action1'
      OnExecute = Action1Execute
    end
    object Action2: TAction
      Caption = 'Close'
      OnExecute = Action2Execute
    end
    object Action3: TAction
      Caption = 'Action3'
      OnExecute = Action3Execute
    end
    object Action4: TAction
      Caption = 'Action4'
      OnExecute = Action4Execute
    end
    object Action5: TAction
      Caption = #12514#12531#12486#12459#12523#12525
      OnExecute = Action5Execute
    end
    object Action6: TAction
      Caption = 'TD'
      OnExecute = Action6Execute
    end
  end
end

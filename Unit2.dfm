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
  object RadioButton1: TRadioButton
    Left = 752
    Top = 87
    Width = 113
    Height = 17
    Caption = 'action_meaning'
    TabOrder = 1
    OnClick = RadioButton1Click
  end
  object RadioButton2: TRadioButton
    Left = 752
    Top = 48
    Width = 113
    Height = 17
    Caption = 'action_reward'
    Checked = True
    TabOrder = 2
    TabStop = True
    OnClick = RadioButton1Click
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
          end
          item
            Action = Action1
            Caption = '&McAgent'
          end
          item
            Action = Action3
            Caption = '&Sarsa'
          end
          item
            Action = Action4
            Caption = '&OffPolicy'
          end
          item
            Action = Action5
            Caption = '&QLearning'
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
      Caption = 'McAgent'
      OnExecute = Action1Execute
    end
    object Action3: TAction
      Caption = 'Sarsa'
      OnExecute = Action3Execute
    end
    object Action4: TAction
      Caption = 'OffPolicy'
      OnExecute = Action3Execute
    end
    object Action5: TAction
      Caption = 'QLearning'
      OnExecute = Action5Execute
    end
  end
end

object Form1: TForm1
  Left = 192
  Top = 125
  Caption = 'Form1'
  ClientHeight = 480
  ClientWidth = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object MainMenu1: TMainMenu
    Left = 152
    Top = 32
    object Fille1: TMenuItem
      Caption = 'Vic20'
      object Reset1: TMenuItem
        Caption = 'Reset'
        OnClick = Reset1Click
      end
      object LoadRom01: TMenuItem
        Caption = 'Load Blitz (type run)'
        OnClick = LoadRom01Click
      end
      object Load1: TMenuItem
        Caption = 'Load Bonzo (type run)'
        OnClick = Load1Click
      end
      object LoadAvengers1: TMenuItem
        Caption = 'Load Rom Avenger'
        OnClick = LoadAvengers1Click
      end
      object LoadAvengers2: TMenuItem
        Caption = 'Load Rom Star Battle'
        OnClick = LoadAvengers2Click
      end
      object LoadSargonchessII1: TMenuItem
        Caption = 'Load Rom Sargon II'
        OnClick = LoadSargonchessII1Click
      end
      object LoadRomFile1: TMenuItem
        Caption = 'Load Rom from File'
        OnClick = LoadRomFile1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 152
    Top = 104
  end
end

object Form1: TForm1
  Left = 263
  Top = 374
  Caption = 'Form1'
  ClientHeight = 256
  ClientWidth = 522
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 64
    Top = 16
    Width = 145
    Height = 57
    Caption = 'Criar forms'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ComboBox1: TComboBox
    Left = 360
    Top = 214
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 1
    Text = 'Skin 1'
    OnChange = ComboBox1Change
    Items.Strings = (
      'Skin 1'
      'Skin 2'
      'Skin 3')
  end
end

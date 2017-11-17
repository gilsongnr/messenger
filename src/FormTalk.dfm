object frmTalk: TfrmTalk
  Left = 349
  Top = 112
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  Caption = 'frmTalk'
  ClientHeight = 550
  ClientWidth = 711
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBoxTop: TPaintBox
    Left = 0
    Top = 0
    Width = 707
    Height = 49
    OnPaint = PaintBoxTopPaint
  end
  object PaintBoxEditL: TPaintBox
    Left = 0
    Top = 401
    Width = 20
    Height = 96
    OnPaint = PaintBoxEditLPaint
  end
  object PaintBoxBottom: TPaintBox
    Left = 0
    Top = 502
    Width = 707
    Height = 46
    OnPaint = PaintBoxBottomPaint
  end
  object PaintBoxEditR: TPaintBox
    Left = 684
    Top = 401
    Width = 20
    Height = 96
    OnPaint = PaintBoxEditRPaint
  end
  object PaintBoxM: TPaintBox
    Left = 0
    Top = 339
    Width = 707
    Height = 61
    OnPaint = PaintBoxMPaint
  end
  object PaintBoxRight: TPaintBox
    Left = 684
    Top = 49
    Width = 20
    Height = 290
    OnPaint = PaintBoxRightPaint
  end
  object PaintBoxLeft: TPaintBox
    Left = 0
    Top = 49
    Width = 20
    Height = 290
    OnPaint = PaintBoxLeftPaint
  end
  object LDigitando: TLabel
    Left = 11
    Top = 513
    Width = 62
    Height = 13
    Caption = 'LDigitando'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object LabelCap: TLabel
    Left = 33
    Top = 14
    Width = 54
    Height = 13
    Caption = 'LabelCap'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 10056038
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object btnEnviar: TSpeedButton
    Left = 609
    Top = 507
    Width = 75
    Height = 25
    Caption = 'Enviar'
    Enabled = False
    Spacing = -2
    OnClick = btnEnviarClick
  end
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 24
    Height = 24
  end
  object PanelMSG: TPanel
    Left = 20
    Top = 49
    Width = 633
    Height = 262
    BevelOuter = bvNone
    TabOrder = 1
    object SplitterArqs: TSplitter
      Left = 0
      Top = 178
      Width = 633
      Height = 2
      Cursor = crVSplit
      Align = alBottom
      Visible = False
    end
    object PanelAcoes: TPanel
      Left = 0
      Top = 180
      Width = 633
      Height = 82
      Align = alBottom
      BevelOuter = bvNone
      Constraints.MinHeight = 80
      TabOrder = 0
      Visible = False
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 633
        Height = 17
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Envio/recebimento de arquivos'
        Color = clActiveCaption
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clCaptionText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object ScrollBoxAcoes: TScrollBox
        Left = 0
        Top = 17
        Width = 633
        Height = 65
        HorzScrollBar.Visible = False
        Align = alClient
        BorderStyle = bsNone
        TabOrder = 1
      end
    end
  end
  object PanelEdit: TPanel
    Left = 20
    Top = 400
    Width = 625
    Height = 101
    BevelOuter = bvNone
    Color = clWhite
    Constraints.MinHeight = 101
    TabOrder = 0
    object EditMsg: TMemo
      Left = 0
      Top = 21
      Width = 625
      Height = 80
      Align = alClient
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Lines.Strings = (
        'testet s'
        'fsfsfs sfsfsfs'
        'fsfsfs')
      ParentColor = True
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 0
      OnChange = EditMsgChange
      OnKeyDown = EditMsgKeyDown
      OnKeyPress = EditMsgKeyPress
    end
    object PanelAssistencia_: TPanel
      Left = 0
      Top = 0
      Width = 625
      Height = 21
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      Visible = False
      DesignSize = (
        625
        21)
      object LabelAss: TLabel
        Left = 361
        Top = 3
        Width = 130
        Height = 13
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'ASSIST'#202'NCIA  REMOTA:'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object btnAssistAceita: TButton
        Left = 495
        Top = 1
        Width = 64
        Height = 19
        Cursor = crHandPoint
        Anchors = [akTop, akRight]
        Caption = 'Aceitar'
        TabOrder = 0
      end
      object btnAssistRejeita: TButton
        Left = 559
        Top = 1
        Width = 64
        Height = 19
        Cursor = crHandPoint
        Anchors = [akTop, akRight]
        Caption = 'Rejeitar'
        TabOrder = 1
      end
    end
  end
end

{*******************************************************************
 Project: Messenger Forms, 
	Components e Classes helpers

  Copyright (C) 2017 Gilson Nunes Rodrigues

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  
  Gilson Nunes Rodrigues - gilson.gnr@gmail.com  
*******************************************************************}

unit Skins;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Windows, Types, Graphics, Helpers.Graphics;

type

  TArrayString = array of string;

  TSkinSpeakPart = class
  private
    FImg       : TPngImage;
    FCenter    : TPoint;
    FFontColor,
      //BackGroundColor,
        FDateColor: TColor;
  public
    constructor Create(const aResName: string; const aCenter: TPoint; aFontColor, aDateColor: TColor);
    destructor Destroy; override;
  private
    procedure DoDraw(aDC: HDC; const aRecSrc, aRecDst: TRect; const aCopyMode: TCopyMode);
  public
    procedure Draw(aCanvas: TCanvas; var aRect: TRect);
    property Img        : TPngImage   read FImg;
    property Center     : TPoint      read FCenter;
    property FontColor  : TColor      read FFontColor;
    property DateColor  : TColor      read FDateColor;
  end;

  TSkinSpeak = class
  public
    constructor Create();
    destructor Destroy; override;
  private
    type
      TParts = array[TAlignment] of TSkinSpeakPart;
  private
    FParts: TParts;
  public
    property Parts: TParts read FParts;
  end;

  { TSkin }

  TSkinKind = (skSkin1, skSkin2, skSkin3, skCustom);

  TSkin = class
  public
    constructor Create();
    destructor Destroy; override;
  private
    FImagTL : TBitmap;
    FImagTR : TBitmap;
    FImagBL : TBitmap;
    FImagBR : TBitmap;
    FImagML : TBitmap;
    FImagMR : TBitmap;
    FSpeak  : TSkinSpeak;
    FKind   : TSkinKind;
    procedure DoLoad(aIMG: TBitmap);
    procedure SetKind(const Value: TSkinKind);
  public
    property Speak    : TSkinSpeak    read FSpeak;
    property Kind     : TSkinKind     read FKind write SetKind;
    procedure LoadFromFile(const aFileName: TFileName);
    function GetColor(): TColor;
    function GetBottomHeight(): Integer;
    procedure DrawLeft(aCanvas: TCanvas; const aRect: TRect);
    procedure DrawTop(aCanvas: TCanvas; const aRect: TRect);
    procedure DrawRight(aCanvas: TCanvas; const aRect: TRect);
    procedure DrawMidle(aCanvas: TCanvas; const aRect: TRect);
    procedure DrawLeftB(aCanvas: TCanvas; const aRect: TRect);
    procedure DrawRightB(aCanvas: TCanvas; const aRect: TRect);
    procedure DrawBottom(aCanvas: TCanvas; const aRect: TRect);
  end;

implementation

{$R ..\resources\skins.RES}

{ TSkin }

constructor TSkin.Create();
begin
  inherited;
  FImagTL := TBitmap.Create;
  FImagTR := TBitmap.Create;
  FImagML := TBitmap.Create;
  FImagMR := TBitmap.Create;
  FImagBL := TBitmap.Create;
  FImagBR := TBitmap.Create;
  FSpeak  := TSkinSpeak.Create;
  FKind   := skCustom;
end;

destructor TSkin.Destroy;
begin
  FSpeak.Free;
  FImagTL.Free;
  FImagTR.Free;
  FImagBL.Free;
  FImagBR.Free;
  FImagML.Free;
  FImagMR.Free;
  inherited;
end;

procedure TSkin.DoLoad(aIMG: TBitmap);
const
  cwLeft   = 20;
  chTop    = 49;
  chMeio   = 61;
  chBottom = 46;

  procedure _PrepareBmp(bmp : TBitmap; X, Y, W, H : Integer);
  begin
    bmp.Canvas.Brush.Color := clRed;
    bmp.Width  := W;
    bmp.Height := H;
    bmp.Canvas.FillRect(Rect(0, 0, W, H));
    bmp.Canvas.CopyRect(Rect(0, 0, W, H), aIMG.Canvas, Rect(X, Y, X + W, Y + H));
  end;

var
  WR : Integer;
begin
  WR := aIMG.Width - cwLeft;

  if WR <= cwLeft then
    raise Exception.Create('Largura de imagem inválida');

  if aIMG.Height <> chTop + chMeio + chBottom + 2 then
    raise Exception.Create('Tamanho de imagem inválido');

  _PrepareBmp(FImagTL, 0, 0, cwLeft, chTop);
  _PrepareBmp(FImagTR, cwLeft, 0, WR, chTop);
  _PrepareBmp(FImagBL, 0, chTop + chMeio + 1, cwLeft, chBottom + 1);
  _PrepareBmp(FImagBR, cwLeft, chTop + chMeio + 1, WR, chBottom + 1);
  _PrepareBmp(FImagML, 0, chTop, cwLeft, chMeio + 1);
  _PrepareBmp(FImagMR, cwLeft, chTop, WR, chMeio + 1);
end;

procedure TSkin.SetKind(const Value: TSkinKind);
const
 sSkinNames : array [TSkinKind] of string = ('BMP_Skin1', 'BMP_Skin2', 'BMP_Skin3', '');
var
  b: TBitmap;
begin
  if FKind = Value then Exit;

  if Value <> skCustom then
  begin
    b := TBitmap.Create;
    try
      b.LoadFromResourceName(HInstance, sSkinNames[Value]);
      DoLoad(b);
    finally
      b.Free;
    end;
  end;
  FKind := Value;
end;

procedure TSkin.LoadFromFile(const aFileName: TFileName);
var
  b: TBitmap;
begin
  b := TBitmap.Create;
  try
    b.LoadFromFile(aFileName);
    DoLoad(b);
  finally
    b.Free;
  end;
  FKind := skCustom;
end;

function TSkin.GetColor(): TColor;
begin
  Result := FImagBR.Canvas.Pixels[0, 0];
end;

function TSkin.GetBottomHeight(): Integer;
begin
  Result := FImagBL.Height;
end;

procedure TSkin.DrawLeft(aCanvas: TCanvas; const aRect: TRect);
begin
  aCanvas.CopyRect(Rect(aRect.Left, aRect.Top, aRect.Left + FImagML.Width, aRect.Bottom), FImagML.Canvas,
                                                                       Rect(0, 0, FImagML.Width, 1));
end;

procedure TSkin.DrawTop(aCanvas: TCanvas; const aRect: TRect);
begin
  aCanvas.Draw(aRect.Left, aRect.Top, FImagTL);
  aCanvas.CopyRect(Rect(aRect.Left + FImagTL.Width, aRect.Top, aRect.Right - FImagTL.Width, aRect.Bottom), FImagTR.Canvas,
                               Rect(0, 0, 1, aRect.Bottom - aRect.Top));
  aCanvas.Draw(aRect.Right - FImagTR.Width, aRect.Top, FImagTR);
end;

procedure TSkin.DrawRight(aCanvas: TCanvas; const aRect: TRect);
begin
  aCanvas.CopyRect(aRect, FImagMR.Canvas, Rect(FImagMR.Width - (aRect.Right - aRect.Left), 0, FImagMR.Width, 1));
end;

procedure TSkin.DrawMidle(aCanvas: TCanvas; const aRect: TRect);
begin
  aCanvas.Draw(aRect.Left, aRect.Top, FImagML);
  aCanvas.CopyRect(Rect(aRect.Left + FImagML.Width, aRect.Top, aRect.Right - FImagMR.Width, aRect.Bottom),
                                              FImagMR.Canvas, Rect(0, 0, 1, aRect.Bottom - aRect.Top));

  aCanvas.Draw(aRect.Right - FImagMR.Width, aRect.Top, FImagMR);
end;

procedure TSkin.DrawLeftB(aCanvas: TCanvas; const aRect: TRect);
begin
  aCanvas.CopyRect(Rect(aRect.Left, aRect.Top, aRect.Left + FImagML.Width, aRect.Bottom), FImagBL.Canvas,
                                                                               Rect(0, 0, FImagBL.Width, 1));
end;

procedure TSkin.DrawRightB(aCanvas: TCanvas; const aRect: TRect);
begin
  aCanvas.CopyRect(aRect, FImagBR.Canvas, Rect(FImagBR.Width - (aRect.Right - aRect.Left), 0, FImagBR.Width, 1));
end;

procedure TSkin.DrawBottom(aCanvas: TCanvas; const aRect: TRect);
begin
  aCanvas.Draw(aRect.Left, aRect.Top, FImagBL);
  aCanvas.CopyRect(Rect(aRect.Left + FImagBL.Width, aRect.Top, aRect.Right - FImagBR.Width, aRect.Bottom),
                                              FImagBR.Canvas, Rect(0, 0, 1, aRect.Bottom - aRect.Top));
  aCanvas.Draw(aRect.Right - FImagBR.Width, aRect.Top, FImagBR);
end;

{ TSkinSpeakPart }

constructor TSkinSpeakPart.Create(const aResName: string; const aCenter: TPoint; aFontColor, aDateColor: TColor);
begin
  inherited Create;
  FImg  := TPngImage.Create;
  FImg.LoadFromResourceName(HInstance, aResName);
  FCenter    := aCenter;
  FFontColor := aFontColor;
  FDateColor := aDateColor;
end;

destructor TSkinSpeakPart.Destroy;
begin
  Img.Free;
  inherited;
end;

procedure TSkinSpeakPart.DoDraw(aDC: HDC; const aRecSrc, aRecDst: TRect; const aCopyMode: TCopyMode);
begin
  Img.DrawDC(aDC, aRecSrc, aRecDst, aCopyMode);
end;

procedure TSkinSpeakPart.Draw(aCanvas: TCanvas; var aRect: TRect);
begin
  aCanvas.FillRect(aRect);
  aCanvas.DrawSkin(DoDraw, aRect, Img.Width, Img.Height, Center);
  Inc(aRect.Left, Center.x - 4);
  Dec(aRect.Right, Img.Width - 4 - Center.x);
end;

{ TSkinSpeak }

constructor TSkinSpeak.Create();
begin
  inherited;
  FParts[taLeftJustify]   := TSkinSpeakPart.Create('PNG_SkinTalkL', Point(19, 21), clBlack, clGrayText);
  FParts[taRightJustify]  := TSkinSpeakPart.Create('PNG_SkinTalkR', Point(12, 21), clWhite, $00AAD7DD);
  FParts[taCenter]        := TSkinSpeakPart.Create('PNG_SkinTalkM', Point(12, 12), clBlack, clGrayText);
end;

destructor TSkinSpeak.Destroy;
begin
  FParts[taLeftJustify].Free;
  FParts[taRightJustify].Free;
  FParts[taCenter].Free;
  inherited;
end;

end.

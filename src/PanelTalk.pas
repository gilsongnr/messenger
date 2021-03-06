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

unit PanelTalk;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Graphics, StdCtrls, Clipbrd, Windows, Controls, ExtCtrls, Menus, Skins, Types,
  {$IFNDEF FPC}
  ShellApi,
  {$ENDIF}
  Helpers.Graphics, Helpers.Menus, Math;

type

  TOrigType  = (otSended, otReceived, otInfo);
  TPanelTalk = class;

  { TControlTalk }

  TControlTalk = class(TPaintBox)
  private
    FPanel     : TPanelTalk;
    FText      : String;
    FFTextPos  : Integer;
    FOrig      : TOrigType;
    FSkin      : TSkinSpeakPart;
  public
    constructor Create(aOwner: TPanelTalk; const aText: String; aType: TOrigType; aColor: TColor); reintroduce;

    procedure UpdateSize(); virtual;
    class procedure DrawText(const aText : string; aCanvas : TCanvas; var vRect: TRect; aCalcRect : Boolean);
    procedure Paint; override;
  public
    property Orig      : TOrigType  read FOrig;
    function GetAlignment() : TAlignment;
  end;

  TSpeakTalk    = class(TControlTalk)
  public
    constructor Create(AOwner: TPanelTalk; const aText: String; aType: TOrigType; aOlded: Boolean; aDateTime: TDateTime); reintroduce;
  strict private
    FSelected : Boolean;
  private
    FTimeStr  : String;
    FDateTime : TDateTime;
    FOlded    : Boolean;
    FTextPos  : Integer;
    FLink     : string;
  public
    destructor Destroy; override;
    procedure UpdateSize(); override;
    procedure Paint; override;
  protected
    procedure DblClick; override;
    procedure Click; override;
  private
    procedure DoSetSelected(aValue: Boolean);
    procedure SetSelected(aValue : Boolean);
    procedure InvertSelected();
  public
    property Selected: Boolean read FSelected;
  end;

  TFocusType = (ftNo, ftYes, ftForce);

  { TPanelTalk }

  TPanelTalk = class(TScrollBox)
  public
    constructor Create(TheOwner: TComponent; aSkin: TSkinSpeak; const aNikName: string); reintroduce;
  private
    FHintWindow     : TStaticText;//THintWindow;
    FNikName        : string;
    FControlLast    : TControlTalk;
    FSpeakSkin      : TSkinSpeak;
    FControlPosProx : Integer;
    FPanelWidth     : Integer;
    FSelectCount    : Integer;
    FDateLast       : TDate;
    miCopySel       : TMenuItem;
    miCopyLink      : TMenuItem;
    miCopyText      : TMenuItem;
    procedure SelectChange(aInc: Boolean);
    procedure PopupMenuPopup(Sender : TObject);
    procedure miCopySelClick(Sender : TObject);
    procedure miCopyClick(Sender : TObject);
    procedure HintWindowClick(Sender : TObject);
    function CalcPanelWidth() : Integer;
    function CalcSpeakWidthMax() : Integer;
  public
    procedure Clear;
    procedure ShowAviso(const aText : string; {aColorBackGround,}aColorText: TColor; const aDateTime : TDateTime = 0); overload;
    procedure ShowAviso(const aText : string; const aDateTime : TDateTime = 0); overload;
    procedure ShowError(const aText : string);
    function Add(const aText : string; const aDateTime : TDateTime; aType : TOrigType; aOlded : Boolean;
                                                                                aFocus : TFocusType = ftYes) : TSpeakTalk;
    procedure AddMsg(const aText : string; const aDateTime : TDateTime; aReceived, aOlded : Boolean; aFocus : TFocusType = ftYes);
    procedure MoveToLast(aHint : TFocusType);
    function GetLastTextSended() : string;
  protected
    procedure Resize; override;
  end;

implementation

{ TControlTalk }

constructor TControlTalk.Create(AOwner: TPanelTalk; const aText : String; aType : TOrigType; aColor : TColor);
begin
  inherited Create(AOwner);
  FPanel        := AOwner;
  FText         := Trim(aText);
  Color         := aColor;
  FOrig         := aType;
  FSkin         := AOwner.FSpeakSkin.Parts[GetAlignment()];
end;

class procedure TControlTalk.DrawText(const aText: string; aCanvas: TCanvas; var vRect: TRect; aCalcRect: Boolean);
var
  vFlags: Integer;
begin
  vFlags := DT_EXPANDTABS or DT_WORDBREAK;
  if aCalcRect then
    vFlags := vFlags or DT_CALCRECT;

  Windows.DrawText(aCanvas.Handle, PChar(aText), Length(aText), vRect, vFlags or DT_EDITCONTROL);
end;

procedure TControlTalk.UpdateSize();
var
  DC : HDC;
  r  : TRect;
begin
  r             := Rect(0, 0, FPanel.CalcSpeakWidthMax(), 0);
  DC            := GetDC(0);
  Canvas.Handle := DC;
  Canvas.Font   := Font;
  DrawText(FText, Canvas, r, True);

  Canvas.Handle := 0;
  ReleaseDC(0, DC);

  FFTextPos := 2;
  Width  := r.Right  + FFTextPos * 2;
  Height := r.Bottom + FFTextPos * 2;
end;

procedure TControlTalk.Paint;
var
  r: TRect;
begin
  r                  := ClientRect;
  Canvas.Font        := Font;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(r);
  InflateRect(r, -FFTextPos, -FFTextPos);
  Canvas.Brush.Style := bsClear;
  DrawText(FText, Canvas, r, False);
end;

function TControlTalk.GetAlignment() : TAlignment;
const
  cAlignments : array[TOrigType] of TAlignment = (taRightJustify, taLeftJustify, taCenter);
begin
  Result := cAlignments[Orig];
end;

{ TSpeakTalk }

constructor TSpeakTalk.Create(AOwner: TPanelTalk; const aText: String; aType: TOrigType; aOlded: Boolean; aDateTime: TDateTime);
var
  s, s2 : string;
  i, i2 : Integer;
begin
  inherited Create(AOwner, aText, aType, AOwner.Color);

  FOlded     := aOlded;
  FDateTime  := aDateTime;
  PopupMenu  := AOwner.PopupMenu;
  Self.Font  := AOwner.Font;
  Font.Color := FSkin.FontColor;
  if FDateTime = 0 then
    FTimeStr := ''
  else
    FTimeStr  := FormatDateTime('hh:mm', aDateTime);

  FLink  := '';
  for s in TArrayString.Create('http:', 'https:', 'www.') do
  begin
    //i := TStrUtils.GetFirstPos(s, FText, #0, True);
    //if i > 0 then
    //begin
    //  i2 := i + Length(s) + 1;
    //  while (i2 < Length(FText)) and not (FText[i2] in [#0..' ', ',', '(', ')', '[', ']']) do
    //    Inc(i2);
    //
    //  if i2 > i then
    //    s2 := Copy(FText, i, i2 - i + 1)
    //  else
    //    s2 := Copy(FText, i);
    //
    //  if Length(s2) > Length(s) then
    //  begin
    //    Cursor  := crHandPoint;
    //    FLink   := s2;
    //    Break;
    //  end;
    //end;
  end;
end;

destructor TSpeakTalk.Destroy;
begin
  if Selected then
    if FPanel <> nil then
      FPanel.SelectChange(False);

  inherited;
end;  

procedure TSpeakTalk.UpdateSize();
var
  DC: HDC;
  r: TRect;
  w : Integer;
  vWidth : Integer;
begin
  vWidth        := FPanel.CalcSpeakWidthMax();
  r             := Rect(0, 0,  vWidth, 0);
  DC            := GetDC(0);
  Canvas.Handle := DC;
  Canvas.Font   := Font;
  DrawText(FText, Canvas, r, True);

  if FTimeStr <> '' then
  begin
    w := ((Canvas.TextWidth(FTimeStr) * 3) div 2) + r.Right;

    if w < vWidth then
      r.Right := w
    else
    begin
      Inc(r.Bottom, Canvas.TextHeight(FTimeStr));
      r.Right := vWidth;
    end;
  end;

  Canvas.Handle := 0;
  ReleaseDC(0, DC);

  FTextPos := 6;
  Inc(r.Bottom, FTextPos * 2);

  if r.Bottom < FSkin.Img.Height then
  begin
    Inc(FTextPos, (FSkin.Img.Height - r.Bottom) div 2);
    r.Bottom := FSkin.Img.Height;
  end;

  Width  := r.Right + FSkin.Img.Width - 8;
  Height := r.Bottom;
end;

procedure TSpeakTalk.Paint;
var
  r: TRect;
begin
  if FPanel.FHintWindow <> nil then
    if FPanel.FHintWindow.Owner = Self then
      if FPanel.ClientHeight - Top > Height div 2 then
        FreeAndNil(FPanel.FHintWindow);

  r                  := ClientRect;
  Canvas.Font        := Font;
  Canvas.Brush.Color := Color;
  FSkin.Draw(Canvas, r);
  InflateRect(r, 0, -FTextPos);
  Canvas.Brush.Style := bsClear;

  DrawText(FText, Canvas, r, False);

  if FTimeStr <> '' then
  begin
    Inc(r.Bottom, 5);
    Inc(r.Right, 5);
    r.Left := Max(r.Left, r.Right - Canvas.TextWidth(FTimeStr) - 4);
    r.Top  := Max(r.Top, r.Bottom - Canvas.TextHeight(FTimeStr) - 4);
    Canvas.Font.Color  := FSkin.DateColor;
    Canvas.Pen.Color   := Canvas.Font.Color;
    Canvas.Pen.Width   := 1;
    Canvas.TextRect(r, r.Left + 2, r.Top + 2, FTimeStr);
  end;

  if FOlded then
    Canvas.FillRect(ClientRect, clBlack, Color, 100);

  if Selected then
    Canvas.FillRect(ClientRect, $BBBB00, clNone, 150);
end;

procedure TSpeakTalk.DblClick;
begin
  inherited;
  InvertSelected();
end;

procedure TSpeakTalk.Click;
begin
  inherited;
  if FPanel.FSelectCount > 0 then
    InvertSelected()
  else if FLink <> '' then
    ShellExecute(0, 'open', PChar(FLink), nil, nil, SW_SHOW);
end;

procedure TSpeakTalk.DoSetSelected(aValue: Boolean);
begin
  FSelected := aValue;
  Invalidate;
  FPanel.SelectChange(aValue);
end;

procedure TSpeakTalk.SetSelected(aValue: Boolean);
begin
  if FSelected <> aValue then
    DoSetSelected(aValue);
end;

procedure TSpeakTalk.InvertSelected();
begin
  DoSetSelected(not FSelected);
end;

{ TPanelTalk }

constructor TPanelTalk.Create(TheOwner: TComponent; aSkin: TSkinSpeak; const aNikName: string);
begin
  inherited Create(TheOwner);
  DoubleBuffered        := True;
  FNikName              := aNikName;
  FSpeakSkin            := aSkin;
  Align                 := alClient;
  Font.Color            := clBlack;
  Font.Height           := -15;
  HorzScrollBar.Visible := False;
  TabOrder              := 0;
  PopupMenu             := TPopupMenu.Create(Self);
  with PopupMenu.Items do
  begin
    miCopySel  := AddNewItem('Copiar Sele��o', miCopySelClick);
    miCopyText := AddNewItem('Copiar mensagem', miCopyClick);
    miCopyLink := AddNewItem('Copiar link', miCopyClick);
  end;
  PopupMenu.OnPopup := PopupMenuPopup;
  VertScrollBar.Increment := 20;
end;

procedure TPanelTalk.SelectChange(aInc : Boolean);
begin
  if aInc then
    Inc(FSelectCount)
  else
    Dec(FSelectCount);
end;

procedure TPanelTalk.PopupMenuPopup(Sender : TObject);
begin
  if not (PopupMenu.PopupComponent is TSpeakTalk) then
  begin
    miCopySel.Visible  := True;
    miCopySel.Enabled  := FSelectCount > 0;
    miCopyText.Visible := False;
    miCopyLink.Visible := False;
  end
  else
  begin
    Self.miCopySel.Visible := FSelectCount > 0;
    with TSpeakTalk(PopupMenu.PopupComponent) do
    begin
      Self.miCopyLink.Visible := FLink <> '';
      Self.miCopyText.Visible := (FLink = '') or (FLink <> FText);
    end;
  end;
end;

procedure TPanelTalk.miCopySelClick(Sender : TObject);  var
  i, j : Integer;
  s, s2 : string;
  vSels : array of TSpeakTalk;
  vSel  : TSpeakTalk;
  d : TDate;
begin
  s := '';
  j := 0;
  d := 0;
  SetLength(vSels, FSelectCount);
  for i := 0 to ControlCount - 1 do
    if Controls[i] is TSpeakTalk then
    begin
      if j >= FSelectCount then
        Break;

      vSel     := TSpeakTalk(Controls[i]);
      vSels[j] := vSel;
      if vSel.Selected then
      begin
        if FSelectCount = 1 then
        begin
          s := vSel.FText;
          Break;
        end
        else
        begin
          case vSel.Orig of
            otReceived : s2 := FNikName + ' disse';
            otSended : s2 := 'Voc� disse';
            else s2 := 'Mensagem';
          end;

          if d <> Trunc(vSel.FDateTime) then
          begin
            d := Trunc(vSel.FDateTime);
            s := s + sLineBreak + '['+ DateToStr(d) + ']' + sLineBreak + sLineBreak;
          end;

          s2 := vSel.FTimeStr + ' ' + s2  +': '+vSel.FText+sLineBreak+StringOfChar('-', 30)+sLineBreak;
        end;
        s := s + s2;
        Inc(j);
      end;
    end;

  Clipboard.AsText := s;

  for i := 0 to j - 1 do
    vSels[i].SetSelected(False);
end;

procedure TPanelTalk.miCopyClick(Sender : TObject);
begin
  if not (PopupMenu.PopupComponent is TSpeakTalk) then
    Exit;

  with TSpeakTalk(PopupMenu.PopupComponent) do
  begin
    if Sender = miCopyLink then
      Clipboard.AsText := FLink
    else
      Clipboard.AsText := FText;
  end;
end;

procedure TPanelTalk.HintWindowClick(Sender: TObject);
begin
  if not Self.VertScrollBar.IsScrollBarVisible then
    Exit;

  if FHintWindow.Owner is TControlTalk then
  begin
    with TControlTalk(FHintWindow.Owner) do
      Self.VertScrollBar.Position := Self.VertScrollBar.Position + Top;
  end;
end;

function TPanelTalk.CalcPanelWidth: Integer;

  function ControlScrollBarGetSize(Kind: TScrollBarKind): Integer;
  const
    SysConsts: array[TScrollBarKind] of Integer = (SM_CYHSCROLL, SM_CYVSCROLL);
  begin
    Result := GetSystemMetrics(SysConsts[Kind]);
  end;

begin
  Result := Self.Width - 4 - ControlScrollBarGetSize(VertScrollBar.Kind);
end;

function TPanelTalk.CalcSpeakWidthMax() : Integer;
const
  cMargemSpeak = 100;
begin
  if FPanelWidth = 0 then
    FPanelWidth := CalcPanelWidth();

  Result := FPanelWidth - cMargemSpeak;
end;

procedure TPanelTalk.Clear;
var
  i : Integer;
begin
  for i := ControlCount - 1 downto 0 do
    if Controls[i] is TControlTalk then
      Controls[i].Free;
end;

procedure TPanelTalk.ShowAviso(const aText : string; {aColorBackGround,}aColorText : TColor; const aDateTime : TDateTime);
begin
  Add(aText, aDateTime, otInfo, False).Font.Color := aColorText;
end;

procedure TPanelTalk.ShowAviso(const aText: string; const aDateTime : TDateTime);
begin
  Add(aText, aDateTime, otInfo, False);
end;

procedure TPanelTalk.ShowError(const aText: string);
begin
  Add(aText, 0, otInfo, False).Font.Color := clRed;
end;

function TPanelTalk.Add(const aText : string; const aDateTime : TDateTime; aType : TOrigType; aOlded : Boolean;
                                                                                      aFocus : TFocusType) : TSpeakTalk;

  procedure _Add(aControl: TControlTalk);
  var
    x : Integer;
  begin
    aControl.Parent := Self;
    aControl.UpdateSize();

    case aControl.GetAlignment() of
      taRightJustify: x :=  FPanelWidth - aControl.Width - 5;
      taCenter      : x := (FPanelWidth - aControl.Width) div 2;
      else            x := 5;
    end;

    Inc(FControlPosProx, 5);
    aControl.Left  := x - HorzScrollBar.Position;
    aControl.Top   := FControlPosProx - VertScrollBar.Position;
    Inc(FControlPosProx, aControl.Height);
  end;

  procedure _CheckDate();
  var
    s : TControlTalk;
    d : TDate;
  begin
    d := Trunc(aDateTime);
    if d = 0 then
      Exit;

    if d = FDateLast then
      Exit;

    FDateLast := d;
    s         := TControlTalk.Create(Self, FormatDateTime('dddd, d '' de ''mmmm'' de '' yyyy', d),
                                                                  otInfo, ColorAdjustLuma(Self.Color, -30, True));
    s.Font.Size  := 8;
    s.Font.Name  := 'Arial';
    s.Font.Color := clBlack;
    _Add(s);
  end;

begin
  _CheckDate();

  Result            := TSpeakTalk.Create(Self, aText, aType, aOlded, aDateTime);
  _Add(Result);

  FControlLast := Result;
  if aFocus <> ftNo then
    MoveToLast(aFocus);
end;

{ TPanelTalk }

procedure TPanelTalk.AddMsg(const aText: string; const aDateTime: TDateTime; aReceived, aOlded: Boolean; aFocus: TFocusType);
const
  cOrigs : array[Boolean] of TOrigType = (otSended, otReceived);
begin
  Add(aText, aDateTime, cOrigs[aReceived], aOlded, aFocus)
end;

procedure TPanelTalk.MoveToLast(aHint : TFocusType);
var
  y : Integer;
const
  cHint = 'Nova mensagem pra voc�';
begin
  if not Self.VertScrollBar.IsScrollBarVisible then
    Exit;

  if FControlLast = nil then
    Exit;

  y := FControlPosProx - Self.ClientHeight;
  if (aHint = ftForce) or (y - Self.VertScrollBar.Position - FControlLast.Height < 100) then
    Self.VertScrollBar.Position := y
  else if (aHint = ftYes) and (FHintWindow = nil) then
  begin
    FHintWindow := TStaticText.Create(FControlLast);
    FHintWindow.AutoSize := True;
    FHintWindow.Color  := Application.HintColor;
    FHintWindow.Transparent := False;
    FHintWindow.Parent := Self.Parent;
    FHintWindow.Font.Size := 12;
    FHintWindow.Font.Name := 'Arial';
    FHintWindow.BorderStyle := sbsSingle;
    FHintWindow.Caption := cHint;
    FHintWindow.Left    := Self.Left + (Self.Width - FHintWindow.Width) div 2;
    FHintWindow.Top     := Self.Top  +  Self.Height - FHintWindow.Height;
    FHintWindow.Show;
    FHintWindow.BringToFront;
    FHintWindow.Cursor  := crHandPoint;
    FHintWindow.OnClick := HintWindowClick;
  end;
end;

function TPanelTalk.GetLastTextSended(): string;
var
  c : TControl;
  p : TControlTalk;
  i : Integer;
begin
  for i := ControlCount - 1 downto 0 do
  begin
    c     := Controls[i];

    if c is TSpeakTalk then
    begin
      p := TControlTalk(Controls[i]);

      if p.Orig = otSended then
      begin
        Result := p.FText;
        Exit;
      end;
    end;
  end;
  Result := '';
end;

procedure TPanelTalk.Resize;
var
  wDif, i, vInc, w, h, wMax : Integer;
  c : TControl;
  p : TControlTalk;
begin
  inherited;
  if FPanelWidth = 0 then
    Exit;

  i := CalcPanelWidth();
  if i <> FPanelWidth then
  begin
    wDif := i - FPanelWidth;
    if wDif < 6 then
      if wDif > -6 then
        Exit;


    FPanelWidth := i;

    {$IFDEF FPC}
    DisableAutoSizing;
    {$ELSE}
    DisableAutoRange;
    {$ENDIF}
    try
      wMax := CalcSpeakWidthMax();
      vInc := 0;
      for i := 0 to ControlCount - 1 do
      begin
        c     := Controls[i];
        c.Top := c.Top + vInc;

        if c is TControlTalk then
        begin
          p := TControlTalk(Controls[i]);
          w := p.Width;
          if (w <= wMax) and (wDif < 0) then
            w := 0
          else
          begin
            h := p.Height;
            p.UpdateSize();
            Inc(vInc, p.Height - h);
            w := p.Width - w;
          end;

          case p.GetAlignment() of
            taRightJustify: p.Left := p.Left - (w - wDif);
            taCenter      : p.Left := p.Left - (w - wDif) div 2;
          end;
        end;
      end;
    finally
      {$IFDEF FPC}
      EnableAutoSizing();
      {$ELSE}
      EnableAutoRange();
      {$ENDIF}
    end;
  end;
end;

end.

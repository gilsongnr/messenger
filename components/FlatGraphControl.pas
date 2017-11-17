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

unit FlatGraphControl;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LMessages, LCLIntf, LCLType,
  {$ELSE}
  Messages, Windows,
  {$ENDIF}
  Classes, SysUtils,  Graphics, Controls, Themes, DwmApi, Forms,
  Helpers.Graphics, Types;

type
  {$IFNDEF FPC}
  TLMessage = TMessage;
  {$ENDIF}

  TFlatGraphControl = class(TGraphicControl)
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  strict private
    FGlyphDown    : TPicture;
    FGlyphEnable  : TPicture;
    FGlyphHot     : TPicture;
    FGlyphDisable : TPicture;
    FExistsGlyphDisable : Boolean;
    FDown           : Boolean;
    FMouseInControl : Boolean;
    FDragging: Boolean;
    procedure SetGlyphEnable(const Value : TPicture);
    procedure SetGlyphDown(const Value : TPicture);
    procedure SetGlyphDisable(const Value : TPicture);
    procedure SetGlyphHot(const Value : TPicture);
    procedure CreateGlyphDisable(W, H: Integer; aSrc: TGraphic);
    procedure GlyphChanged(Sender : TObject);
    procedure CMMouseEnter(var Message: TLMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TLMessage); message CM_MOUSELEAVE;
    procedure UpdateTracking;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    property MouseInControl: Boolean read FMouseInControl;
  published
    property OnClick;
    property GlyphEnable  : TPicture  read FGlyphEnable   write SetGlyphEnable;
    property GlyphDown    : TPicture  read FGlyphDown     write SetGlyphDown;
    property GlyphDisable : TPicture  read FGlyphDisable  write SetGlyphDisable stored FExistsGlyphDisable;
    property GlyphHot     : TPicture  read FGlyphHot      write SetGlyphHot;
  end;

implementation

{$IFDEF FPC}
  {$define StyleServices}
  {$else}
    {$IF CompilerVersion <= 18.5}
    {$define StyleServices}
    {$IFEND}
{$endif}

{$IFDEF StyleServices}
function StyleServices(): TThemeServices; inline;
begin
  Result := ThemeServices;
end;

type

  { TThemeServicesHelper }

  TThemeServicesHelper = class helper for TThemeServices
  public
    function Enabled: Boolean;
  end;

{ TThemeServicesHelper }

function TThemeServicesHelper.Enabled : Boolean;
begin
  Result := Self.ThemesEnabled;
end;

{$ENDIF}

{ TFlatGraphControl }

constructor TFlatGraphControl.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FExistsGlyphDisable     := True;
  FGlyphEnable            := TPicture.Create;
  FGlyphDisable           := TPicture.Create;
  FGlyphDown              := TPicture.Create;
  FGlyphHot               := TPicture.Create;
  FGlyphEnable.OnChange   := GlyphChanged;
  FGlyphDown.OnChange     := GlyphChanged;
  FGlyphDisable.OnChange  := GlyphChanged;
  FGlyphHot.OnChange      := GlyphChanged;
end;

destructor TFlatGraphControl.Destroy;
begin
  FGlyphEnable.Free;
  FGlyphDown.Free;
  FGlyphDisable.Free;
  FGlyphHot.Free;
  inherited;
end;

procedure TFlatGraphControl.SetGlyphEnable(const Value: TPicture);
begin
  GlyphEnable.Assign(Value);
  if not FExistsGlyphDisable then
    if not GlyphDisable.IsEmpty then
      GlyphDisable.Assign(nil);
end;

procedure TFlatGraphControl.SetGlyphDown(const Value : TPicture);
begin
  GlyphDown.Assign(Value);
end;

procedure TFlatGraphControl.SetGlyphDisable(const Value : TPicture);
begin
  GlyphDisable.Assign(Value);
  FExistsGlyphDisable := not GlyphDisable.IsEmpty;
end;

procedure TFlatGraphControl.SetGlyphHot(const Value: TPicture);
begin
  GlyphHot.Assign(Value);
  if not FExistsGlyphDisable then
    if not GlyphDisable.IsEmpty then
      if GlyphEnable.IsEmpty then
        GlyphDisable.Assign(nil);
end;

procedure TFlatGraphControl.CreateGlyphDisable(W, H: Integer; aSrc: TGraphic);
var
  vColor, vColorTransparent : TColor;
  bmp : TBitmap;
  X, Y : Integer;
begin
  if aSrc <> nil then
  begin
    if W > 0 then
    begin
      if H < 1 then
      begin
        H := aSrc.Height;
        H := (H * W) div aSrc.Width;
      end;
    end
    else if H > 0 then
    begin
      if W < 1 then
      begin
        W := aSrc.Width;
        W := (W * H) div aSrc.Height;
      end;
    end
    else
    begin
      H := aSrc.Height;
      W := aSrc.Width;
    end;
  end
  else
  begin
    W := 0;
    H := 0;
  end;

  bmp := TBitmap.Create;
  try
    if W < 1 then
      bmp.Width := 1
    else
      bmp.Width := W;

    if H < 1 then
      bmp.Height := 1
    else
      bmp.Height := H;

    bmp.Canvas.Brush.Style := bsSolid;
    bmp.Canvas.Brush.Color := clBtnFace;
    bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));
    if (W > 0) and (H > 0) then
      bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height), aSrc);

    vColorTransparent := bmp.TransparentColor;

    for X := 0 to bmp.Width - 1 do
      for Y := 0 to bmp.Height - 1 do
      begin
        vColor := bmp.Canvas.Pixels[X, Y];
        if vColor <> vColorTransparent then
          bmp.Canvas.Pixels[X, Y] := ColorToGrayScale(vColor);
      end;

    FGlyphDisable.Assign(bmp);
  finally
    bmp.Free;
  end;
  FExistsGlyphDisable := False;
end;

procedure TFlatGraphControl.GlyphChanged(Sender : TObject);
begin
  Invalidate;
end;

procedure TFlatGraphControl.CMMouseEnter(var Message: TLMessage);
var
  NeedRepaint: Boolean;
begin
  inherited;
  NeedRepaint := not FMouseInControl and Enabled and (DragMode <> dmAutomatic) and (GetCapture = 0);

  if (NeedRepaint or StyleServices.Enabled) and not (csDesigning in ComponentState) then
  begin
    FMouseInControl := True;
    if Enabled then
      Repaint;
  end;
end;

procedure TFlatGraphControl.CMMouseLeave(var Message: TLMessage);
var
  NeedRepaint: Boolean;
begin
  inherited;
  NeedRepaint := FMouseInControl and Enabled and not FDragging;
  if NeedRepaint or StyleServices.Enabled then
  begin
    FMouseInControl := False;
    if Enabled then
      Repaint;
  end;
end;

procedure TFlatGraphControl.UpdateTracking;
var
  P: TPoint;
begin
  if Enabled then
  begin
    GetCursorPos(P{%H-});
    FMouseInControl := not (FindDragTarget(P, True) = Self);
    if FMouseInControl then
      Perform(CM_MOUSELEAVE, 0, 0)
    else
      Perform(CM_MOUSEENTER, 0, 0);
  end;
end;

procedure TFlatGraphControl.Paint;
var
  PaintRect   : TRect;

  function _DestRect(w, h: Integer): TRect;
  begin
    Result  := Rect(0, 0, w, h);
    OffsetRect(Result, (PaintRect.Right - w) div 2, (PaintRect.Bottom - h) div 2);
  end;

  procedure _Draw(aGraph: TPicture);
  begin
    Canvas.StretchDraw(_DestRect(aGraph.Width, aGraph.Height), aGraph.Graphic);
  end;

  function _TryDraw(aGraph: TPicture): Boolean;
  begin
    if (aGraph = nil) or aGraph.IsEmpty then
      Result := False
    else
    begin
      _Draw(aGraph);
      Result := True;
    end;
  end;

  procedure _DrawGlyphDisabled();

    procedure _DrawG(aGraph: TGraphic);
    begin
      {$IFNDEF FPC}
      //if TPngImage.SuportGrayScaleDraw then
        if aGraph is TPngImage then
        begin
          with TPngImage(aGraph) do
          begin
            Draw(Self.Canvas, Rect(0, 0, Width, Height), _DestRect(Width, Height), True);
            Exit;
          end;
      end;
      {$ENDIF}
      CreateGlyphDisable(-1, -1, aGraph);
      _Draw(GlyphDisable);
    end;

  begin
    if not _TryDraw(GlyphDisable) then
      if GlyphEnable.IsEmpty then
        _DrawG(GlyphHot.Graphic)
      else
        _DrawG(GlyphEnable.Graphic);
  end;

begin
  //writeln('TFlatGraphControl.Paint ', Name, ' Down: ', FDown);
  Canvas.Font := Self.Font;
  PaintRect   := Rect(0, 0, Width, Height);
  {$IFDEF FPC}
  if StyleServices.Enabled then
  {$ELSE}
  if ThemeControl(Self) then
  {$ENDIF}
    StyleServices.DrawParentBackground(0, Canvas.Handle, nil, False);

  if csDesigning in ComponentState then
    DrawEdge(Canvas.Handle, PaintRect, BDR_RAISEDINNER, BF_RECT);

  if not Enabled then
  begin
    FDragging := False;
    _DrawGlyphDisabled();
  end
  else if not (FDown and _TryDraw(GlyphDown)) then
  begin
    if MouseInControl or not _TryDraw(GlyphEnable) then
      _TryDraw(GlyphHot);
  end;
end;

procedure TFlatGraphControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    //writeln('TFlatGraphControl.MouseDown ');
    if not FDown then
    begin
      FDown := True;
      Invalidate;
    end;
    FDragging := True;
  end;
end;

procedure TFlatGraphControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if not FDragging then
    if not FMouseInControl then
      UpdateTracking;
end;

procedure TFlatGraphControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    FDragging := False;
    //DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);

    FDown := False;
    FMouseInControl := False;

    //if DoClick then
    //  Invalidate;

    //if DoClick then Click;
    UpdateTracking;
  end;
end;

end.

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

unit FormTalk;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, ShellApi, StdCtrls, Buttons, PanelTalk, Skins,
  Types,
  {$IFDEF FPC}
  LCLType,
  {$ENDIF}
  Helpers.Graphics,
  FlatGraphControl;

const
//  MSG_UpdateOnline    = 1;
  MSG_UpdateSkin      = 2;
//  MSG_CortaBorda0     = 5;
//  MSG_CortaBorda10    = 6;
//  MSG_UpdateDateTime  = 7;
//  MSG_ProcessLeater   = 8;
  MSG_AdjustBorder      = 9;
  cBorda = 4;

type
  //TContato      = class;
  //TStatePanel   = (spWaiting, spTransfering, spDone, spCanceled, spTimeOut, spError, spWaitingAccept);

  { TfrmTalk }

  TfrmTalk = class(TForm)
    PanelEdit : TPanel;
    EditMsg: TMemo;
    PanelAcoes : TPanel;
    Panel1 : TPanel;
    ScrollBoxAcoes : TScrollBox;
    SplitterArqs : TSplitter;
    PanelMSG : TPanel;
    PaintBoxTop : TPaintBox;
    PaintBoxLeft : TPaintBox;
    PaintBoxM : TPaintBox;
    PaintBoxEditL : TPaintBox;
    PaintBoxBottom : TPaintBox;
    PaintBoxEditR : TPaintBox;
    PaintBoxRight : TPaintBox;
    LDigitando : TLabel;
    btnEnviar: TSpeedButton;
    LabelCap : TLabel;
    Image1 : TImage;
    PanelAssistencia_: TPanel;
    LabelAss: TLabel;
    btnAssistAceita: TButton;
    btnAssistRejeita: TButton;
    procedure EditMsgChange(Sender : TObject);
    procedure EditMsgKeyPress(Sender : TObject; var Key : Char);
    procedure FormResize(Sender : TObject);
    procedure FormClose(Sender : TObject; var Action : TCloseAction);
    procedure FormActivate(Sender : TObject);
    procedure PaintBoxTopPaint(Sender : TObject);
    procedure PaintBoxLeftPaint(Sender : TObject);
    procedure PaintBoxRightPaint(Sender : TObject);
    procedure PaintBoxMPaint(Sender : TObject);
    procedure PaintBoxEditLPaint(Sender : TObject);
    procedure PaintBoxEditRPaint(Sender : TObject);
    procedure PaintBoxBottomPaint(Sender : TObject);
    procedure btnEnviarClick(Sender : TObject);
    //procedure miScriptClick(Sender : TObject);
    procedure FormKeyDown(Sender : TObject; var Key : Word; {%H-}Shift : TShiftState);
    //procedure MemoMsgKeyPress(Sender: TObject; var Key: Char);
    procedure FormMouseWheel(Sender: TObject; {%H-}Shift : TShiftState; WheelDelta: Integer; MousePos: TPoint; var {%H-}Handled: Boolean);
    procedure EditMsgKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
  public
    constructor Create(aOwner: TComponent; aSkin: TSkin; const aNikName: string); reintroduce;
    procedure DoLoaded; //override;
    destructor Destroy; override;
  private
    btnClose      : TFlatGraphControl;
    btnMax        : TFlatGraphControl;
    btnMin        : TFlatGraphControl;
    btnAvisoOff   : TFlatGraphControl;
    btnHistor     : TFlatGraphControl;
    btnFont       : TFlatGraphControl;
    btnSkin       : TFlatGraphControl;
    btnVNC        : TFlatGraphControl;
    btnSendFile   : TFlatGraphControl;
    btnViewer     : TFlatGraphControl;
    btnAtendNotify: TFlatGraphControl;
    class procedure AdjustBorder(Handle: THandle; AddButton: Integer);
    procedure btnMinClick(Sender : TObject);
    procedure btnMaxClick(Sender : TObject);
    procedure btnCloseClick(Sender : TObject);
    procedure miVNCClick(Sender : TObject);
    procedure miVNCDesconectarClick(Sender: TObject);
  private
    FFlashing : Boolean;
    FPanelTalk: TPanelTalk;
    FSkin     : TSkin;
    procedure WMUSER(var msg : TMessage); message WM_USER + 1;
    procedure WMDropFiles(var Msg : TMessage); message wm_DropFiles;
    //no delphi Berlin, usar esse evento deu pau
    procedure _FormCanResize(Sender : TObject; var NewWidth, NewHeight : Integer; var Resize : Boolean);
  private
    procedure DoSkinChanged();
    procedure PostAdjustBorder();
  protected
    procedure CreateParams(var Params : TCreateParams); override;
    procedure WMNCHitTest(var Msg : TWMNCHitTest); message WM_NCHITTEST;
    procedure DoSendText(const s: string); virtual; abstract;
  public
    FAssistRemota : string;
    FInAssistance : Boolean;
    property PanelTalk : TPanelTalk read FPanelTalk;
    procedure FlashExec;
    class procedure UpdateSkin();
  private
  end;

implementation

{$R *.DFM}
{$R ..\resources\btns.RES}

{ TfrmConversa }

constructor TfrmTalk.Create(aOwner: TComponent; aSkin: TSkin; const aNikName: string);

  function _CreateBtn(const aName, aHint: string): TFlatGraphControl;
  begin
    Result    := TFlatGraphControl.Create(Self);
    Result.GlyphEnable.ForceToPNG().LoadFromResourceName(HINSTANCE, 'png_btn'+aName);
    Result.GlyphHot.ForceToPNG().LoadFromResourceName(HINSTANCE, 'png_btn'+aName+'Hot');
    with Result.GlyphDown.ForceToPNG() do
    begin
      LoadFromResourceName(HINSTANCE, 'png_btn'+aName+'Hot');
      MakeHalfTransparent();
    end;
    Result.Parent        := Self;
    Result.Hint          := aHint;
    Result.ShowHint      := aHint <> '';
  end;

var
  vLeft : Integer;

  function _CreateBtnTask(const aName, aHint: string; aOnClick: TNotifyEvent = nil): TFlatGraphControl;
  begin
    Result := _CreateBtn(aName, aHint);
    Result.SetBounds(vLeft, 330, 37, 37);
    Result.OnClick := aOnClick;
    Inc(vLeft, 39);
  end;

begin
  inherited Create(AOwner);
  LDigitando.Transparent := True;
  LabelCap.Transparent   := True;
  FSkin       := aSkin;
  Constraints.MinWidth := 500;
  btnMin        := _CreateBtn('Min', 'Minimizar');
  with btnMin do
  begin
    SetBounds(594, 0, GlyphEnable.Width + 1, GlyphEnable.Height + 1);
    OnClick       := btnMinClick;
  end;
  Image1.Picture.ForceToPNG().LoadFromResourceName(HINSTANCE, 'png_Talk');
  btnMax        := _CreateBtn('Max', 'Maximizar');
  with btnMax do
  begin
    SetBounds(626, 0, GlyphEnable.Width + 1, GlyphEnable.Height + 1);
    OnClick       := btnMaxClick;
  end;

  btnClose        := _CreateBtn('Close', 'Fechar');
  with btnClose do
  begin
    SetBounds(653, 0, GlyphEnable.Width + 1, GlyphEnable.Height + 1);
    OnClick       := btnCloseClick;
  end;

  FPanelTalk := TPanelTalk.Create(Self, aSkin.Speak, aNikName);
  FPanelTalk.Parent      := PanelMSG;
  FPanelTalk.BorderStyle := bsNone;
  FPanelTalk.Color       := PanelEdit.Color;
  FPanelTalk.Show;

  vLeft           := 22;
  btnAvisoOff     := _CreateBtnTask('no', 'Avisar quando contato estiver on-line');
  btnHistor       := _CreateBtnTask('no', 'Histórico');
  btnFont         := _CreateBtnTask('no', 'Opções de Fonte');
  btnSkin         := _CreateBtnTask('no', 'Skin');
  btnVNC          := _CreateBtnTask('VNC', 'Conectar com VNC', miVNCClick);
  btnViewer       := _CreateBtnTask('viewer', 'Solicitar tela remota');
  btnAtendNotify  := _CreateBtnTask('no', 'Notificar atendimento');
  btnSendFile     := _CreateBtnTask('FileTransf', 'Transferência de arquivos');

  PaintBoxBottom.Height := aSkin.GetBottomHeight();
  DoubleBuffered        := True;
  FFlashing             := False;
  LDigitando.Caption    := ' ';
  Caption               := aNikName;
  LabelCap.Caption      := aNikName;
  EditMsg.Clear;
  //PanelTalk.UpdateFonte();
  FormResize(Self);
  DoSkinChanged();
  PostAdjustBorder();
end;

procedure TfrmTalk.DoLoaded();
begin
  inherited;
  //Self.AlignOwnerCenter();
  SetWindowPos(Self.Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_SHOWWINDOW);
  DragAcceptFiles(Self.Handle, True);
end;

destructor TfrmTalk.Destroy;
begin
  DragAcceptFiles(Handle, False);
  inherited;
end;

class procedure TfrmTalk.AdjustBorder(Handle: THandle; AddButton: Integer);
var
  hr1, hr2: THandle;
  R1, R2: TRect;
begin
  Windows.GetClientRect(Handle, R1{%H-});
  Windows.GetWindowRect(Handle, R2{%H-});
  MapWindowPoints(0, Handle, R2, 2);
  OffsetRect(R1, -R2.Left, -R2.Top);

  hr1 := CreateRoundRectRgn(R1.Left, R1.Top, R1.Right, R1.Bottom + AddButton, 20, 20);
  try
    if AddButton > 0 then
    begin
      hr2 := CreateRectRgnIndirect(R1);
      CombineRgn(hr1, hr2, hr1, RGN_AND);
      DeleteObject(hr2);
    end;
    SetWindowRgn(Handle, hr1, True);
  finally
    DeleteObject(hr1);
  end;
end;

procedure TfrmTalk.btnMinClick(Sender : TObject);
begin
  WindowState := wsMinimized;
end;

procedure TfrmTalk.btnMaxClick(Sender : TObject);
begin
  if WindowState = wsNormal then
  begin
    WindowState := wsMaximized;
    btnMax.Hint := 'Restaurar';
  end
  else
  begin
    WindowState := wsNormal;
    btnMax.Hint := 'Maximizar';
  end;
end;

procedure TfrmTalk.btnCloseClick(Sender : TObject);
begin
  Close;
end;

procedure TfrmTalk.miVNCClick(Sender : TObject);
begin
  //FContato.VNCRequest();
end;

procedure TfrmTalk.miVNCDesconectarClick(Sender: TObject);
begin
  //FContato.VNCDisconnect();
end;

procedure TfrmTalk.EditMsgChange(Sender : TObject);
begin
//  if not Proto.GetConnected() then
//    Exit;

  if btnEnviar.Enabled = (Trim(EditMsg.Text) <> '') then
    Exit;

  btnEnviar.Enabled := not btnEnviar.Enabled;

//  if FContato.Online then
//    Proto.SendData(MSG_TEXTO, '', Ord(not btnEnviar.Enabled) + 1, FContato.FNome, 0);
end;

procedure TfrmTalk.EditMsgKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_UP then
  begin
    if Trim(EditMsg.Text) = '' then
    begin
      EditMsg.Text := PanelTalk.GetLastTextSended();
    end;
  end;
end;

procedure TfrmTalk.EditMsgKeyPress(Sender : TObject; var Key : Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    btnEnviarClick(Sender);
  end;
end;

procedure TfrmTalk.DoSkinChanged();
begin
  PanelEdit.Color       := FSkin.GetColor();
  ScrollBoxAcoes.Color  := PanelEdit.Color;
  PanelTalk.Color       := PanelEdit.Color;
  //PanelAvisoOff.Color   := PanelEdit.Color;
end;

procedure TfrmTalk.PostAdjustBorder();
begin
  PostMessage(Handle, WM_USER + 1, MSG_AdjustBorder, 0);
end;

procedure TfrmTalk.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WndParent := 0;
  Params.Style := (Params.Style or WS_POPUP) and not WS_CAPTION;
end;

procedure TfrmTalk.FormResize(Sender : TObject);
var
  i: Integer;
begin
  PaintBoxTop.Width     := ClientWidth;
  PaintBoxRight.Left    := PaintBoxTop.Width - PaintBoxRight.Width;
  PanelMSG.Width        := PaintBoxRight.Left - PanelMSG.Left;
  PaintBoxM.Width       := PaintBoxTop.Width;
  PaintBoxBottom.Width  := PaintBoxTop.Width;
  PaintBoxEditR.Left    := PaintBoxRight.Left;
  PanelEdit.Width       := PanelMSG.Width;
  PaintBoxBottom.Top    := ClientHeight - PaintBoxBottom.Height;
  PaintBoxEditL.Top     := PaintBoxBottom.Top - PaintBoxEditL.Height;
  PaintBoxM.Top         := PaintBoxEditL.Top - PaintBoxM.Height;
  PaintBoxLeft.Height   := PaintBoxM.Top - PaintBoxLeft.Top;
  PaintBoxRight.Height  := PaintBoxLeft.Height;
  PanelMSG.Height       := PaintBoxLeft.Height;
  PaintBoxEditR.Top     := PaintBoxEditL.Top;
  PanelEdit.Top         := PaintBoxEditR.Top;
  i                     := PaintBoxM.Top + (PaintBoxM.Height - btnVNC.Height) div 2;
  btnAvisoOff.Top       := i;
  btnViewer.Top         := i;
  btnVNC.Top            := i;
  btnHistor.Top         := i;
  btnSkin.Top           := i;
  btnFont.Top           := i;
  btnSendFile.Top       := i;
  btnAtendNotify.Top    := i;
  btnClose.Left         := PaintBoxTop.Width - btnClose.Width - 10;
  btnMax.Left           := btnClose.Left - btnMax.Width + 1;
  btnMin.Left           := btnMax.Left - btnMin.Width + 1;
  btnEnviar.Top         := PaintBoxBottom.Top + PaintBoxBottom.Height - btnEnviar.Height - 5;
  btnEnviar.Left        := PanelEdit.Left + PanelEdit.Width - btnEnviar.Width;
  LDigitando.Top        := btnEnviar.Top + (btnEnviar.Height - LDigitando.Height) div 2;

  PostAdjustBorder();
end;

procedure TfrmTalk.FormClose(Sender : TObject; var Action : TCloseAction);
begin
  Action := caNone;

  if FInAssistance then
  begin
    ShowMessage('Primeiro cancele a conexão com assistência remota');
    WindowState := wsMinimized;//Show;
    Exit;
  end;

//  if FContato.FThreadARQ <> nil then
//    //if not FContato.FThreadARQ.Terminated then
//    begin
//      if not TDialogs.ExecTime('Cancelar o envio de arquivo(s)?', False, 10, Self, mtWarning) then
//        Exit;
//
//      FContato.FThreadARQ.Finalize;
//    end;

  Action := caFree;
end;

procedure TfrmTalk.WMUSER(var msg : TMessage);
begin
  case msg.WParam of
//    MSG_UpdateOnline :
    MSG_AdjustBorder: AdjustBorder(Handle, 0);
    MSG_UpdateSkin:
      begin
        DoSkinChanged();
        Invalidate;
      end;
  end;
end;

procedure TfrmTalk.WMDropFiles(var Msg : TMessage);
var
  i, vCount, BufferSize : word;
  Drop : HDROP;
  FileName : string;
  vList : TStrings;
begin
  Drop      := Msg.wParam;
  vCount := DragQueryFile(Drop, $FFFFFFFF, nil, 0);
  if vCount = 0 then
    Exit;

  vList := TStringList.Create;
  try
    SetLength(FileName, MAX_PATH + 1);
    for i := 0 to vCount - 1 do
    begin
      BufferSize := DragQueryFile(Drop, i, PChar(FileName), MAX_PATH + 1);
      vList.Add(Copy(FileName, 1, BufferSize));
    end;

    PanelTalk.AddMsg(vList.Text, Now, True, True);
  finally
    vList.Free;
  end;
  Msg.Result := 0;
end;

procedure TfrmTalk._FormCanResize(Sender : TObject; var NewWidth, NewHeight : Integer; var Resize : Boolean);
begin
  Resize := (NewWidth >= 380) and (NewHeight >= 480);
end;

{$IFDEF FPC}
type
  FLASHWINFO = record
    cbSize: UINT;
    hwnd: HWND;
    dwFlags: DWORD;
    uCount: UINT;
    dwTimeout: DWORD;
  end;
  //PFLASHWINFO = ^FLASHWINFO;

const
  FLASHW_STOP      = 0;
  {$EXTERNALSYM FLASHW_STOP}
  FLASHW_CAPTION   = $00000001;
  {$EXTERNALSYM FLASHW_CAPTION}
  FLASHW_TRAY      = $00000002;
  {$EXTERNALSYM FLASHW_TRAY}
  FLASHW_ALL       = FLASHW_CAPTION or FLASHW_TRAY;
  {$EXTERNALSYM FLASHW_ALL}
  FLASHW_TIMER     = $00000004;
  {$EXTERNALSYM FLASHW_TIMER}
  //FLASHW_TIMERNOFG = $0000000C;
  //{$EXTERNALSYM FLASHW_TIMERNOFG}

function FlashWindowEx(const pfwi:FLASHWINFO):WINBOOL; stdcall; external 'user32' name 'FlashWindowEx';
{$ENDIF}

procedure TfrmTalk.FormActivate(Sender : TObject);
var
  pfwi : FLASHWINFO;
begin
  if FFlashing then
  begin
    pfwi.cbSize := SizeOf(pfwi);
    pfwi.hwnd := Handle;
    pfwi.dwFlags := FLASHW_STOP;
    pfwi.uCount := 0;
    pfwi.dwTimeout := 0;
    FlashWindowEx(pfwi);
    FFlashing := False;
  end;
end;

procedure TfrmTalk.PaintBoxTopPaint(Sender : TObject);
begin
  with PaintBoxTop do
    FSkin.DrawTop(Canvas, ClientRect);
end;

procedure TfrmTalk.PaintBoxLeftPaint(Sender : TObject);
begin
  with PaintBoxLeft do
   FSkin.DrawLeft(Canvas, ClientRect);
end;

procedure TfrmTalk.PaintBoxRightPaint(Sender : TObject);
begin
  with PaintBoxRight do
    FSkin.DrawRight(Canvas, ClientRect);
end;

procedure TfrmTalk.PaintBoxMPaint(Sender : TObject);
begin
  with PaintBoxM do
    FSkin.DrawMidle(Canvas, ClientRect);
end;

procedure TfrmTalk.PaintBoxEditLPaint(Sender : TObject);
begin
  with PaintBoxEditL do
    FSkin.DrawLeftB(Canvas, ClientRect);
end;

procedure TfrmTalk.PaintBoxEditRPaint(Sender : TObject);
begin
  with PaintBoxEditR do
    FSkin.DrawRightB(Canvas, ClientRect);
end;

procedure TfrmTalk.PaintBoxBottomPaint(Sender : TObject);
begin
  with PaintBoxBottom do
    FSkin.DrawBottom(Canvas, ClientRect)
end;

procedure TfrmTalk.WMNCHitTest(var Msg : TWMNCHitTest);
var
  P : TPoint;

  function _IsFocusInControl() : Boolean;
  var
    i : Integer;
    c : TControl;
  begin
    for i := 0 to ControlCount - 1 do
    begin
      c := Controls[i];
      if c is btnClose.ClassType then
        if c.Visible and PtInRect(c.BoundsRect, P) then
        begin
          Result := True;
          Exit;
        end;
    end;

    Result := False;
  end;

begin
  P := ScreenToClient(SmallPointToPoint(Msg.Pos));

  Msg.Result := -1;

  if P.y < 10 then
  begin
    if P.X < 10 then
      Msg.Result := HTTOPLEFT
    else if P.X >= PaintBoxTop.Width - 10 then
      Msg.Result := HTTOPRIGHT;
  end
  else if P.y >= PaintBoxBottom.Top + PaintBoxBottom.Height - 10 then
  begin
    if P.X < 10 then
      Msg.Result := HTBOTTOMLEFT
    else if P.X >= PaintBoxTop.Width - 10 then
      Msg.Result := HTBOTTOMRIGHT;
  end;

  if Msg.Result = -1 then
  begin
    if P.y < cBorda then
      Msg.Result := HTTOP
    else if P.y >= PaintBoxBottom.Top + PaintBoxBottom.Height - cBorda then
      Msg.Result := HTBOTTOM
    else if P.X < cBorda then
      Msg.Result := HTLEFT
    else if P.X >= PaintBoxTop.Width - cBorda then
      Msg.Result := HTRIGHT
    else
    begin
      if _IsFocusInControl() then
        Msg.Result := HTCLIENT
      else
        Msg.Result := HTCAPTION;
    end;
  end;
end;

procedure TfrmTalk.btnEnviarClick(Sender : TObject);
begin
  if Trim(EditMsg.Text) <> '' then
  begin
    DoSendText(EditMsg.Text);
    EditMsg.Text := '';
  end;
end;

procedure TfrmTalk.FormKeyDown(Sender : TObject; var Key : Word;
  Shift : TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TfrmTalk.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  c : TControl;
begin
  c := ControlAtPos(ScreenToClient(MousePos), False, True);
  if c = PanelMSG then
  begin
    with PanelTalk.VertScrollBar do
    begin
      if WheelDelta < 0 then
        Position := Position + Increment * 3
      else
        Position := Position - Increment * 3;
    end;
  end;
end;

procedure TfrmTalk.FlashExec();
var
  pfwi : FLASHWINFO;
begin
  if not Visible then
  begin
    ShowWindow(Handle, SW_SHOWNOACTIVATE);
    Visible := True;
  end;

  if GetActiveWindow <> Handle then
  begin
    pfwi.cbSize := SizeOf(pfwi);
    pfwi.hwnd := Handle;
    pfwi.dwFlags := FLASHW_ALL or FLASHW_TIMER; // or FLASHW_TIMER;//or FLASHW_TIMERNOFG;
    pfwi.uCount := 5;
    pfwi.dwTimeout := 0; //100;
    FlashWindowEx(pfwi);
    FFlashing := True;
  end;
end;

class procedure TfrmTalk.UpdateSkin();
var
  i: Integer;
begin
  for i := 0 to Screen.CustomFormCount - 1 do
    if Screen.CustomForms[i] is Self then
      PostMessage(Screen.CustomForms[i].Handle, WM_USER + 1, MSG_UpdateSkin, 0);
end;

end.


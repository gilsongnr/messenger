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

unit Unit1;

{$IFDEF FPC}
{$mode delphi}
{$Codepage cp1252}
{$ENDIF}

interface

uses
  Classes, Controls, SysUtils, Graphics, Forms, Dialogs,
  StdCtrls, FormTalk, ExtCtrls, Buttons, Skins, PanelTalk;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ComboBox1: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    FSkin: TSkin;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FSkin       := TSkin.Create();
  FSkin.Kind  := skSkin1;
  Randomize;
end;

destructor TForm1.Destroy;
begin
  FSkin.Free;
  inherited Destroy;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  FSkin.Kind := TSkinKind(ComboBox1.ItemIndex);
  TfrmTalk.UpdateSkin();
end;

type
  TfrmTalkTest = class(TfrmTalk)
  public
    procedure AfterConstruction; override;
  protected
    procedure DoSendText(const s: string); override;
  private
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  end;

function _GenerateText(aCountWords: Integer): string;
var
  w: string;
  i, r: Integer;
begin
  Result := '';
  for r := 1 to aCountWords do
  begin
    SetLength(w, Random(20)+1);
    for i := 1 to Length(w) do
      w[i] := Char(Random(Ord('z') - Ord(Pred('a'))) + Ord('a'));

    Result := Result + w + ' ';
  end;
end;

{ TfrmTalkTest }

procedure TfrmTalkTest.AfterConstruction;
begin
  inherited;
  with TButton.Create(Self) do
  begin
    Parent  := Self;
    SetBounds(453, 360, 75, 25);
    Anchors := [akRight, akBottom];
    Caption := 'Fala';
    OnClick := Button1Click;
  end;

  with TButton.Create(Self) do
  begin
    Parent  := Self;
    SetBounds(607, 360, 75, 25);
    Anchors := [akRight, akBottom];
    Caption := 'info';
    OnClick := Button2Click;
  end;

  with TButton.Create(Self) do
  begin
    Parent  := Self;
    SetBounds(530, 360, 75, 25);
    Anchors := [akRight, akBottom];
    Caption := 'Ouve';
    OnClick := Button3Click;
  end;
  LDigitando.Caption     := 'Fulano está digitando...';
end;

procedure TfrmTalkTest.DoSendText(const s: string);
begin
  PanelTalk.AddMsg(s, Now, False, Random(1) = 1);
end;

procedure TfrmTalkTest.Button1Click(Sender: TObject);
begin
  PanelTalk.Add(_GenerateText(Random(20)+1), Now, otSended, False);
end;

procedure TfrmTalkTest.Button2Click(Sender: TObject);
begin
  PanelTalk.Add(_GenerateText(Random(3)+1), Now, otInfo, False);
end;

procedure TfrmTalkTest.Button3Click(Sender: TObject);
begin
  PanelTalk.Add(_GenerateText(Random(20)+1), Now, otReceived, False);
end;

procedure TForm1.Button1Click(Sender: TObject);
{$J+}
const
  j: Integer = 0;
{$J-}
var
  z: Integer;
begin
  for z := 1 to 3 do
  begin
    Inc(j);
    with TfrmTalkTest.Create(Self, FSkin, Format('Contato %.5d %.d %s', [j, z, IntToHex(Random($FFFF), 4)])) do
    begin
      Position := poDesigned;
      Left := Left + j * 20;
      Top  := Top + j * 20;
      if j mod 2 = 0 then
        Show
      else
        FlashExec; //noifica o usuário na barra de tarefas
    end;
  end;
end;

end.

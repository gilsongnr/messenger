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

unit Helpers.Menus;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Menus;

type
  { TMenuItemHelper }

  TMenuItemHelper = class helper for TMenuItem
  public
    //function AddItem(Item : TMenuItem) : TMenuItem;
    //function InsertItem(Index: Integer; Item: TMenuItem): TMenuItem;
    //procedure InsertTo(AParent: TMenuItem; Index: Integer = -1);
    function AddNewItem(const aCaption: string; aImagIndex: Integer = -1; aOnClick: TNotifyEvent = nil): TMenuItem; overload;
    function AddNewItem(const aCaption: string; aOnClick: TNotifyEvent): TMenuItem; overload;
    //function AddNewItem(aPosition : Integer; const ACaption: string; AOnClick: TNotifyEvent = nil): TMenuItem; overload;
    //function CreateClone(aOwner : TComponent): TMenuItem; overload;
    //function CreateClone(): TMenuItem; overload;
  end;

implementation

{ TMenuItemHelper }

function TMenuItemHelper.AddNewItem(const aCaption: string; aImagIndex: Integer; aOnClick: TNotifyEvent): TMenuItem;
begin
  Result := TMenuItem.Create(Self);
  with Result do
  begin
    Caption     := ACaption;
    OnClick     := AOnClick;
    ImageIndex  := AImagIndex;
  end;
  Add(Result);
end;

function TMenuItemHelper.AddNewItem(const aCaption: string; aOnClick: TNotifyEvent): TMenuItem;
begin
  Result := AddNewItem(aCaption, -1, aOnClick);
end;

end.


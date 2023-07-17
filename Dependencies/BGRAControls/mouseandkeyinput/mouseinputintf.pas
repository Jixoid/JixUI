{ MouseInputIntf

  Copyright (C) 2008 Tom Gregorovic

  This source is free software; you can redistribute it and/or modify it under the terms of the
  GNU General Public License as published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
  even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit MouseInputIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms;
  
type
  { TMouseInput }

  TMouseInput = class
  protected
    procedure DoDown(Button: TMouseButton); dynamic; abstract;
    procedure DoMove(ScreenX, ScreenY: Integer); dynamic; abstract;
    procedure DoUp(Button: TMouseButton); dynamic; abstract;
    procedure DoScrollUp; dynamic; abstract;
    procedure DoScrollDown; dynamic; abstract;
  public
    procedure Down(Button: TMouseButton; Shift: TShiftState);
    procedure Down(Button: TMouseButton; Shift: TShiftState; Control: TControl; X, Y: Integer);
    procedure Down(Button: TMouseButton; Shift: TShiftState; ScreenX, ScreenY: Integer);
    
    procedure Move(Shift: TShiftState; Control: TControl; X, Y: Integer; Duration: Integer = 0);
    procedure MoveBy(Shift: TShiftState; DX, DY: Integer; Duration: Integer = 0);
    procedure Move(Shift: TShiftState; ScreenX, ScreenY: Integer; Duration: Integer);
    procedure Move(Shift: TShiftState; ScreenX, ScreenY: Integer);

    procedure ScrollUp(Shift: TShiftState);
    procedure ScrollUp(Shift: TShiftState; Control: TControl; X, Y: Integer);
    procedure ScrollUp(Shift: TShiftState; ScreenX, ScreenY: Integer);
    procedure ScrollDown(Shift: TShiftState);
    procedure ScrollDown(Shift: TShiftState; Control: TControl; X, Y: Integer);
    procedure ScrollDown(Shift: TShiftState; ScreenX, ScreenY: Integer);

    procedure Up(Button: TMouseButton; Shift: TShiftState);
    procedure Up(Button: TMouseButton; Shift: TShiftState; Control: TControl; X, Y: Integer);
    procedure Up(Button: TMouseButton; Shift: TShiftState; ScreenX, ScreenY: Integer);
    
    procedure Click(Button: TMouseButton; Shift: TShiftState);
    procedure Click(Button: TMouseButton; Shift: TShiftState; Control: TControl; X, Y: Integer);
    procedure Click(Button: TMouseButton; Shift: TShiftState; ScreenX, ScreenY: Integer);
    
    procedure DblClick(Button: TMouseButton; Shift: TShiftState);
    procedure DblClick(Button: TMouseButton; Shift: TShiftState; Control: TControl; X, Y: Integer);
    procedure DblClick(Button: TMouseButton; Shift: TShiftState; ScreenX, ScreenY: Integer);
  end;

Implementation
End.

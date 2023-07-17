{ XMouseInput

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
unit XMouseInput;

{$mode objfpc}{$H+}
{$linklib Xtst}

interface

uses
  Classes, SysUtils, Controls, Forms,
  XLib, MouseInputIntf;
  
type

  { TXMouseInput }

  TXMouseInput = class(TMouseInput)
  protected
    procedure DoDown(Button: TMouseButton); override;
    procedure DoMove(ScreenX, ScreenY: Integer); override;
    procedure DoUp(Button: TMouseButton); override;
    procedure DoScrollUp; override;
    procedure DoScrollDown; override;
  end;
  
function InitializeMouseInput: TMouseInput;

function XTestFakeButtonEvent(dpy: PDisplay; button: dword; is_press: Boolean;
  delay: dword): longint; cdecl; external;

function XTestFakeMotionEvent(dpy: PDisplay; screen: longint; x: longint; y: longint;
  delay: dword): longint; cdecl; external;

Implementation
End.

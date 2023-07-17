// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Created by BGRA Controls Team
  Dibo, Circular, lainz (007) and contributors.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html
}

unit BGRAScript;

{$I bgracontrols.map.inc}
{ $define debug}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, Dialogs;

{Template}
procedure SynCompletionList(itemlist: TStrings);
{Scripting}
function ScriptCommand(command: string; var bitmap: TBGRABitmap;
  var variables: TStringList; var line: integer): boolean;
function ScriptCommandList(commandlist: TStrings; var bitmap: TBGRABitmap): boolean;

{Tools}
function StrToDrawMode(mode: string): TDrawMode;

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}

unit BCBrightAndContrast;

{ Unit contributed by esvignolo }

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, Graphics,{$IFDEF FPC}LCLType{$ELSE}Types, BGRAGraphics, GraphType, FPImage{$ENDIF};

function Bright(aColor: TColor; BrightPercent: byte): TColor;
function GetContrastColor(ABGColor: TColor): TColor;

Implementation
End.

unit RGBCMYKUtils;

interface

// Activate only one of these defines  - see comments below

{.$DEFINE CMYK_FORMULA_1}       // Original formula used by mbColorLib
{$DEFINE CMYK_FORMULA_2}        // Result agrees with OpenOffice

uses
  LCLIntf, Graphics, Math;

function CMYtoColor(C, M, Y: integer): TColor;
procedure RGBtoCMY(clr: TColor; var C, M, Y: integer);

function CMYKToColor (C, M, Y, K: Integer): TColor;
procedure ColorToCMYK(clr: TColor; out C, M, Y, K: integer);

function GetCValue(c: TColor): integer;
function GetMValue(c: TColor): integer;
function GetYValue(c: TColor): integer;
function GetKValue(c: TColor): integer;


Implementation
End.

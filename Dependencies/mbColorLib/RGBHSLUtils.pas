unit RGBHSLUtils;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, Graphics, Math, Scanlines;

var //set these variables to your needs, e.g. 360, 255, 255
  MaxHue: integer = 359;
  MaxSat: integer = 240;
  MaxLum: integer = 240;

{function HSLtoRGB(H, S, L: double): TColor;}
function HSLRangeToRGB(H, S, L: integer): TColor;

{procedure ColorToHSL(AColor: TColor; var H, S, L: Double);}
function HSLtoColor(H, S, L: Double): TColor;

{procedure RGBtoHSL(RGB: TColor; out H, S, L: Double);       }
procedure RGBtoHSLRange(RGB: TColor; out H1, S1, L1: integer);

function GetHValue(AColor: TColor): integer;
function GetSValue(AColor: TColor): integer;
function GetLValue(AColor: TColor): integer;

function HSLToRGBTriple(H, S, L : integer) : TRGBTriple;
function HSLToRGBQuad(H, S, L: integer): TRGBQuad;
procedure RGBTripleToHSL(RGBTriple : TRGBTriple; var h, s, l: integer);


Implementation
End.

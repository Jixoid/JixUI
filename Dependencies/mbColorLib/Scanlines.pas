unit Scanlines;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, Graphics;

type
  TRGBTripleArray = array [0..65535] of TRGBTriple;
  pRGBTripleArray = ^TRGBTripleArray;

  TRGBQuadArray = array [0..65535] of TRGBQuad;
  pRGBQuadArray = ^TRGBQuadArray;

function RGBtoRGBTriple(R, G, B: byte): TRGBTriple;
function RGBtoRGBQuad(R, G, B: byte): TRGBQuad; overload;
function RGBToRGBQuad(c: TColor): TRGBQuad; overload;
function RGBQuadToRGB(q: TRGBQuad): TColor;
function RGBTripleToColor(RGBTriple : TRGBTriple) : TColor;


Implementation
End.

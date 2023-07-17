unit mbColorConv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TBrightnessMode = (bmLuminance, bmValue);

{ HSL color model }

function HSLtoColor(H, S, L: double): TColor;
procedure HSLtoRGB(H, S, L: Double; out R, G, B: Integer);

procedure ColortoHSL(c: TColor; out H, S, L: Double);
procedure RGBtoHSL(R, G, B: Integer; out H, S, L: Double);

{ HSV color model }

procedure ColorToHSV(c: TColor; out H, S, V: Double);
procedure RGBtoHSV(R, G, B: Integer; out H, S, V: Double);

function HSVtoColor(H, S, V: Double): TColor;
procedure HSVtoRGB(H, S, V: Double; out R, G, B: Integer);

{ H, S, L, V extraction }

function GetRelHValue(c: TColor): Double;
function GetRelSValueHSL(c: TColor): Double;
function GetRelSValueHSV(c: TColor): Double;
function GetRelLValue(c: TColor): Double;
function GetRelVValue(c: TColor): Double;


Implementation
End.

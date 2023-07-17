unit mbUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLIntf;

procedure Clamp(var AValue: Integer; AMin, AMax: Integer); overload;
procedure Clamp(var AValue: Double; AMin, AMax: Double); overload;
procedure DrawHorDottedLine(ACanvas: TCanvas; X1, X2, Y: Integer; AColor: TColor);
function PointInCircle(p: TPoint; Size: integer): boolean;
function PtInCircle(p, ctr: TPoint; Radius: Integer): Boolean;

function HighContrastColor(AColor: TColor): TColor;

function HeightOfRect(R: TRect): Integer;
function WidthOfRect(R: TRect): Integer;
function IsEmptyRect(R: TRect): Boolean;

const
  EMPTY_RECT: TRect = (Left: -1; Top: -1; Right: -1; Bottom: -1);
  TWO_PI = 2.0 * pi;

Implementation
End.

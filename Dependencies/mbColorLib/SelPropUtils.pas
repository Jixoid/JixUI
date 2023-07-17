unit SelPropUtils;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, Classes, Graphics;

procedure DrawSelCross(x, y: integer; Canvas: TCanvas; Color: TColor);
procedure DrawSelCrossCirc(x, y: integer; Canvas: TCanvas; Color: TColor);
procedure DrawSelCirc(x, y: integer; Canvas: TCanvas);
procedure DrawSelSquare(x, y: integer; Canvas: TCanvas);

Implementation
End.

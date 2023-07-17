unit HColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms,
  HTMLColors, mbColorConv, mbTrackBarPicker;

type
  THColorPicker = class(TmbHSLVTrackBarPicker)
  private
    function ArrowPosFromHue(h: Double): integer;
    function HueFromArrowPos(p: integer): Double;
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure Execute(tbaAction: integer); override;
    function GetArrowPos: integer; override;
    function GetGradientColor(AValue: Integer): TColor; override;
    function GetSelectedValue: integer; override;
    procedure SetMaxHue(H: Integer); override;
    procedure SetRelHue(H: Double); override;
    procedure SetSelectedColor(c: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Layout default lyHorizontal;
    property Hue default 0;
    property Saturation default 255;
    property Luminance default 127;
    property Value default 255;
    property SelectedColor default clRed;
    property HintFormat;
  end;


Implementation
End.

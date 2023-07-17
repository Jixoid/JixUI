unit HRingPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Math, Forms,
  HTMLColors, mbColorConv, mbColorPickerControl;

type
  THRingPicker = class(TmbHSLVColorPickerControl)
  private
    FSelectedColor: TColor;
    FHueLineColor: TColor;
    FRadius: integer;
    procedure SetRadius(r: integer);
    procedure SetHueLineColor(c: TColor);
  protected
    procedure CreateGradient; override;
    procedure DrawHueLine;
    function GetGradientColor2D(X, Y: Integer): TColor; override;
    function GetSelectedColor: TColor; override;
//    function MouseOnPicker(X, Y: Integer): Boolean;
    procedure Paint; override;
    procedure Resize; override;
    procedure SelectColor(x, y: integer); override;
    procedure SetRelHue(H: Double); override;
    procedure SetSelectedColor(c: TColor); override;
    procedure UpdateCoords;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorAtPoint(x, y: integer): TColor; override;
    property ColorUnderCursor;
  published
    property Hue default 0;
    property Luminance default 127;
    property Saturation default 255;
    property Value default 255;
    property MaxHue default 360;
    property MaxLuminance default 255;
    property MaxSaturation default 255;
    property MaxValue default 255;
    property HueLineColor: TColor read FHueLineColor write SetHueLineColor default clGray;
    property Radius: integer read FRadius write SetRadius default 40;
    property SelectedColor default clRed; //clNone;
    property OnChange;
  end;


Implementation
End.

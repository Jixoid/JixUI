unit HSColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms,
  HTMLColors, mbColorConv, mbColorPickerControl;

type

  { THSColorPicker }

  THSColorPicker = class(TmbHSLVColorPickerControl)
  private
    FLumDisp, FValDisp: Double;  // Lum and Value used for display
  protected
    procedure CreateWnd; override;
    procedure DrawMarker(x, y: integer);
    function GetGradientColor2D(x, y: Integer): TColor; override;
    function GetSelectedColor: TColor; override;
    procedure Paint; override;
    function PredictColor: TColor;
    procedure Resize; override;
    procedure SelectColor(x, y: Integer); override;
    procedure SetMaxHue(H: Integer); override;
    procedure SetMaxSat(S: Integer); override;
    procedure SetRelHue(H: Double); override;
    procedure SetRelSat(S: Double); override;
    procedure SetSelectedColor(c: TColor); override;
    procedure UpdateCoords;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorAtPoint(x, y: Integer): TColor; override;
  published
    property SelectedColor default clRed;
    property Hue default 0;
    property Saturation default 255;
    property Luminance default 127;
    property Value default 255;
    property MaxHue default 360;
    property MaxSaturation default 255;
    property MaxLuminance default 255;
    property MaxValue default 255;
    property MarkerStyle default msCross;
    property OnChange;
  end;


Implementation
End.

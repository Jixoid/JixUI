unit SLColorPicker;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms,
  mbColorConv, mbColorPickerControl;

type
  TSLColorPicker = class(TmbHSLVColorPickerControl)
  private
    FHint: array[TBrightnessMode] of string;
    function GetHint(AMode: TBrightnessMode): String;
    procedure SetHint(AMode: TBrightnessMode; AText: String);
  protected
    procedure CorrectCoords(var x, y: integer);
    procedure CreateWnd; override;
    procedure DrawMarker(x, y: integer);
    function GetGradientColor2D(X, Y: Integer): TColor; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure SelectColor(x, y: integer); override;
    procedure SetBrightnessMode(AMode: TBrightnessMode); override;
    procedure SetMaxLum(L: Integer); override;
    procedure SetMaxSat(S: Integer); override;
    procedure SetMaxVal(V: Integer); override;
    procedure SetRelLum(L: Double); override;
    procedure SetRelSat(S: Double); override;
    procedure SetRelVal(V: Double); override;
    procedure SetSelectedColor(c: TColor); override;
    procedure UpdateCoords;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorAtPoint(x, y: integer): TColor; override;
    property ColorUnderCursor;
  published
    property Hue default 0;
    property Saturation default 0;
    property Luminance default 255;
    property Value default 255;
    property MaxHue default 360;
    property MaxSaturation default 255;
    property MaxLuminance default 255;
    property MaxValue default 255;
    property SelectedColor default clWhite;
    property MarkerStyle default msCircle;
    property SLHintFormat: String index bmLuminance read GetHint write SetHint;
    property SVHintFormat: String index bmValue read GetHint write SetHint;
    property OnChange;
  end;

Implementation
End.

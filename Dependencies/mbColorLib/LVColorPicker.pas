{ A trackbar picker for Luminance or Value parameters from the HSL or HSV
  color models (depending on setting for BrightnessMode) }

unit LVColorPicker;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms,
  HTMLColors, mbColorConv, mbTrackBarPicker;

type
  TLVColorPicker = class(TmbHSLVTrackBarPicker)
  private
    FHint: array[TBrightnessMode] of string;
    function ArrowPosFromLum(L: Double): integer;
    function ArrowPosFromVal(V: Double): integer;
    function LumFromArrowPos(p: integer): Double;
    function ValFromArrowPos(p: Integer): Double;
    function GetHint(AMode: TBrightnessMode): String;
    procedure SetHint(AMode: TBrightnessMode; AText: String);
  protected
    procedure Execute(tbaAction: integer); override;
    function GetArrowPos: integer; override;
    function GetGradientColor(AValue: Integer): TColor; override;
    function GetSelectedValue: integer; override;
    procedure SetBrightnessMode(AMode: TBrightnessMode); override;
    procedure SetMaxLum(L: Integer); override;
    procedure SetMaxVal(V: Integer); override;
    procedure SetRelLum(L: Double); override;
    procedure SetRelVal(V: Double); override;
    procedure SetSelectedColor(c: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Hue default 0;
    property Saturation default 0;
    property Luminance default 255;
    property Value default 255;
    property SelectedColor default clWhite;
    property LHintFormat: String index bmLuminance read GetHint write SetHint;
    property VHintFormat: String index bmValue read GetHint write SetHint;
  end;

Implementation
End.

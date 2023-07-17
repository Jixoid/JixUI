unit HSLColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms, Menus, Themes,
  HTMLColors, mbColorConv, HSColorPicker, LVColorPicker, mbBasicPicker;

type
  THSLColorPicker = class(TmbBasicPicker)
  private
    FHSPicker: THSColorPicker;
    FLVPicker: TLVColorPicker;
    FRed, FGreen, FBlue: integer;
    FHSHint: string;
    FLVMenu, FHSMenu: TPopupMenu;
    FLVIncrement: integer;
    FHSCursor, FLVCursor: TCursor;
    PBack: TBitmap;
    function GetBrightnessMode: TBrightnessMode;
    function GetHue: Integer;
    function GetSat: Integer;
    function GetLum: Integer;
    function GetVal: Integer;
    function GetMaxHue: Integer;
    function GetMaxSat: Integer;
    function GetMaxLum: Integer;
    function GetMaxVal: Integer;
    function GetRelHue: Double;
    function GetRelSat: Double;
    function GetRelLum: Double;
    function GetRelVal: Double;
    function GetLVHint(AMode: TBrightnessMode): String;

    procedure SetBrightnessMode(AMode: TBrightnessMode);

    procedure SetHue(H: integer);
    procedure SetSat(S: integer);
    procedure SetLum(L: integer);
    procedure SetVal(V: Integer);

    procedure SetMaxHue(H: Integer);
    procedure SetMaxLum(L: Integer);
    procedure SetMaxSat(S: Integer);
    procedure SetMaxVal(V: Integer);

    procedure SetRed(R: integer);
    procedure SetGreen(G: integer);
    procedure SetBlue(B: integer);

    procedure SetRelHue(H: Double);
    procedure SetRelLum(L: Double);
    procedure SetRelSat(S: Double);
    procedure SetRelVal(V: Double);

    procedure SetHSCursor(c: TCursor);
    procedure SetHSHint(h: string);
    procedure SetHSMenu(m: TPopupMenu);

    procedure SetLVCursor(c: TCursor);
    procedure SetLVHint(AMode: TBrightnessMode; AText: string);
    procedure SetLVMenu(m: TPopupMenu);
    procedure SetLVIncrement(i: integer);

  protected
    procedure DoChange; override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    function GetColorUnderCursor: TColor; override;
    function GetSelectedColor: TColor; override;
    procedure HSPickerChange(Sender: TObject);
    procedure LVPickerChange(Sender: TObject);
    procedure Paint; override;
    procedure Resize; override;
    procedure SetSelectedColor(Value: TColor); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetHexColorUnderCursor: string; override;
    function GetSelectedHexColor: string;
    procedure SetFocus; override;
    property ColorUnderCursor;
    property Red: integer read FRed write SetRed;
    property Green: integer read FGreen write SetGreen;
    property Blue: integer read FBlue write SetBlue;
    property RelHue: Double read GetRelHue write SetRelHue;
    property RelSaturation: Double read GetRelSat write SetRelSat;
    property RelLuminance: Double read GetRelLum write SetRelLum;
    property RelValue: Double read GetRelVal write SetRelVal;
  published
    property BrightnessMode: TBrightnessMode read GetBrightnessMode
      write SetBrightnessMode default bmLuminance;
    property Hue: integer read GetHue write SetHue default 0;
    property Saturation: integer read GetSat write SetSat default 255;
    property Luminance: integer read GetLum write SetLum default 127;
    property LVIncrement: integer read FLVIncrement write SetLVIncrement default 1;
    property Value: Integer read GetVal write SetVal default 255;
    property SelectedColor default clRed;
    property HSPickerPopupMenu: TPopupMenu read FHSMenu write SetHSMenu;
    property LVPickerPopupMenu: TPopupMenu read FLVMenu write SetLVMenu;
    property HSPickerHintFormat: string read FHSHint write SetHSHint;
    property LPickerHintFormat: string index bmLuminance read GetLVHint write SetLVHint;
    property VPickerHintFormat: string index bmValue read GetLVHint write SetLVHint;
    property HSPickerCursor: TCursor read FHSCursor write SetHSCursor default crDefault;
    property LVPickerCursor: TCursor read FLVCursor write SetLVCursor default crDefault;
    property MaxHue: Integer read GetMaxHue write SetMaxHue default 360;
    property MaxSaturation: Integer read GetMaxSat write SetMaxSat default 255;
    property MaxLuminance: Integer read GetMaxLum write SetMaxLum default 255;
    property MaxValue: Integer read GetMaxVal write SetMaxVal default 255;
    property TabStop default true;
    property ShowHint;
    property ParentShowHint;
    property Anchors;
    property Align;
    property BorderSpacing;
    property Visible;
    property Enabled;
    property TabOrder;
    property Color;
    property ParentColor default true;
    property OnChange;
    property OnMouseMove;
  end;


Implementation
End.

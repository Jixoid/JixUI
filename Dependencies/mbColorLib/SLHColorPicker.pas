unit SLHColorPicker;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms, Menus, Themes,
  RGBHSLUtils, mbTrackBarPicker, HTMLColors, SLColorPicker, HColorPicker,
  mbColorConv, mbBasicPicker;

type
  TSLHColorPicker = class(TmbBasicPicker)
  private
    FSLPicker: TSLColorPicker;
    FHPicker: THColorPicker;
    FSelectedColor: TColor;
//    FHValue, FSValue, FLValue: Double;
//    FRed, FGreen, FBlue: integer;
    FSLHint, FHHint: string;
    FSLMenu, FHMenu: TPopupMenu;
    FSLCursor, FHCursor: TCursor;
    PBack: TBitmap;
    function GetBrightnessMode: TBrightnessMode;
    function GetHue: Integer;
    function GetSat: Integer;
    function GetLum: Integer;
    function GetVal: Integer;
    function GetMaxHue: Integer;
    function GetMaxLum: Integer;
    function GetMaxSat: Integer;
    function GetMaxVal: Integer;
    function GetRed: Integer;
    function GetGreen: Integer;
    function GetBlue: Integer;
    procedure SetBlue(B: integer);
    procedure SetBrightnessMode(bm: TBrightnessMode);
    procedure SetGreen(G: integer);
    procedure SetHue(H: integer);
    procedure SetLum(L: integer);
    procedure SetRed(R: integer);
    procedure SetSat(S: integer);
    procedure SetVal(V: Integer);
    procedure SetMaxHue(H: Integer);
    procedure SetMaxSat(S: Integer);
    procedure SetMaxLum(L: Integer);
    procedure SetMaxVal(V: Integer);
    procedure SetHHint(h: string);
    procedure SetSLHint(h: string);
    procedure SetSLMenu(m: TPopupMenu);
    procedure SetHMenu(m: TPopupMenu);
    procedure SetHCursor(c: TCursor);
    procedure SetSLCursor(c: TCursor);
    procedure HPickerChange(Sender: TObject);
    procedure SLPickerChange(Sender: TObject);
  protected
    procedure DoChange; override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    function GetColorUnderCursor: TColor; override;
    function GetSelectedColor: TColor; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetSelectedColor(c: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
//    procedure BeginUpdate; override;
//    procedure EndUpdate(DoUpdate: Boolean = true); override;
    function GetHexColorUnderCursor: string; override;
    function GetSelectedHexColor: string;
    procedure SetFocus; override;
    property ColorUnderCursor;
    property Red: integer read GetRed write SetRed default 255;
    property Green: integer read GetGreen write SetGreen default 0;
    property Blue: integer read GetBlue write SetBlue default 0;
  published
    property BrightnessMode: TBrightnessMode read GetBrightnessMode
      write SetBrightnessMode default bmValue;
    property SelectedColor default clRed;
    property Hue: integer read GetHue write SetHue default 0;
    property Saturation: integer read GetSat write SetSat default 255;
    property Value: Integer read GetVal write SetVal default 255;
    property Luminance: integer read GetLum write SetLum default 127;
    property HPickerPopupMenu: TPopupMenu read FHMenu write SetHMenu;
    property SLPickerPopupMenu: TPopupMenu read FSLMenu write SetSLMenu;
    property HPickerHintFormat: string read FHHint write SetHHint;
    property SLPickerHintFormat: string read FSLHint write SetSLHint;
    property HPickerCursor: TCursor read FHCursor write SetHCursor default crDefault;
    property SLPickerCursor: TCursor read FSLCursor write SetSLCursor default crDefault;
    property MaxHue: Integer read GetMaxHue write SetMaxHue default 360;
    property MaxSaturation: Integer read GetMaxSat write SetMaxSat default 240;
    property MaxLuminance: Integer read GetMaxLum write SetMaxLum default 240;
    property TabStop default true;
    property ShowHint;
    property ParentShowHint;
    property Anchors;
    property Align;
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

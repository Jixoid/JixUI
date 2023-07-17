unit HSLRingPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics,
  Forms, Menus, Math, Themes,
  mbColorConv, HRingPicker, SLColorPicker, HTMLColors, mbBasicPicker;

type
  THSLRingPicker = class(TmbBasicPicker)
  private
    FRingPicker: THRingPicker;
    FSLPicker: TSLColorPicker;
    FSelectedColor: TColor;
//    FRValue, FGValue, FBValue: integer;
    FRingHint, FSLHint: string;
    FSLMenu, FRingMenu: TPopupMenu;
    FSLCursor, FRingCursor: TCursor;
    PBack: TBitmap;
    function GetBrightnessMode: TBrightnessMode;
    function GetHue: Integer;
    function GetLum: Integer;
    function GetSat: Integer;
    function GetVal: Integer;
    function GetMaxHue: Integer;
    function GetMaxLum: Integer;
    function GetMaxSat: Integer;
    function GetRed: Integer;
    function GetGreen: Integer;
    function GetBlue: Integer;
    function GetLVHint(AMode: TBrightnessMode): String;
    procedure SetBrightnessMode(AMode: TBrightnessMode);
    procedure SetHue(H: integer);
    procedure SetSat(S: integer);
    procedure SetLum(L: integer);
    procedure SetVal(V: Integer);
    procedure SetMaxHue(H: Integer);
    procedure SetMaxLum(L: Integer);
    procedure SetMaxSat(S: Integer);
    procedure SetRed(R: integer);
    procedure SetGreen(G: integer);
    procedure SetBlue(B: integer);
    procedure SetRingHint(h: string);
    procedure SetSLMenu(m: TPopupMenu);
    procedure SetRingMenu(m: TPopupMenu);
    procedure SetRingCursor(c: TCursor);
    procedure SetSLCursor(c: TCursor);
    procedure SetLVHint(AMode: TBrightnessMode; AText: String);
  protected
//    procedure CreateWnd; override;
    procedure DoChange; override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    function GetColorUnderCursor: TColor; override;
    function GetSelectedColor: TColor; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure RingPickerChange(Sender: TObject);
    procedure SetSelectedColor(c: TColor); override;
    procedure SLPickerChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetHexColorUnderCursor: string; override;
    function GetSelectedHexColor: string;
    procedure SetFocus; override;
    property ColorUnderCursor;
    property Red: integer read GetRed write SetRed;
    property Green: integer read GetGreen write SetGreen;
    property Blue: integer read GetBlue write SetBlue;
  published
    property BrightnessMode: TBrightnessMode read GetBrightnessMode
      write SetBrightnessMode default bmValue;
    property Hue: integer read GetHue write SetHue default 0;
    property Saturation: integer read GetSat write SetSat default 255;
    property Luminance: integer read GetLum write SetLum default 127;
    property Value: Integer read GetVal write SetVal default 255;
    property SelectedColor default clRed;
    property RingPickerPopupMenu: TPopupMenu read FRingMenu write SetRingMenu;
    property SLPickerPopupMenu: TPopupMenu read FSLMenu write SetSLMenu;
    property RingPickerHintFormat: string read FRingHint write SetRingHint;
    property SLPickerHintFormat: string index bmLuminance read GetLVHint write SetLVHint;
    property SVPickerHintFormat: String index bmValue read GetLVHint write SetLVHint;
    property RingPickerCursor: TCursor read FRingCursor write SetRingCursor default crDefault;
    property SLPickerCursor: TCursor read FSLCursor write SetSLCursor default crDefault;
    property MaxHue: Integer read GetMaxHue write SetMaxHue default 360;
    property MaxLuminance: Integer read GetMaxLum write SetMaxLum default 240;
    property MaxSaturation: Integer read GetMaxSat write SetMaxSat default 240;
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
    property OnChange; //: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseMove;
  end;

Implementation
End.

unit mbColorPickerControl;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Classes, Controls, Graphics, Forms, Themes,
  HTMLColors, mbColorConv, mbBasicPicker;

type
  TMarkerStyle = (msCircle, msSquare, msCross, msCrossCirc);

  { TmbCustomPicker }

  TmbCustomPicker = class(TmbBasicPicker)
  private
    FHintFormat: string;
    FMarkerStyle: TMarkerStyle;
    FWebSafe: boolean;
    procedure SetMarkerStyle(s: TMarkerStyle);
    procedure SetWebSafe(s: boolean);
  protected
    FSelected: TColor;
    mx, my: integer;
    procedure CreateGradient; override;
    function GetHintStr({%H-}X, {%H-}Y: Integer): String; override;
    function GetSelectedColor: TColor; override;
    procedure InternalDrawMarker(X, Y: Integer; C: TColor);
    procedure SetSelectedColor(C: TColor); override;
    procedure WebSafeChanged; dynamic;
    procedure CMGotFocus(var Message: TLMessage); message CM_ENTER;
    procedure CMLostFocus(var Message: TLMessage); message CM_EXIT;
//    procedure CMMouseLeave(var Message: TLMessage); message CM_MOUSELEAVE;
    property MarkerStyle: TMarkerStyle read FMarkerStyle write SetMarkerStyle;
  public
    constructor Create(AOwner: TComponent); override;
    property ColorUnderCursor;
  published
    property HintFormat: string read FHintFormat write FHintFormat;
    property WebSafe: boolean read FWebSafe write SetWebSafe default false;
  end;

  TmbColorPickerControl = class(TmbCustomPicker)
  published
    property Anchors;
    property Align;
    property BorderSpacing;
    property ShowHint;
    property ParentShowHint;
    property Visible;
    property Enabled;
    property PopupMenu;
    property TabOrder;
    property TabStop default true;
    property Color;
    property ParentColor;
    property DragCursor;
    property DragMode;
    property DragKind;
    property Constraints;
    property OnContextPopup;
    property OnGetHintStr;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDrag;
  end;

  TmbHSLVColorPickerControl = class(TmbColorPickerControl)
  private
    FBrightnessMode: TBrightnessMode;
    function GetHue: Integer;
    function GetLum: Integer;
    function GetSat: Integer;
    function GetVal: Integer;
    function GetRed: Integer;
    function GetGreen: Integer;
    function GetBlue: Integer;
    procedure SetHue(h: integer);
    procedure SetLum(L: Integer);
    procedure SetSat(s: integer);
    procedure SetVal(v: integer);
    procedure SetRed(R: Integer);
    procedure SetGreen(G: Integer);
    procedure SetBlue(B: Integer);
  protected
    FHue, FSat, FLum, FVal: Double;
    FMaxHue, FMaxSat, FMaxLum, FMaxVal: Integer;
    procedure ColorToHSLV(c: TColor; var H, S, L, V: Double);
    procedure CorrectCoords(var x, y: integer);
    function HSLVtoColor(H, S, L, V: Double): TColor;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SelectColor({%H-}x, {%H-}y: Integer); virtual;
    procedure SetBrightnessMode(AMode: TBrightnessMode); virtual;
    procedure SetMaxHue(H: Integer); virtual;
    procedure SetMaxLum(L: Integer); virtual;
    procedure SetMaxSat(S: Integer); virtual;
    procedure SetMaxVal(V: Integer); virtual;
    procedure SetRelHue(H: Double); virtual;
    procedure SetRelLum(L: Double); virtual;
    procedure SetRelSat(S: Double); virtual;
    procedure SetRelVal(V: Double); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property RelHue: Double read FHue write SetRelHue;
    property RelSaturation: Double read FSat write SetRelSat;
    property RelLuminance: Double read FLum write SetRelLum;
    property RelValue: Double read FVal write SetRelVal;
    property Red: Integer read GetRed write SetRed;
    property Green: Integer read GetGreen write SetGreen;
    property Blue: Integer read GetBlue write SetBlue;
  published
    property BrightnessMode: TBrightnessMode
      read FBrightnessMode write SetBrightnessMode default bmLuminance;
    property Hue: integer read GetHue write SetHue;
    property Luminance: Integer read GetLum write SetLum;
    property Saturation: integer read GetSat write SetSat;
    property Value: integer read GetVal write SetVal;
    property MaxHue: Integer read FMaxHue write SetMaxHue default 360;
    property MaxSaturation: Integer read FMaxSat write SetMaxSat default 255;
    property MaxLuminance: Integer read FMaxLum write SetMaxLum default 255;
    property MaxValue: Integer read FMaxVal write SetMaxVal default 255;
  end;

Implementation
End.

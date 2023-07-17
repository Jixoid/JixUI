unit mbTrackBarPicker;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Classes, Controls, Graphics, Forms,
  Themes, ExtCtrls,
  PalUtils, mbColorConv, mbBasicPicker;

const
  TBA_Resize = 0;
  TBA_Paint = 1;
  TBA_MouseMove = 2;
  TBA_MouseDown = 3;
  TBA_MouseUp = 4;
  TBA_WheelUp = 5;
  TBA_WheelDown = 6;
  TBA_VKUp = 7;
  TBA_VKCtrlUp = 8;
  TBA_VKDown = 9;
  TBA_VKCtrlDown = 10;
  TBA_VKLeft = 11;
  TBA_VKCtrlLeft = 12;
  TBA_VKRight = 13;
  TBA_VKCtrlRight = 14;
  TBA_RedoBMP = 15;

type
  TTrackBarLayout = (lyHorizontal, lyVertical);
  TSliderPlacement = (spBefore, spAfter, spBoth);
  TSelIndicator = (siArrows, siRect);

  { TmbTrackBarPicker }

  TmbTrackBarPicker = class(TmbBasicPicker)
  private
    Aw, Ah: integer;
    mx, my: integer;
    FBevelInner: TBevelCut;
    FBevelOuter: TBevelCut;
    FBevelWidth: TBevelWidth;
    FBorderStyle: TBorderStyle;
    FHintFormat: string;
    FIncrement: integer;
    FNewArrowStyle: boolean;
    FPlacement: TSliderPlacement;
    FSelIndicator: TSelIndicator;
    FWebSafe: boolean;
    procedure CalcPickRect;
    procedure DrawMarker(p: integer);
    procedure SetBevelInner(Value: TBevelCut);
    procedure SetBevelOuter(Value: TBevelCut);
    procedure SetBevelWidth(Value: TBevelWidth);
    procedure SetLayout(Value: TTrackBarLayout);
    procedure SetNewArrowStyle(s: boolean);
    procedure SetPlacement(Value: TSliderPlacement);
    procedure SetSelIndicator(Value: TSelIndicator);
    procedure SetWebSafe(s: boolean);
    function XToArrowPos(p: integer): integer;
    function YToArrowPos(p: integer): integer;
  protected
    FArrowPos: integer;
//    FBack: TBitmap;
    FLayout: TTrackBarLayout;
    FLimit: integer;
    FPickRect: TRect;
    procedure CreateGradient; override;
    procedure CreateWnd; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure DrawFrames; dynamic;
    procedure Execute(tbaAction: integer); dynamic;
    function GetArrowPos: integer; dynamic;
    function GetHintPos(X, Y: Integer): TPoint; override;
    function GetHintStr(X, Y: Integer): String; override;
    function GetSelectedValue: integer; virtual; abstract;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetBorderStyle(Value: TBorderStyle); override;
    procedure CMGotFocus(var Message: TLMessage); message CM_ENTER;
    procedure CMLostFocus(var Message: TLMessage); message CM_EXIT;
    property HintFormat: string read FHintFormat write FHintFormat;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvNone;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvNone;
    property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property Increment: integer read FIncrement write FIncrement default 1;
    property Layout: TTrackBarLayout read FLayout write SetLayout default lyHorizontal;
    property ArrowPlacement: TSliderPlacement read FPlacement write SetPlacement default spAfter;
    property NewArrowStyle: boolean read FNewArrowStyle write SetNewArrowStyle default false;
    property SelectionIndicator: TSelIndicator read FSelIndicator write SetSelIndicator default siArrows;
    property WebSafe: boolean read FWebSafe write SetWebSafe default false;
    property TabStop default true;
    property ShowHint;
    property Color;
    property ParentColor;
    property ParentShowHint default true;
    property Anchors;
    property Align;
    property BorderSpacing;
    property Visible;
    property Enabled;
    property PopupMenu;
    property TabOrder;
    property DragCursor;
    property DragMode;
    property DragKind;
    property Constraints;
    property OnChange;
    property OnContextPopup;
    property OnGetHintStr;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
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

  { TmbHSLVTrackbarPicker }

  TmbHSLVTrackbarPicker = class(TmbTrackbarPicker)
  private
    FBrightnessMode: TBrightnessMode;
    function GetHue: Integer;
    function GetLum: Integer;
    function GetSat: Integer;
    function GetVal: Integer;
    procedure SetHue(h: integer);
    procedure SetLum(L: Integer);
    procedure SetSat(s: integer);
    procedure SetVal(v: integer);
  protected
    FHue, FSat, FLum, FVal: Double;
    FMaxHue, FMaxSat, FMaxLum, FMaxVal: Integer;
    procedure ColorToHSLV(c: TColor; var H, S, L, V: Double);
    function GetSelectedColor: TColor; override;
    function HSLVtoColor(H, S, L, V: Double): TColor;
    procedure SetBrightnessMode(AMode: TBrightnessMode); virtual;
    procedure SetMaxHue(h: Integer); virtual;
    procedure SetMaxLum(L: Integer); virtual;
    procedure SetMaxSat(s: Integer); virtual;
    procedure SetMaxVal(v: Integer); virtual;
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

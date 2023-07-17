unit HexaColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

//{$I mxs.inc}

uses
  LCLIntf, LCLType, LMessages, SysUtils, Classes, Controls, Graphics, StdCtrls,
  Forms, Themes, Math,
  HTMLColors, mbBasicPicker;

const
  CustomCell = -2;
  NoCell = -1;

type
  TMarker = (smArrow, smRect);

  TCombEntry = record
    Position: TPoint;
    Color: COLORREF;
    TabIndex: integer;
  end;

  TCombArray = array of TCombEntry;

  TFloatPoint = record
    X, Y: Extended;
  end;

  TRGBrec = record
    Red, Green, Blue: Single;
  end;

  TSelectionMode = (smNone, smColor, smBW, smRamp);

  THexaColorPicker = class(TmbBasicPicker)
  private
    FIncrement: integer;
    FSelectedCombIndex: integer;
    mX, mY: integer;
    FHintFormat: string;
    FUnderCursor: TColor;
    //FOnChange,
    FOnIntensityChange: TNotifyEvent;
    FCurrentColor: TColor;
    FSelectedIndex: Integer;
    FColorCombRect, FBWCombRect, FSliderRect, FCustomColorRect: TRect;
    FCombSize, FLevels: Integer;
    FBWCombs, FColorCombs: TCombArray;
    FCombCorners: array[0..5] of TFloatPoint;
    FCenterColor: TRGBrec;
    FCenterIntensity: Single;
    FSliderWidth: integer;
    FCustomIndex: Integer;  // If FSelectedIndex contains CustomCell then this index shows
                            // which index in the custom area has been selected.
                            // Positive values indicate the color comb and negative values
                            // indicate the B&W combs (complement). This value is offset with
                            // 1 to use index 0 to show no selection.
    FRadius: Integer;
    FSelectionMode: TSelectionMode;
    FSliderVisible: boolean;
    FMarker: TMarker;
    FNewArrowStyle: boolean;
    FIntensityText: string;
    procedure CalculateCombLayout;
    procedure ChangeIntensity(increase: boolean);
    procedure DrawAll;
    procedure DrawComb(ACanvas: TCanvas; X, Y, Size: Integer);
    procedure DrawCombControls(ACanvas: TCanvas);
    procedure EndSelection;
    procedure EnumerateCombs;
    function FindBWArea(X, Y: Integer): Integer;
    function FindColorArea(X, Y: Integer): Integer;
    function GetIntensity: integer;
    function GetNextCombIndex(i: integer): integer;
    function GetPreviousCombIndex(i: integer): integer;
    procedure HandleCustomColors(var Message: TLMMouse);
    function HandleBWArea(const Message: TLMMouse): Boolean;
    function HandleColorComb(const Message: TLMMouse): Boolean;
    function HandleSlider(const Message: TLMMouse): Boolean;
    procedure Initialize;
    function PtInComb(Comb: TCombEntry; P: TPoint; Scale: Integer): Boolean;
    procedure SetIntensity(v: integer);
    procedure SetNewArrowStyle(Value: boolean);
    procedure SetMarker(Value: TMarker);
    procedure SetRadius(r: integer);
    procedure SetSliderVisible(Value: boolean);
    procedure SetSliderWidth(w: integer);
    function SelectAvailableColor(Color: TColor): boolean;
    procedure SelectColor(Color: TColor);
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetSelectedColor(Value: TColor); override;
    procedure CMHintShow(var Message: TLMessage); message CM_HINTSHOW;
    procedure WMLButtonDown(var Message: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TLMLButtonUp); message LM_LBUTTONUP;
    procedure WMMouseMove(var Message: TLMMouseMove); message LM_MOUSEMOVE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetColorAtPoint(X, Y: integer): TColor; override;
    function GetColorUnderCursor: TColor; override;
    function GetHexColorUnderCursor: string; override;
    function GetHexColorAtPoint(X, Y: integer): string;
    function GetSelectedCombIndex: integer;
    procedure SelectCombIndex(i: integer);
    property ColorUnderCursor: TColor read GetColorUnderCursor;
  published
    property Align;
    property Anchors;
    property HintFormat: string read FHintFormat write FHintFormat;
    property Intensity: integer read GetIntensity write SetIntensity default 100;
    property IntensityIncrement: integer read FIncrement write FIncrement default 1;
    property IntensityText: string read FIntensityText write FIntensityText;
    property NewArrowStyle: boolean read FNewArrowStyle write SetNewArrowStyle default false;
    property SelectedColor: TColor read FCurrentColor write SetSelectedColor default clBlack;
    property SliderVisible: boolean read FSliderVisible write SetSliderVisible default true;
    property SliderWidth: integer read FSliderWidth write SetSliderWidth default 12;
    property SliderMarker: TMarker read FMarker write SetMarker default smArrow;
    property ShowHint default true;
    property TabStop default true;
    property Visible;
    property Enabled;
    property PopupMenu;
    property TabOrder;
    property Color;
    property ParentColor;
    property DragCursor;
    property DragMode;
    property DragKind;
    property Constraints;
    property OnChange; //: TNotifyEvent read FOnChange write FOnChange;
    property OnIntensityChange: TNotifyEvent read FOnIntensityChange write FOnIntensityChange;
    property OnDblClick;
    property OnContextPopup;
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

  const
    DefCenterColor: TRGBrec = (Red: 1; Green: 1; Blue: 1);  // White
    DefColors: array[0..5] of TRGBrec = (
      (Red: 1; Green: 0; Blue: 1),     // Magenta
      (Red: 1; Green: 0; Blue: 0),     // Red
      (Red: 1; Green: 1; Blue: 0),     // Yellow
      (Red: 0; Green: 1; Blue: 0),     // Green
      (Red: 0; Green: 1; Blue: 1),     // Cyan
      (Red: 0; Green: 0; Blue: 1)      // Blue
    );
    DefCenter: TFloatPoint = (X: 0; Y: 0);


Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCMDButton;

{$I bgracontrols.map.inc}

// Set this to show number of repaint in each MDBUTTON
{ $DEFINE MDBUTTON_DEBUG}

// Set this to animate only a MDBUTTON at a time
{$DEFINE MDBUTTON_ANIMATEONLYONE}

interface

uses
  Classes, SysUtils, Types, {$IFDEF FPC}LResources,{$ELSE}BGRAGraphics, GraphType, FPImage,{$ENDIF}
  Forms, Controls, Graphics, Dialogs,
  BCBaseCtrls, BGRABitmap, BGRABitmapTypes, ExtCtrls, Math, BGRABlend;

type
  TBCMDButtonCheckMarkPosition = (cmpBottom,cmpTop,cmpLeft,cmpRight);

var
  // Default icons for Check Box
  {BCMDBUTTONBALLOTBOX: string = '☐'; // '✗'
  BCMDBUTTONBALLOTBOXWITHCHECK: string = '☑'; // '✓'

  // Default icons for Radio Button
  BCMDBUTTONRADIOBUTTON: string = '🔘';
  BCMDBUTTONRADIOBUTTONCIRCLE: string = '◯';}

  // Characters that can be used on systems that lack of the previous unicode symbols
  BCMDBUTTONBALLOTBOX: string = '[  ]';
  BCMDBUTTONBALLOTBOXWITHCHECK: string = '[X]';
  BCMDBUTTONRADIOBUTTON: string = '[O]';
  BCMDBUTTONRADIOBUTTONCIRCLE: string = '[  ]';

  // Animation speed
  // Possible values: between 0 and 1
  // 0 is an infinite animation that display nothing (only redraw itself)
  // 1 is the faster animation (like no animation, from 0 to 1 in 1 frame)
  // Recommended values: between 0.01 (slow) and 0.1 (fast), default 0.04
  // Hint: turn on debug to see how much frames are rendered
  BCMDBUTTONANIMATIONSPEED: double = 0.04;

  // Global enable/disable animations
  BCMDBUTTONANIMATION: boolean = True;

  // Global posiotn of checkmarks 0=bottom, 1=top, 2=left, 3=right
  BCMDBUTTONCHECKMARKPOSITION : TBCMDButtonCheckMarkPosition = cmpBottom;

  BCMDBUTTONCHECKMARKCOLOR : TColor = $00BB513F;

const
  // Timer speed: default 15 (a bit more than 60 fps)
  // Other values: 16 (60 fps) 20 (50 fps) 25 (40 fps) 33 (30 fps)
  // Hint: 15 is the smoothest -tested- value on Windows, even if 16 is closer to 60 fps
  //       * values below 15 are not noticeable
  //       * higher values are not smooth
  // Hint: changing this doesn't change the ammount of frames rendered,
  //       only changes the time between frames
  // Hint: if you decrease MDBUTTONTIMERSPEED, increase BCMDBUTTONANIMATIONSPEED
  //       to keep a smooth animation
  BCMDBUTTONTIMERSPEED: integer = 15;

type
  TBCMDButtonState = (mdbsNormal, mdbsHover, mdbsActive);
  TBCMDButtonKind = (mdbkNormal, mdbkToggle, mdbkToggleGroup, mdbkCheckBox,
    mdbkRadioButton, mdbkTab);

  { TBCMDButtonStyle }

  TBCMDButtonStyle = class(TPersistent)
  private
    FColor: TColor;
    FOnChange: TNotifyEvent;
    FTextColor: TColor;
    procedure SetFColor(AValue: TColor);
    procedure SetFTextColor(AValue: TColor);
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
  published
    property Color: TColor read FColor write SetFColor;
    property TextColor: TColor read FTextColor write SetFTextColor;
  end;

  { TCustomBCMDButton }

  TCustomBCMDButton = class(TBGRAGraphicCtrl)
  private
    FChecked: boolean;
    FKind: TBCMDButtonKind;
    {$IFDEF INDEBUG}
    FCount: integer;
    {$ENDIF}
    FRounding: integer;
    FTextAutoSize: boolean;
    FTextProportional: boolean;
    FTextProportionalRatio: single;
    FTimer: TTimer;
    FPercent: double;
    FCircleSize: double;
    FCX, FCY: integer;
    FAlphaPercent: double;
    FAlignment: TAlignment;
    FAnimation: boolean;
    FState: TBCMDButtonState;
    FStyleActive: TBCMDButtonStyle;
    FStyleDisabled: TBCMDButtonStyle;
    FStyleHover: TBCMDButtonStyle;
    FStyleNormal: TBCMDButtonStyle;
    FTextLayout: TTextLayout;
    procedure OnChangeStyle(Sender: TObject);
    procedure SetFAlignment(AValue: TAlignment);
    procedure SetFAnimation(AValue: boolean);
    procedure SetFChecked(AValue: boolean);
    procedure SetFKind(AValue: TBCMDButtonKind);
    procedure SetFStyleActive(AValue: TBCMDButtonStyle);
    procedure SetFStyleDisabled(AValue: TBCMDButtonStyle);
    procedure SetFStyleHover(AValue: TBCMDButtonStyle);
    procedure SetFStyleNormal(AValue: TBCMDButtonStyle);
    procedure SetFTextAutoSize(AValue: boolean);
    procedure SetFTextLayout(AValue: TTextLayout);
    procedure SetFTextProportional(AValue: boolean);
    procedure SetFTextProportionalRatio(AValue: single);
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
    {%H-}WithThemeSpace: boolean); override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure RealSetText(const Value: TCaption); override;
    procedure OnTimer(Sender: TObject);
    procedure OnStartTimer(Sender: TObject);
    procedure OnStopTimer(Sender: TObject);
    function easeInOutQuad(t: double): double;
    function easeOutQuad(t: double): double;
    procedure UncheckOthers;
    class function GetControlClassDefaultSize: TSize; override;
    function GetRealCaption: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SelectAll;
    procedure UnselectAll;
    procedure InvertSelection;
    function GetSelected: TStringList;
  published
    property Animation: boolean read FAnimation write SetFAnimation default False;
    property Alignment: TAlignment read FAlignment write SetFAlignment default taCenter;
    property TextLayout: TTextLayout
      read FTextLayout write SetFTextLayout default tlCenter;
    property StyleNormal: TBCMDButtonStyle read FStyleNormal write SetFStyleNormal;
    property StyleHover: TBCMDButtonStyle read FStyleHover write SetFStyleHover;
    property StyleActive: TBCMDButtonStyle read FStyleActive write SetFStyleActive;
    property StyleDisabled: TBCMDButtonStyle read FStyleDisabled write SetFStyleDisabled;
    property Checked: boolean read FChecked write SetFChecked default False;
    property Kind: TBCMDButtonKind read FKind write SetFKind default mdbkNormal;
    // If text size is used to measure buttons
    // Disable it if you use the buttons in a grid, for example
    property TextAutoSize: boolean read FTextAutoSize write SetFTextAutoSize;
    // Enable it if you want that text size grows with height
    property TextProportional: boolean read FTextProportional write SetFTextProportional;
    // Each character font height proportional to height of control
    // Set it in conjunction with TextProportional, values recommended between 0...1
    property TextProportionalRatio: single read FTextProportionalRatio
      write SetFTextProportionalRatio;
  end;

  TBCMDButton = class(TCustomBCMDButton)
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    {$IFDEF FPC} //#
    property OnChangeBounds;
    {$ENDIF}
    //property Cancel;
    property Caption;
    property Color;
    property Constraints;
    //property Default;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBidiMode;
    //property ModalResult;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    //property OnEnter;
    //property OnExit;
    //property OnKeyDown;
    //property OnKeyPress;
    //property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    //property OnUTF8KeyPress;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    //property TabOrder;
    //property TabStop;
    property Visible;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

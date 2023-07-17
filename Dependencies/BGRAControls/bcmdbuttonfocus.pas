// SPDX-License-Identifier: LGPL-3.0-linking-exception
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCMDButtonFocus;

{$I bgracontrols.map.inc}

// Set this to show number of repaint in each MDBUTTON
{ $DEFINE MDBUTTON_DEBUG}

// Set this to animate only a MDBUTTON at a time
{ $DEFINE MDBUTTON_ANIMATEONLYONE}

interface

uses
  Classes, SysUtils, Types, {$IFDEF FPC}LCLType, LResources, LMessages,{$ENDIF}
  Forms, Controls, Graphics, Dialogs,
  {$IFNDEF FPC}Windows, Messages, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCBaseCtrls, BGRABitmap, BGRABitmapTypes, ExtCtrls, Math, BGRABlend, BCMDButton;

type

  { TCustomBCMDButtonFocus }

  TCustomBCMDButtonFocus = class(TBGRACustomCtrl)
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
    // START / MDBUTTONFOCUS ONLY
    procedure WMSetFocus(var Message: {$IFDEF FPC}TLMSetFocus{$ELSE}TWMKillFocus{$ENDIF}); message {$IFDEF FPC}LM_SETFOCUS{$ELSE}WM_SETFOCUS{$ENDIF};
    procedure WMKillFocus(var Message: {$IFDEF FPC}TLMKillFocus{$ELSE}TWMKillFocus{$ENDIF}); message {$IFDEF FPC}LM_KILLFOCUS{$ELSE}WM_KILLFOCUS{$ENDIF};
    procedure UpdateFocus(AFocused: boolean);
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
    // END / MDBUTTONFOCUS ONLY
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
    class function GetControlClassDefaultSize: TSize;override;
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

  TBCMDButtonFocus = class(TCustomBCMDButtonFocus)
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

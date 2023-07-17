// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Created by BGRA Controls Team
  Dibo, Circular, lainz (007) and contributors.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCTrackbarUpdown;

{$I bgracontrols.map.inc}

interface

uses
  {$IFDEF FPC}LCLType, LResources,{$ENDIF}
  Classes, SysUtils, Types, Forms, Controls, Graphics, Dialogs,
  {$IFNDEF FPC}BGRAGraphics, GraphType, FPImage, {$ENDIF}
  ExtCtrls, BGRABitmap, BCBaseCtrls, BCTypes;

type
  TTrackBarUpDownChangeEvent = procedure(Sender: TObject; AByUser: boolean) of object;

  { TCustomBCTrackbarUpdown }

  TCustomBCTrackbarUpdown = class(TBCCustomControl)
  protected
    FHandlingUserInput: boolean;
    FLongTimeInterval,FShortTimeInterval: integer;
    FMinValue,FMaxValue,FIncrement,FValue: integer;
    FAllowNegativeValues: boolean;
    FStartNegativeValue: boolean;
    FBarExponent: single;
    FSelStart,FSelLength: integer;
    FEmptyText: boolean;
    FBarClick,FUpClick,FDownClick: boolean;

    FTimer: TTimer;
    FOnChange: TTrackBarUpDownChangeEvent;
    FBCBorder: TBCBorder;
    FBCRounding: TBCRounding;
    FBCBackground: TBCBackground;
    FBCButtonBackground,FBCButtonDownBackground: TBCBackground;
    FArrowColor: TColor;
    FHasTrackBar: boolean;

    FCanvasScaling: double;
    FTextLeft: Integer;
    FBarLeft,FBarTop,FBarWidth,FBarHeight: Integer;
    FUpDownWidth: Integer;
    FUpDownLeft: Integer;
    FDownButtonTop: integer;
    function GetValue: integer;
    procedure SetAllowNegativeValues(AValue: boolean);
    procedure SetArrowColor(AValue: TColor);
    procedure SetHasTrackBar(AValue: boolean);
    procedure SetBarExponent(AValue: single);
    procedure SetBCBackground(AValue: TBCBackground);
    procedure SetBCBorder(AValue: TBCBorder);
    procedure SetBCButtonBackground(AValue: TBCBackground);
    procedure SetBCButtonDownBackground(AValue: TBCBackground);
    procedure SetBCRounding(AValue: TBCRounding);
    procedure OnChangeProperty({%H-}Sender: TObject; {%H-}AData: PtrInt);
    procedure Timer({%H-}Sender: TObject);
    procedure RenderOnBitmap(ABitmap: TBGRABitmap);
    procedure DrawControl; override;
    procedure DoSelectAll;
    function GetText: string; virtual;
    procedure SetText(AValue: string); virtual;
    procedure EnabledChanged; override;
    procedure NotifyChange; virtual;
    procedure SetIncrement(AValue: integer);
    procedure SetMaxValue(AValue: integer);
    procedure SetMinValue(AValue: integer);
    procedure SetValue(AValue: integer);
    function ValueToBarPos(AValue: integer): integer;
    function BarPosToValue(ABarPos: integer): integer;
    procedure MouseDown(Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure UTF8KeyPress(var UTF8Key: {$IFDEF FPC}TUTF8Char{$ELSE}String{$ENDIF}); override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SelectAll;
    function RemoveSelection: boolean; //returns True if there was a selection to be removed
    procedure DelayTimer; //use after the program has been busy updating something according to the value of this component
    procedure SetFocus; override;
    destructor Destroy; override;
    property Border: TBCBorder read FBCBorder write SetBCBorder;
    property Background: TBCBackground read FBCBackground write SetBCBackground;
    property ButtonBackground: TBCBackground read FBCButtonBackground write SetBCButtonBackground;
    property ButtonDownBackground: TBCBackground read FBCButtonDownBackground write SetBCButtonDownBackground;
    property Rounding: TBCRounding read FBCRounding write SetBCRounding;
    property ArrowColor: TColor read FArrowColor write SetArrowColor;
    property HasTrackBar: boolean read FHasTrackBar write SetHasTrackBar;

    property AllowNegativeValues: boolean read FAllowNegativeValues write SetAllowNegativeValues;
    property BarExponent: single read FBarExponent write SetBarExponent;
    property Increment: integer read FIncrement write SetIncrement;
    property LongTimeInterval: integer read FLongTimeInterval write FLongTimeInterval;
    property MinValue: integer read FMinValue write SetMinValue;
    property MaxValue: integer read FMaxValue write SetMaxValue;
    property OnChange: TTrackBarUpDownChangeEvent read FOnChange write FOnChange;
    property Text: string read GetText write SetText;
    property Value: integer read GetValue write SetValue;
    property SelStart: integer read FSelStart;
    property SelLength: integer read FSelLength;
    property ShortTimeInterval: integer read FShortTimeInterval write FShortTimeInterval;
  end;

  TBCTrackbarUpdown = class(TCustomBCTrackbarUpdown)
  published
    property AllowNegativeValues;
    property BarExponent;
    property Increment;
    property LongTimeInterval;
    property MinValue;
    property MaxValue;
    property OnChange;
    property Value;
    property SelStart;
    property SelLength;
    property ShortTimeInterval;
    property Background;
    property ButtonBackground;
    property ButtonDownBackground;
    property Border;
    property Rounding;
    property Font;
    property HasTrackBar;
    property ArrowColor;

    //inherited
    property Align;
    property Anchors;
    property BorderSpacing;
    property ChildSizing;
    {$IFDEF FPC} //#
    property OnGetDockCaption;
    {$ENDIF}
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Base framework classes

  originally written in 2012 by Krzysztof Dibowski dibowski at interia.pl 
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}

unit BCBaseCtrls;
{$WARN 5062 off : Found abstract method: $1}
{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Graphics,
  {$IFNDEF FPC}Windows, Messages, BGRAGraphics, GraphType, FPImage, {$ELSE} LCLType,{$ENDIF}
  BGRABitmap, BGRABitmapTypes;

type

{$IFNDEF FPC}
  TSpacingSize = Integer;
  TControlCellAlign = (
    ccaFill,
    ccaLeftTop,
    ccaRightBottom,
    ccaCenter
    );
  TControlCellAligns = set of TControlCellAlign;

  TControlBorderSpacingDefault = record
    Left: TSpacingSize;
    Top: TSpacingSize;
    Right: TSpacingSize;
    Bottom: TSpacingSize;
    Around: TSpacingSize;
  end;
  PControlBorderSpacingDefault = ^TControlBorderSpacingDefault;


  TBGRAGraphicCtrl = class;
  TBGRACustomCtrl  = class;
  TControlBorderSpacing = class;

  ILCLControl = interface
   ['{97A3D274-C4BD-4095-9B23-8E50D6E0EA24}']
    procedure DoOnResize;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;{%H-}WithThemeSpace: boolean);
    procedure MouseEnter;
    procedure MouseLeave;
    procedure TextChanged;
    procedure FontChanged(Sender: TObject);
    procedure RealSetText(const Value: TCaption);
    procedure Resize;
    procedure SetColor(Value: TColor);
    function GetColor : TColor;
    function ColorIsStored: boolean;
    function RealGetText: TCaption;
    function CreateControlBorderSpacing: TControlBorderSpacing;
    procedure SetBorderSpacing(const AValue: TControlBorderSpacing);
    procedure SetInitialBounds(aLeft, aTop, aWidth, aHeight: integer);
    procedure InvalidatePreferredSize;
    procedure EnableAutoSizing;
    procedure DoBorderSpacingChange(Sender: TObject; InnerSpaceChanged: Boolean);
    function  GetInstance : TObject;
    function IsInnerBorderStored: boolean;
  end;

  TControlBorderSpacing = class(TPersistent)
  private
    FAround: TSpacingSize;
    FBottom: TSpacingSize;
    FCellAlignHorizontal: TControlCellAlign;
    FCellAlignVertical: TControlCellAlign;
    FControl: ILCLControl;
    FInnerBorder: Integer;
    FLeft: TSpacingSize;
    FOnChange: TNotifyEvent;
    FRight: TSpacingSize;
    FTop: TSpacingSize;
    FDefault: PControlBorderSpacingDefault;
    function GetAroundBottom: Integer;
    function GetAroundLeft: Integer;
    function GetAroundRight: Integer;
    function GetAroundTop: Integer;
    function GetControlBottom: Integer;
    function GetControlHeight: Integer;
    function GetControlLeft: Integer;
    function GetControlRight: Integer;
    function GetControlTop: Integer;
    function GetControlWidth: Integer;
    function IsAroundStored: boolean;
    function IsBottomStored: boolean;
    function IsInnerBorderStored: boolean;
    function IsLeftStored: boolean;
    function IsRightStored: boolean;
    function IsTopStored: boolean;
    procedure SetAround(const AValue: TSpacingSize);
    procedure SetBottom(const AValue: TSpacingSize);
    procedure SetCellAlignHorizontal(const AValue: TControlCellAlign);
    procedure SetCellAlignVertical(const AValue: TControlCellAlign);
    procedure SetInnerBorder(const AValue: Integer);
    procedure SetLeft(const AValue: TSpacingSize);
    procedure SetRight(const AValue: TSpacingSize);
    procedure SetSpace(Kind: TAnchorKind; const AValue: integer);
    procedure SetTop(const AValue: TSpacingSize);
  protected
    procedure Change(InnerSpaceChanged: Boolean); virtual;
  public
    constructor Create(OwnerControl: ILCLControl; ADefault: PControlBorderSpacingDefault = nil);
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    function IsEqual(Spacing: TControlBorderSpacing): boolean;
    procedure GetSpaceAround(var SpaceAround: TRect); virtual;
    function GetSideSpace(Kind: TAnchorKind): Integer; // Around+GetSpace
    function GetSpace(Kind: TAnchorKind): Integer; virtual;
    procedure AutoAdjustLayout(const AXProportion, AYProportion: Double);
  public
    property Control: ILCLControl read FControl;
    property Space[Kind: TAnchorKind]: integer read GetSpace write SetSpace;
    property AroundLeft: Integer read GetAroundLeft;
    property AroundTop: Integer read GetAroundTop;
    property AroundRight: Integer read GetAroundRight;
    property AroundBottom: Integer read GetAroundBottom;
    property ControlLeft: Integer read GetControlLeft;
    property ControlTop: Integer read GetControlTop;
    property ControlWidth: Integer read GetControlWidth;
    property ControlHeight: Integer read GetControlHeight;
    property ControlRight: Integer read GetControlRight;
    property ControlBottom: Integer read GetControlBottom;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Left: TSpacingSize read FLeft write SetLeft stored IsLeftStored;
    property Top: TSpacingSize read FTop write SetTop stored IsTopStored;
    property Right: TSpacingSize read FRight write SetRight stored IsRightStored;
    property Bottom: TSpacingSize read FBottom write SetBottom stored IsBottomStored;
    property Around: TSpacingSize read FAround write SetAround stored IsAroundStored;
    property InnerBorder: Integer read FInnerBorder write SetInnerBorder stored IsInnerBorderStored default 0;
    property CellAlignHorizontal: TControlCellAlign read FCellAlignHorizontal write SetCellAlignHorizontal default ccaFill;
    property CellAlignVertical: TControlCellAlign read FCellAlignVertical write SetCellAlignVertical default ccaFill;
  end;

  TChildControlResizeStyle = (
      crsAnchorAligning, // (like Delphi)
      crsScaleChilds, // scale children equally, keep space between children fixed
      crsHomogenousChildResize, // enlarge children equally (i.e. by the same amount of pixel)
      crsHomogenousSpaceResize // enlarge space between children equally
      {$IFDEF EnablecrsSameSize}
      ,crsSameSize  // each child gets the same size (maybe one pixel difference)
      {$ENDIF}
    );

  TControlChildrenLayout = (
      cclNone,
      cclLeftToRightThenTopToBottom, // if BiDiMode <> bdLeftToRight then it becomes RightToLeft
      cclTopToBottomThenLeftToRight
    );

  TControlChildSizing = class(TPersistent)
  private
    FControl: ILCLControl;
    FControlsPerLine: integer;
    FEnlargeHorizontal: TChildControlResizeStyle;
    FEnlargeVertical: TChildControlResizeStyle;
    FHorizontalSpacing: integer;
    FLayout: TControlChildrenLayout;
    FLeftRightSpacing: integer;
    FOnChange: TNotifyEvent;
    FShrinkHorizontal: TChildControlResizeStyle;
    FShrinkVertical: TChildControlResizeStyle;
    FTopBottomSpacing: integer;
    FVerticalSpacing: integer;
    procedure SetControlsPerLine(const AValue: integer);
    procedure SetEnlargeHorizontal(const AValue: TChildControlResizeStyle);
    procedure SetEnlargeVertical(const AValue: TChildControlResizeStyle);
    procedure SetHorizontalSpacing(const AValue: integer);
    procedure SetLayout(const AValue: TControlChildrenLayout);
    procedure SetLeftRightSpacing(const AValue: integer);
    procedure SetShrinkHorizontal(const AValue: TChildControlResizeStyle);
    procedure SetShrinkVertical(const AValue: TChildControlResizeStyle);
    procedure SetTopBottomSpacing(const AValue: integer);
    procedure SetVerticalSpacing(const AValue: integer);
  protected
    procedure Change; virtual;
  public
    constructor Create(OwnerControl: ILCLControl);
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    function IsEqual(Sizing: TControlChildSizing): boolean;
    procedure SetGridSpacing(Spacing: integer);
  public
    property Control: ILCLControl read FControl;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property LeftRightSpacing: integer read FLeftRightSpacing write SetLeftRightSpacing default 0;
    property TopBottomSpacing: integer read FTopBottomSpacing write SetTopBottomSpacing default 0;
    property HorizontalSpacing: integer read FHorizontalSpacing write SetHorizontalSpacing default 0;
    property VerticalSpacing: integer read FVerticalSpacing write SetVerticalSpacing default 0;
    property EnlargeHorizontal: TChildControlResizeStyle read FEnlargeHorizontal
                           write SetEnlargeHorizontal default crsAnchorAligning;
    property EnlargeVertical: TChildControlResizeStyle read FEnlargeVertical
                             write SetEnlargeVertical default crsAnchorAligning;
    property ShrinkHorizontal: TChildControlResizeStyle read FShrinkHorizontal
                            write SetShrinkHorizontal default crsAnchorAligning;
    property ShrinkVertical: TChildControlResizeStyle read FShrinkVertical
                              write SetShrinkVertical default crsAnchorAligning;
    property Layout: TControlChildrenLayout read FLayout write SetLayout default cclNone;
    property ControlsPerLine: integer read FControlsPerLine write SetControlsPerLine default 0;
  end;
{$ENDIF}

{$IFDEF FPC}
  TBGRAGraphicCtrl = class(TGraphicControl)
{$ELSE}
  TBGRAGraphicCtrl = class(TGraphicControl, ILCLControl)
  protected
    FBorderSpacing: TControlBorderSpacing;
    FOnChange: TNotifyEvent;
    FMouseInClient: boolean;
    procedure DoOnResize; virtual;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;{%H-}WithThemeSpace: boolean); virtual;
    procedure MouseEnter;  virtual;
    procedure MouseLeave;  virtual;
    procedure TextChanged; virtual;
    procedure FontChanged(Sender: TObject); virtual;
    procedure RealSetText(const Value: TCaption); virtual;
    procedure Resize; override;
    class function GetControlClassDefaultSize: TSize; virtual;
    procedure SetColor(Value: TColor); virtual;
    function GetColor : TColor;
    procedure CMMouseEnter(var Message :TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message :TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    function ColorIsStored: boolean; virtual;
    function RealGetText: TCaption; virtual;
    function CreateControlBorderSpacing: TControlBorderSpacing; virtual;
    procedure SetBorderSpacing(const AValue: TControlBorderSpacing);
    procedure DoBorderSpacingChange(Sender: TObject; InnerSpaceChanged: Boolean); virtual;
    function  GetInstance : TObject;
    function IsInnerBorderStored: boolean;
  public
    constructor Create(TheOwner: TComponent);override;
    destructor Destroy;override;
    procedure SetInitialBounds(aLeft, aTop, aWidth, aHeight: integer); virtual;
    procedure InvalidatePreferredSize; virtual;
    procedure EnableAutoSizing;
    property Color: TColor read GetColor write SetColor stored ColorIsStored default clWindow;
    property BorderSpacing: TControlBorderSpacing read FBorderSpacing write SetBorderSpacing;
    property Caption;
    property Canvas;
{$ENDIF}
  end;

{$IFDEF FPC}
  TBGRACustomCtrl = class(TCustomControl)
{$ELSE}
  TBGRACustomCtrl = class(TCustomControl, ILCLControl)
  protected
    FBorderSpacing: TControlBorderSpacing;
    FChildSizing: TControlChildSizing;
    FOnChange: TNotifyEvent;
    FMouseInClient: boolean;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;{%H-}WithThemeSpace: boolean); {$IFDEF FPC}override;{$ELSE}virtual;{$ENDIF}
    procedure DoOnResize; virtual;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure TextChanged; virtual;
    procedure FontChanged(Sender: TObject); virtual;
    function GetDefaultDockCaption: String; virtual;
    procedure RealSetText(const Value: TCaption); virtual;
    procedure EnabledChanged; virtual;
    procedure Resize; override;
    class function GetControlClassDefaultSize: TSize; virtual;
    procedure SetColor(Value: TColor); virtual;
    function  GetColor : TColor;
    procedure UTF8KeyPress(var UTF8Key: {$IFDEF FPC}TUTF8Char{$ELSE}String{$ENDIF}); virtual;
    procedure CMMouseEnter(var Message :TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message :TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    function ColorIsStored: boolean; virtual;
    function RealGetText: TCaption; virtual;
    function CreateControlBorderSpacing: TControlBorderSpacing; virtual;
    procedure SetBorderSpacing(const AValue: TControlBorderSpacing);
    procedure DoBorderSpacingChange(Sender: TObject; InnerSpaceChanged: Boolean); virtual;
    function  GetInstance : TObject;
    function IsInnerBorderStored: boolean;
    procedure SetChildSizing(const AValue: TControlChildSizing);
    procedure DoChildSizingChange(Sender: TObject); virtual;
  public
    constructor Create(TheOwner: TComponent);override;
    destructor Destroy;override;
    procedure SetInitialBounds(aLeft, aTop, aWidth, aHeight: integer); virtual;
    procedure InvalidatePreferredSize; virtual;
    procedure EnableAutoSizing;
    property Color: TColor read GetColor write SetColor stored ColorIsStored default clWindow;
    property BorderSpacing: TControlBorderSpacing read FBorderSpacing write SetBorderSpacing;
    property ChildSizing: TControlChildSizing read FChildSizing write SetChildSizing;
    property Caption;
    property Canvas;
{$ENDIF}
  end;


{$IFDEF FPC}
  TBGRACustomPanel = class(TCustomPanel)
{$ELSE}
  TBGRACustomPanel = class(TCustomPanel, ILCLControl)
  protected
    FBorderSpacing: TControlBorderSpacing;
    FChildSizing: TControlChildSizing;
    FOnChange: TNotifyEvent;
    FMouseInClient: boolean;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;{%H-}WithThemeSpace: boolean); {$IFDEF FPC}override;{$ELSE}virtual;{$ENDIF}
    procedure DoOnResize; virtual;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure TextChanged; virtual;
    procedure FontChanged(Sender: TObject); virtual;
    function GetDefaultDockCaption: String; virtual;
    procedure RealSetText(const Value: TCaption); virtual;
    procedure EnabledChanged; virtual;
    procedure Resize; override;
    class function GetControlClassDefaultSize: TSize; virtual;
    procedure SetColor(Value: TColor); virtual;
    function  GetColor : TColor;
    procedure UTF8KeyPress(var UTF8Key: {$IFDEF FPC}TUTF8Char{$ELSE}String{$ENDIF}); virtual;
    procedure CMMouseEnter(var Message :TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message :TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    function ColorIsStored: boolean; virtual;
    function RealGetText: TCaption; virtual;
    function CreateControlBorderSpacing: TControlBorderSpacing; virtual;
    procedure SetBorderSpacing(const AValue: TControlBorderSpacing);
    procedure DoBorderSpacingChange(Sender: TObject; InnerSpaceChanged: Boolean); virtual;
    function  GetInstance : TObject;
    function IsInnerBorderStored: boolean;
    procedure SetChildSizing(const AValue: TControlChildSizing);
    procedure DoChildSizingChange(Sender: TObject); virtual;
  public
    constructor Create(TheOwner: TComponent);override;
    destructor Destroy;override;
    procedure SetInitialBounds(aLeft, aTop, aWidth, aHeight: integer); virtual;
    procedure InvalidatePreferredSize; virtual;
    procedure EnableAutoSizing;
    property Color: TColor read GetColor write SetColor stored ColorIsStored default clWindow;
    property BorderSpacing: TControlBorderSpacing read FBorderSpacing write SetBorderSpacing;
    property ChildSizing: TControlChildSizing read FChildSizing write SetChildSizing;
    property Canvas;
{$ENDIF}
  end;

  TOnBCPropertyChange = procedure(ASender: TObject; AData: PtrInt) of object;

  { TBCProperty
    Base BC Property with OnChange event support
  }

  TBCProperty = class(TPersistent)
  private
    FOnChange: TOnBCPropertyChange;
  protected
    FControl: TControl;
    procedure Change(AData: PtrInt = 0); virtual;
  public
    constructor Create(AControl: TControl); virtual;
  public
    property Control: TControl read FControl;
    property OnChange: TOnBCPropertyChange read FOnChange write FOnChange;
  end;

  { TBGRABitmapEx
    Some BGRABitmap descendant which can store custom data and has NeedRender flag
  }

  TBGRABitmapEx = class(TBGRABitmap)
  private
    FCustomData: PtrInt;
    FNeedRender: Boolean;
  protected
    procedure Init; override;
  public
    property NeedRender: Boolean read FNeedRender write FNeedRender;
    property CustomData: PtrInt read FCustomData write FCustomData;
    procedure Discard;
  end;

  { TBCGraphicControl
    BC graphic control with some basic functionality like begin/end update and
    debug functions
  }

  TBCGraphicControl = class(TBGRAGraphicCtrl)
  private
    {$IFDEF INDEBUG}
    FPaintCount: Integer;
    {$ENDIF}
    FUpdateCount: Integer;
  protected
    procedure DoOnResize; override;
  protected
    {$IFDEF INDEBUG}
    function GetDebugText: String; virtual;
    {$ENDIF}
    procedure Paint; override; // do not override in descendants!
    // All descendants should use DrawControl method instead of Paint.
    // DrawControl is not called between BeginUpdate and EndUpdate
    procedure DrawControl; virtual;
    // This method is called when control should be rendered (when some
    // general action occur which change "body" e.g. resize)
    procedure RenderControl; virtual;
  public
    {$IFDEF FPC}
    { Save all published settings to file }
    procedure SaveToFile(AFileName: string); virtual; abstract;
    { Load and assign all published settings from file }
    procedure LoadFromFile(AFileName: string); virtual; abstract;
    { Assign the properties from AFileName to this instance }
    procedure AssignFromFile(AFileName: string); virtual; abstract;
    { Used by SaveToFile/LoadFromFile }
    {$ENDIF}
    constructor Create(AOwner: TComponent); override;
    // This disable DrawControl method
    procedure BeginUpdate; virtual;
    // This enable DrawControl method
    procedure EndUpdate; virtual;
    // Called on EndUpdate if FUpdateCount is 0
    procedure UpdateControl; virtual;
    // Check if BeginUpdate was called
    function IsUpdating: Boolean;
  end;

  { TBCStyleDummyProperty
    This is only dummy property type for access to style editor from
    object inspector
  }

  TBCStyleDummyProperty = class(TBCProperty)

  end;

  { TBCStyleGraphicControl
    All descendants of this class have support for saving and loading styles and
    access to style editor from object inspector or component context menu
  }

  TBCStyleGraphicControl = class(TBCGraphicControl)
  private
    FAssignStyle: TBCStyleDummyProperty;
  protected
    function GetStyleExtension: String; virtual; abstract;
    // Dummy property for access to style editor dialog
    property AssignStyle: TBCStyleDummyProperty read FAssignStyle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property StyleExtension: String read GetStyleExtension;
  end;

  { TBCCustomControl
    BC custom control with some basic functionality like begin/end update and
    debug functions
  }

  TBCCustomControl = class(TBGRACustomCtrl)
  private
    {$IFDEF INDEBUG}
    FPaintCount: Integer;
    {$ENDIF}
    FUpdateCount: Integer;
  protected
    procedure DoOnResize; override;
  protected
    {$IFDEF INDEBUG}
    function GetDebugText: String; virtual;
    {$ENDIF}
    procedure Paint; override; // do not override in descendants!
    // All descendants should use DrawControl method instead of Paint.
    // DrawControl is not called between BeginUpdate and EndUpdate
    procedure DrawControl; virtual;
    // This method is called when control should be rendered (when some
    // general action occur which change "body" e.g. resize)
    procedure RenderControl; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    // This disable DrawControl method
    procedure BeginUpdate; virtual;
    // This enable DrawControl method
    procedure EndUpdate; virtual;
    // Called on EndUpdate if FUpdateCount is 0
    procedure UpdateControl; virtual;
    // Check if BeginUpdate was called
    function IsUpdating: Boolean;
  end;

  { TBCStyleCustomControl
    All descendants of this class have support for saving and loading styles and
    access to style editor from object inspector or component context menu
  }

  TBCStyleCustomControl = class(TBCCustomControl)
  private
    FAssignStyle: TBCStyleDummyProperty;
  protected
    function GetStyleExtension: String; virtual; abstract;
    // Dummy property for access to style editor dialog
    property AssignStyle: TBCStyleDummyProperty read FAssignStyle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property StyleExtension: String read GetStyleExtension;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Equivalent of standard lazarus TPanel but using BGRA Controls framework for render

  Functionality:
  - Customizable background (gradient etc.)
  - Customizable border (frame 3D or normal border, rounding etc)
  - FontEx (shadow etc.)

  originally written in 2011 by Krzysztof Dibowski dibowski at interia.pl
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCPanel;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Types, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BCBaseCtrls, BGRABitmapTypes, BCTypes, LCLVersion;

type
  TOnAfterRenderBCPanel = procedure(Sender: TObject; const ABGRA: TBGRABitmap;
    ARect: TRect) of object;
  TBCPanelBorderStyle = (bpsBorder, bpsFrame3d);

  { TCustomBCPanel }

  TCustomBCPanel = class(TBCStyleCustomControl)
  private
    { Private declarations }
    {$IFDEF INDEBUG}
    FRenderCount: Integer;
    {$ENDIF}
    FBackground: TBCBackground;
    FBevelWidth: Integer;
    FBGRA: TBGRABitmapEx;
    FBevelInner, FBevelOuter : TBevelCut;
    FBorder: TBCBorder;
    FBorderBCStyle: TBCPanelBorderStyle;
    FFontEx: TBCFont;
    FOnAfterRenderBCPanel: TOnAfterRenderBCPanel;
    FRounding: TBCRounding;
    procedure SetBackground(AValue: TBCBackground);
    procedure SetBevelInner(AValue: TBevelCut);
    procedure SetBevelOuter(AValue: TBevelCut);
    procedure SetBevelWidth(AValue: Integer);
    procedure SetBorder(AValue: TBCBorder);
    procedure SetBorderBCStyle(AValue: TBCPanelBorderStyle);
    procedure SetFontEx(AValue: TBCFont);
    procedure SetRounding(AValue: TBCRounding);
    procedure Render;
    procedure OnChangeProperty({%H-}Sender: TObject; {%H-}AData: BGRAPtrInt);
    procedure OnChangeFont({%H-}Sender: TObject; {%H-}AData: BGRAPtrInt);
  protected
    { Protected declarations }
    procedure AdjustClientRect(var aRect: TRect); override;
    class function GetControlClassDefaultSize: TSize; override;
    function GetDefaultDockCaption: String; override;
    procedure SetEnabled(Value: boolean); override;
    procedure TextChanged; override;
  protected
    function GetStyleExtension: String; override;
    {$IFDEF INDEBUG}
    function GetDebugText: String; override;
    {$ENDIF}
    procedure DrawControl; override;
    procedure RenderControl; override;
  protected
    {$IF LCL_FULLVERSION >= 2080000}
    procedure SetParentBackground(const AParentBackground: Boolean); override;
    {$ENDIF}
    property Background: TBCBackground read FBackground write SetBackground;
    property BevelInner: TBevelCut read FBevelInner write SetBevelInner;
    property BevelOuter: TBevelCut read FBevelOuter write SetBevelOuter;
    property BevelWidth: Integer read FBevelWidth write SetBevelWidth;
    property Border: TBCBorder read FBorder write SetBorder;
    property BorderBCStyle: TBCPanelBorderStyle
      read FBorderBCStyle write SetBorderBCStyle default bpsFrame3d;
    property FontEx: TBCFont read FFontEx write SetFontEx;
    property Rounding: TBCRounding read FRounding write SetRounding;
  protected
    { Events }
    property OnAfterRenderBCPanel: TOnAfterRenderBCPanel
      Read FOnAfterRenderBCPanel Write FOnAfterRenderBCPanel;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateControl; override; // Called by EndUpdate
  public
    { Streaming }
    {$IFDEF FPC}
    procedure SaveToFile(AFileName: string);
    procedure LoadFromFile(AFileName: string);
    {$ENDIF}
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
  end;

  { TBCPanel }

  TBCPanel = class(TCustomBCPanel)
  published
    property Align;
    property Anchors;
    property AssignStyle;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    {$IFDEF FPC} //#
    property OnGetDockCaption;
    {$ENDIF}
    property Background;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property Border;
    property BorderBCStyle;
    property Caption;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FontEx;
    property ParentBackground;
    property PopupMenu;
    property Rounding;
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
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnAfterRenderBCPanel;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

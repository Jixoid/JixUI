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
unit BGRAVirtualScreen;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LMessages, LResources, LCLIntf,{$ENDIF}  Types, Forms, BCBaseCtrls, Controls, Graphics, Dialogs,
  {$IFNDEF FPC}Windows, Messages, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  ExtCtrls, BGRABitmap, BCTypes;

type
  { TCustomBGRAVirtualScreen }

  TCustomBGRAVirtualScreen = class(TBGRACustomPanel)
  private
    { Private declarations }
    FBGRA:        TBGRABitmap;
    FOnRedraw:    TBGRARedrawEvent;
    FDiscardedRect: TRect;
    FBevelInner, FBevelOuter: TPanelBevel;
    FBevelWidth:  TBevelWidth;
    FBorderWidth: TBorderWidth;
    FAlignment:   TAlignment;
    FBitmapAutoScale: boolean;
    function GetBitmapHeight: integer;
    function GetBitmapScale: double;
    function GetBitmapWidth: integer;
    function GetVSCaption: string;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetBevelInner(const AValue: TPanelBevel);
    procedure SetBevelOuter(const AValue: TPanelBevel);
    procedure SetBevelWidth(const AValue: TBevelWidth);
    procedure SetBitmapAutoScale(AValue: boolean);
    procedure SetBorderWidth(const AValue: TBorderWidth);
    procedure SetVSCaption(AValue: string);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure Resize; override;
    procedure BGRASetSize(AWidth, AHeight: integer);
    procedure RedrawBitmapContent; virtual;
    procedure SetColor(Value: TColor); {$IFDEF FPC}override;{$ENDIF}
    procedure WMEraseBkgnd(var Message: {$IFDEF FPC}TLMEraseBkgnd{$ELSE}TWMEraseBkgnd{$ENDIF}); message {$IFDEF FPC}LM_ERASEBKGND{$ELSE}WM_ERASEBKGND{$ENDIF};
    procedure SetEnabled(Value: boolean); override;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    procedure RedrawBitmap; overload;
    procedure RedrawBitmap(ARect: TRect); overload;
    procedure RedrawBitmap(ARectArray: array of TRect); overload;
    procedure DiscardBitmap; overload;
    procedure DiscardBitmap(ARect: TRect); overload;
    destructor Destroy; override;
  public
    property OnRedraw: TBGRARedrawEvent Read FOnRedraw Write FOnRedraw;
    property Bitmap: TBGRABitmap Read FBGRA;
    property BitmapAutoScale: boolean read FBitmapAutoScale write SetBitmapAutoScale default true;
    property BitmapScale: double read GetBitmapScale;
    property BitmapWidth: integer read GetBitmapWidth;
    property BitmapHeight: integer read GetBitmapHeight;
    property BorderWidth: TBorderWidth Read FBorderWidth Write SetBorderWidth default 0;
    property BevelInner: TPanelBevel Read FBevelInner Write SetBevelInner default bvNone;
    property BevelOuter: TPanelBevel Read FBevelOuter Write SetBevelOuter default bvNone;
    property BevelWidth: TBevelWidth Read FBevelWidth Write SetBevelWidth default 1;
    property Alignment: TAlignment Read FAlignment Write SetAlignment;
    property Caption: string read GetVSCaption write SetVSCaption;
  end;

  TBGRAVirtualScreen = class(TCustomBGRAVirtualScreen)
  published
    property OnRedraw;
    property Bitmap;
    property BitmapAutoScale;
    // TPanel
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    {$IFDEF FPC} //#
    property OnGetDockCaption;
    {$ENDIF}
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
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
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

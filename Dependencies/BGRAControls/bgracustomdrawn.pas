// SPDX-License-Identifier: LGPL-3.0-linking-exception
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BGRACustomDrawn;

{$I bgracontrols.map.inc}

interface

uses
  Classes, Types, FPCanvas, Graphics, Controls, Math, LazUTF8, Forms, ExtCtrls,
  {$IFNDEF FPC}BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCThemeManager,
  { CustomDrawn }
  CustomDrawnControls, CustomDrawnDrawers, CustomDrawn_Common,
  { BGRABitmap }
  BGRABitmap, BGRABitmapTypes, BGRAGradients;

type

  { TBCDButton }

  TBCDButton = class(TCDButton)
  private
    FBCThemeManager: TBCThemeManager;
    procedure SetFBCThemeManager(AValue: TBCThemeManager);
  published
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFBCThemeManager;
  end;

  { TBCDEdit }

  TBCDEdit = class(TCDEdit)
  private
    FBCThemeManager: TBCThemeManager;
    procedure SetFBCThemeManager(AValue: TBCThemeManager);
  published
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFBCThemeManager;
  end;

  { TBCDStaticText }

  TBCDStaticText = class(TCDStaticText)
  private
    FBCThemeManager: TBCThemeManager;
    procedure SetFBCThemeManager(AValue: TBCThemeManager);
  published
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFBCThemeManager;
  end;

  { TBCDProgressBar }

  TBCDProgressBar = class(TCDProgressBar)
  private
    FBCThemeManager: TBCThemeManager;
    procedure SetFBCThemeManager(AValue: TBCThemeManager);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFBCThemeManager;
  end;

  { TBCDSpinEdit }

  TBCDSpinEdit = class(TCDSpinEdit)
  private
    FBCThemeManager: TBCThemeManager;
    procedure SetFBCThemeManager(AValue: TBCThemeManager);
  published
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFBCThemeManager;
  end;

  { TBCDCheckBox }

  TBCDCheckBox = class(TCDCheckBox)
  private
    FBCThemeManager: TBCThemeManager;
    procedure SetFBCThemeManager(AValue: TBCThemeManager);
  published
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFBCThemeManager;
  end;

  { TBCDRadioButton }

  TBCDRadioButton = class(TCDRadioButton)
  private
    FBCThemeManager: TBCThemeManager;
    procedure SetFBCThemeManager(AValue: TBCThemeManager);
  published
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFBCThemeManager;
  end;

  { TBCDPanel }

  TBCDPanel = class(TPanel)
  private
    FBCThemeManager: TBCThemeManager;
    FDarkTheme: boolean;
    procedure SetFBCThemeManager(AValue: TBCThemeManager);
    procedure SetFDarkTheme(AValue: boolean);
  protected
    procedure Paint; override;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property DarkTheme: boolean read FDarkTheme write SetFDarkTheme default True;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property ChildSizing;
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
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  published
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFBCThemeManager;
  end;

  { TBGRADrawer }

  TBGRADrawer = class(TCDDrawerCommon)
  private
    FRandSeed: integer;
  protected
    procedure AssignFont(Bitmap: TBGRABitmap; Font: TFont);
  public
    constructor Create; override;
    { General }
    function GetMeasures(AMeasureID: integer): integer; override;
    procedure DrawTickmark(ADest: TFPCustomCanvas; ADestPos: TPoint;
      AState: TCDControlState); override;
    function DPIAdjustment(const AValue: integer): integer;
    { Button }
    procedure DrawButton(ADest: TFPCustomCanvas; ADestPos: TPoint;
      ASize: TSize; AState: TCDControlState; AStateEx: TCDButtonStateEx); override;
    { Edit }
    procedure DrawEditBackground(ADest: TCanvas; ADestPos: TPoint;
      ASize: TSize; AState: TCDControlState; {%H-}AStateEx: TCDEditStateEx); override;
    procedure DrawEditFrame(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; {%H-}AStateEx: TCDEditStateEx); override;
    procedure DrawCaret(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); override;
    procedure DrawEdit(ADest: TCanvas; ASize: TSize; AState: TCDControlState;
      AStateEx: TCDEditStateEx); override;
    { Panel }
    procedure DrawPanel(ADest: TCanvas; ASize: TSize; {%H-}AState: TCDControlState;
      {%H-}AStateEx: TCDPanelStateEx); override;
    { Static Text }
    procedure DrawStaticText(ADest: TCanvas; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    { Progress Bar }
    procedure DrawProgressBar(ADest: TCanvas; ASize: TSize;
      {%H-}AState: TCDControlState; AStateEx: TCDProgressBarStateEx); override;
    { CheckBox }
    procedure DrawCheckBoxSquare(ADest: TCanvas; ADestPos: TPoint;
      ASize: TSize; AState: TCDControlState; {%H-}AStateEx: TCDControlStateEx); override;
    procedure DrawCheckBox(ADest: TCanvas; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    { RadioButton }
    procedure DrawRadioButtonCircle(ADest: TCanvas; {%H-}ADestPos: TPoint;
      {%H-}ASize: TSize; AState: TCDControlState; {%H-}AStateEx: TCDControlStateEx); override;
    procedure DrawRadioButton(ADest: TCanvas; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Customizable component which using BGRABitmap for drawing. Control mostly rendered
  using framework.

  Functionality:
  - Gradients
  - Double gradients
  - Rounding
  - Drop down list
  - Glyph
  - States (normal, hover, clicked)
  - Caption with shadow
  - Full alpha and antialias support

  originally written in 2012 by Krzysztof Dibowski dibowski at interia.pl
}

{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}

unit BCButton;

{$I bgracontrols.map.inc}

interface

uses
  Classes, types, {$IFDEF FPC}LCLType, LResources, {$ENDIF} Controls, Dialogs,
  ActnList, ImgList, Menus, // MORA
  Buttons, Graphics,
  {$IFNDEF FPC}BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BGRABitmap, BGRABitmapTypes, BCThemeManager, BCTypes, Forms, BCBasectrls,
  fpjsonrtti, Typinfo, fpjson;

{off $DEFINE DEBUG}

type
  TBCButtonMemoryUsage = (bmuLow, bmuMedium, bmuHigh);
  TBCButtonState = class;
  TBCButtonStyle = (bbtButton, bbtDropDown);
  TOnAfterRenderBCButton = procedure(Sender: TObject; const ABGRA: TBGRABitmap;
    AState: TBCButtonState; ARect: TRect) of object;
  TBCButtonPropertyData = (pdNone, pdUpdateSize);

  // MORA: DropDown styles
  TBCButtonDropDownStyle = (
    bdsSeparate,     // DropDown is a separate button (default)
    bdsCommon        // DropDown is same as main button
    );
  TBCButtonDropDownPosition = (
    bdpLeft,         // default
    bdpBottom);

  { TBCButtonState }

  TBCButtonState = class(TBCProperty)
  private
    FBackground: TBCBackground;
    FBorder: TBCBorder;
    FFontEx: TBCFont;
    procedure OnChangeFont({%H-}Sender: TObject; {%H-}AData: PtrInt);
    procedure OnChangeChildProperty({%H-}Sender: TObject; AData: PtrInt);
    procedure SetBackground(AValue: TBCBackground);
    procedure SetBorder(AValue: TBCBorder);
    procedure SetFontEx(const AValue: TBCFont);
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Scale(AScale: single; APreserveDefaultFontHeight: boolean = true);
  published
    property Background: TBCBackground read FBackground write SetBackground;
    property Border: TBCBorder read FBorder write SetBorder;
    property FontEx: TBCFont read FFontEx write SetFontEx;
  end;

  { TCustomBCButton }

  TCustomBCButton = class(TBCStyleGraphicControl)
  private
    { Private declarations }
    {$IFDEF INDEBUG}
    FRenderCount: integer;
    {$ENDIF}
    FDropDownArrowSize: integer;
    FDropDownWidth: integer;
    FFlipArrow: boolean;
    FActiveButt: TBCButtonStyle;
    FBGRANormal, FBGRAHover, FBGRAClick: TBGRABitmapEx;
    FCanvasScale: Single;
    FCanvasScaleMode: TBCCanvasScaleMode;
    FGlyphAlignment: TBCAlignment;
    FGlyphOldPlacement: boolean;
    FGlyphScale: single;
    FInnerMargin: single;
    FMemoryUsage: TBCButtonMemoryUsage;
    FPreserveGlyphOnAssign: boolean;
    FRounding: TBCRounding;
    FRoundingDropDown: TBCRounding;
    FStateClicked: TBCButtonState;
    FStateHover: TBCButtonState;
    FStateNormal: TBCButtonState;
    FDown: boolean;
    FGlyph: TBitmap;
    FGlyphMargin: integer;
    FButtonState: TBCMouseState;
    FDownButtonState: TBCMouseState;
    FOnAfterRenderBCButton: TOnAfterRenderBCButton;
    FOnButtonClick: TNotifyEvent;
    FStaticButton: boolean;
    FStyle: TBCButtonStyle;
    FGlobalOpacity: byte;
    FTextApplyGlobalOpacity: boolean;
    AutoSizeExtraY: integer;
    AutoSizeExtraX: integer;
    FLastBorderWidth: integer;
    // MORA
    FClickOffset: boolean;
    FDropDownArrow: boolean;
    FDropDownMenu: TPopupMenu;
    FDropDownMenuVisible: boolean;
    FDropDownClosingTime: TDateTime;
    FDropDownPosition: TBCButtonDropDownPosition;
    FDropDownStyle: TBCButtonDropDownStyle;
    FImageChangeLink: TChangeLink;
    FImageIndex: integer;
    FImages: TCustomImageList;
    FSaveDropDownClosed: TNotifyEvent;
    FShowCaption: boolean;
    procedure AssignDefaultStyle;
    procedure CalculateGlyphSize(out NeededWidth, NeededHeight: integer);
    procedure DropDownClosed(Sender: TObject);
    function GetBGRAClick: TBGRABitmapEx;
    function GetBGRAHover: TBGRABitmapEx;
    function GetBGRANormal: TBGRABitmapEx;
    procedure OnRestoreProperty(Sender: TObject; AObject: TObject;
      Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
    procedure OnStreamProperty(Sender: TObject; AObject: TObject;
      Info: PPropInfo; var Res: TJSONData);
    procedure RenderAll(ANow: boolean = False);
    function GetButtonRect: TRect;
    function GetDropDownWidth(AFull: boolean = True): integer;
    function GetDropDownRect(AFull: boolean = True): TRect;
    procedure SetBCButtonStateClicked(const AValue: TBCButtonState);
    procedure SetBCButtonStateHover(const AValue: TBCButtonState);
    procedure SetBCButtonStateNormal(const AValue: TBCButtonState);
    procedure SetCanvasScaleMode(AValue: TBCCanvasScaleMode);
    procedure SetClickOffset(AValue: boolean);
    procedure SetDown(AValue: boolean);
    procedure SetDropDownArrow(AValue: boolean);
    procedure SetDropDownArrowSize(AValue: integer);
    procedure SetDropDownPosition(AValue: TBCButtonDropDownPosition);
    procedure SetDropDownWidth(AValue: integer);
    procedure SetFlipArrow(AValue: boolean);
    procedure SetGlyph(const AValue: TBitmap);
    procedure SetGlyphAlignment(AValue: TBCAlignment);
    procedure SetGlyphMargin(const AValue: integer);
    procedure SetGlyphOldPlacement(AValue: boolean);
    procedure SetGlyphScale(AValue: single);
    procedure SetImageIndex(AValue: integer);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetInnerMargin(AValue: single);
    procedure SetMemoryUsage(AValue: TBCButtonMemoryUsage);
    procedure SetRounding(AValue: TBCRounding);
    procedure SetRoundingDropDown(AValue: TBCRounding);
    procedure SetShowCaption(AValue: boolean);
    procedure SetStaticButton(const AValue: boolean);
    procedure SetStyle(const AValue: TBCButtonStyle);
    procedure SetGlobalOpacity(const AValue: byte);
    procedure SetTextApplyGlobalOpacity(const AValue: boolean);
    procedure UpdateSize;
    procedure OnChangeGlyph({%H-}Sender: TObject);
    procedure OnChangeState({%H-}Sender: TObject; AData: PtrInt);
    procedure ImageListChange(ASender: TObject);
    function  GetGlyph: TBitmap;
  protected
    { Protected declarations }
    procedure LimitMemoryUsage;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      {%H-}WithThemeSpace: boolean); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter;  override;
    procedure MouseLeave;  override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure SetEnabled(Value: boolean); override;
    procedure TextChanged;  override;
  protected
    // MORA
    procedure ActionChange(Sender: TObject; CheckDefaults: boolean); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure Render(ABGRA: TBGRABitmapEx; AState: TBCButtonState); virtual;
    procedure RenderState(ABGRA: TBGRABitmapEx; AState: TBCButtonState;
      const ARect: TRect; ARounding: TBCRounding); virtual;
    property ClickOffset: boolean read FClickOffset write SetClickOffset default False;
    property DropDownArrow: boolean
      read FDropDownArrow write SetDropDownArrow default False;
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;
    property DropDownStyle: TBCButtonDropDownStyle
      read FDropDownStyle write FDropDownStyle default bdsSeparate;
    property DropDownPosition: TBCButtonDropDownPosition
      read FDropDownPosition write SetDropDownPosition default bdpLeft;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property ShowCaption: boolean read FShowCaption write SetShowCaption default True;
  protected
    {$IFDEF INDEBUG}
    function GetDebugText: string; override;
    {$ENDIF}
    function GetStyleExtension: string; override;
    procedure DrawControl; override;
    procedure RenderControl; override;
    property BGRANormal: TBGRABitmapEx read GetBGRANormal;
    property BGRAHover: TBGRABitmapEx read GetBGRAHover;
    property BGRAClick: TBGRABitmapEx read GetBGRAClick;
  protected
    property AutoSizeExtraVertical: integer read AutoSizeExtraY;
    property AutoSizeExtraHorizontal: integer read AutoSizeExtraX;
    property CanvasScaleMode: TBCCanvasScaleMode read FCanvasScaleMode write SetCanvasScaleMode default csmAuto;
    property StateNormal: TBCButtonState read FStateNormal write SetBCButtonStateNormal;
    property StateHover: TBCButtonState read FStateHover write SetBCButtonStateHover;
    property StateClicked: TBCButtonState read FStateClicked
      write SetBCButtonStateClicked;
    property Down: boolean read FDown write SetDown default False;
    property DropDownWidth: integer read FDropDownWidth write SetDropDownWidth;
    property DropDownArrowSize: integer read FDropDownArrowSize
      write SetDropDownArrowSize;
    property FlipArrow: boolean read FFlipArrow write SetFlipArrow default False;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property GlyphScale: single read FGlyphScale write SetGlyphScale default 1;
    property GlyphMargin: integer read FGlyphMargin write SetGlyphMargin default 5;
    property GlyphAlignment: TBCAlignment read FGlyphAlignment write SetGlyphAlignment default bcaCenter;
    property GlyphOldPlacement: boolean read FGlyphOldPlacement write SetGlyphOldPlacement default true;
    property Style: TBCButtonStyle read FStyle write SetStyle default bbtButton;
    property StaticButton: boolean
      read FStaticButton write SetStaticButton default False;
    property GlobalOpacity: byte read FGlobalOpacity write SetGlobalOpacity;
    property Rounding: TBCRounding read FRounding write SetRounding;
    property RoundingDropDown: TBCRounding read FRoundingDropDown
      write SetRoundingDropDown;
    property TextApplyGlobalOpacity: boolean
      read FTextApplyGlobalOpacity write SetTextApplyGlobalOpacity;
    property OnAfterRenderBCButton: TOnAfterRenderBCButton
      read FOnAfterRenderBCButton write FOnAfterRenderBCButton;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property MemoryUsage: TBCButtonMemoryUsage read FMemoryUsage write SetMemoryUsage;
    property InnerMargin: single read FInnerMargin write SetInnerMargin;
    property PreserveGlyphOnAssign: boolean read FPreserveGlyphOnAssign write FPreserveGlyphOnAssign default True;
  public
    { Constructor }
    constructor Create(AOwner: TComponent); override;
    { Destructor }
    destructor Destroy; override;
    { Assign the properties from Source to this instance }
    procedure Assign(Source: TPersistent); override;
    { Set dropdown size and autosize extra padding }
    procedure SetSizeVariables(newDropDownWidth, newDropDownArrowSize,
      newAutoSizeExtraVertical, newAutoSizeExtraHorizontal: integer);
    { Called by EndUpdate }
    procedure UpdateControl; override;
    property CanvasScale: single read FCanvasScale;
  public
    procedure ScaleStyle(AScale: single; APreserveDefaultFontHeight: boolean = true);
    {$IFDEF FPC}
    { Save all published settings to file }
    procedure SaveToFile(AFileName: string); override;
    procedure SaveToJSONFile(AFileName: string);
    function SaveToJSON: string;
    { Load and assign all published settings from file }
    procedure LoadFromFile(AFileName: string); override;
    procedure LoadFromJSONFile(AFileName: string);
    procedure LoadFromJSON(AJSON: string);
    { Assign the properties from AFileName to this instance }
    procedure AssignFromFile(AFileName: string); override;
    procedure AssignFromResource(AResourceName: string);
    {$ENDIF}
    { Used by SaveToFile/LoadFromFile }
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
  end;

  TBCButton = class(TCustomBCButton)
  private
    FBCThemeManager: TBCThemeManager;
    procedure SetFBCThemeManager(AValue: TBCThemeManager);
  published
    property Action;
    property Align;
    property Anchors;
    { Click to edit the style. Available when editing only. If you want to stream the style from a file at runtime please use LoadFromFile and SaveToFile methods. }
    property AssignStyle;
    property AutoSize;
    { The style of the button when pressed. }
    property StateClicked;
    { The style of the button when hovered. }
    property StateHover;
    { The default style of the button. }
    property StateNormal;
    property BorderSpacing;
    property CanvasScaleMode;
    property Caption;
    property Color;
    property Constraints;
    { Set to True to change the button to always show a StateClicked style that will not change when button is clicked or hovered. }
    property Down;
    { The width of the dropdown arrow area. }
    property DropDownWidth;
    { The size of the dropdown arrow. }
    property DropDownArrowSize;
    property Enabled;
    { Changes the direction of the arrow. Default: False. }
    property FlipArrow;
    { Set the opacity that will be applied to the whole button. Default: 255. }
    property GlobalOpacity;
    { The glyph icon. }
    property Glyph;
    property GlyphScale;
    property GlyphAlignment;
    property GlyphOldPlacement;
    property PreserveGlyphOnAssign;
    { The margin of the glyph icon. }
    property GlyphMargin;
    property Hint;
    property InnerMargin;
    { Called when the button finish the render. Use it to add your own drawings to the button. }
    property OnAfterRenderBCButton;
    { Called when the button part is clicked, not the dropdown. }
    property OnButtonClick;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property ParentColor;
    property PopupMenu;
    { Change the style of the rounded corners of the button. }
    property Rounding;
    { Change the style of the rounded corners of the dropdown part of the button. }
    property RoundingDropDown;
    { Set to True to change the button to always show a StateNormal style that will not change when button is clicked or hovered. }
    property StaticButton;
    property ShowHint;
    { The style of button that will be used. bbtButton or bbtDropDown. }
    property Style;
    { Apply the global opacity to rendered text. Default: False. }
    property TextApplyGlobalOpacity;
    property Visible;
    { -ToDo: Unused property? }
    property ClickOffset;
    { Show the dropdown arrow. }
    property DropDownArrow;
    { The dropdown menu that will be displayed when the button is pressed. }
    property DropDownMenu;
    { The kind of dropdown that will be used. bdsSeparate will show the dropdown down the dropdown arrow side. bdsCommon will show the dropdown down the whole button. }
    property DropDownStyle;
    { The position of the dropdown arrow. }
    property DropDownPosition;
    { The image list that holds an image to be used with the button ImageIndex property. }
    property Images;
    { The index of the image that will be used for the button as glyph icon if glyph property is not set. }
    property ImageIndex;
    { Show caption or hides it. Default: True. }
    property ShowCaption;
    { Limit memory usage by selecting one of the options. Default: bmuHigh. }
    property MemoryUsage;
    { The unique name of the control in the form. }
    property Name;
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFBCThemeManager;
  end;

  { TBCButtonActionLink }

  TBCButtonActionLink = class(TControlActionLink)
  protected
    procedure AssignClient(AClient: TObject); override;
    procedure SetChecked(Value: boolean); override;
    procedure SetImageIndex(Value: integer); override;
  public
    function IsCheckedLinked: boolean; override;
    function IsImageIndexLinked: boolean; override;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

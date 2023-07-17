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
unit BCImageButton;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  {$IFDEF FPC}{$ifdef Windows}Windows,{$endif}LCLType, LResources, LMessages,{$ENDIF} ExtCtrls,
  Types,
  {$IFNDEF FPC}Windows, Messages, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  { BGRAControls }
  BCBaseCtrls, BCEffect,
  { BGRABitmap }
  BGRABitmap, BGRABitmapTypes, BGRASliceScaling;

{off $DEFINE DEBUG}

function CalculateAspectRatioH(W1, H1, W2: integer): integer; //result H2
function CalculateAspectRatioW(W1, H1, H2: integer): integer; //result W2
function CalculateDestRect(ImageW, ImageH, DestW, DestH: integer;
  Stretch, Proportional, Center: boolean): TRect;
procedure AssignFontToBGRA(Source: TFont; Dest: TBGRABitmap);

type
  TBCGraphicButtonState = (gbsNormal, gbsHover, gbsActive, gbsDisabled);

  TOnRenderControl = procedure(Sender: TObject; Bitmap: TBGRABitmap;
    State: TBCGraphicButtonState) of object;

type

  { TBCGraphicButton }

  TBCGraphicButton = class(TBCGraphicControl)
  protected
    FState: TBCGraphicButtonState;
    FModalResult: TModalResult;
  protected
    procedure DoClick; virtual;
    procedure DoMouseDown; virtual;
    procedure DoMouseUp; virtual;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure DoMouseMove({%H-}x, {%H-}y: integer); virtual;
  protected
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
  public
    property ModalResult: TModalResult
      read FModalResult write FModalResult default mrNone;
  end;

  { TBCXButton }
  TBCXButton = class(TBCGraphicButton)
  protected
    FOnRenderControl: TOnRenderControl;
    FBGRANormal, FBGRAHover, FBGRAActive, FBGRADisabled: TBGRABitmap;
  protected
    class function GetControlClassDefaultSize: TSize; override;
    procedure DrawControl; override;
    procedure RenderControl; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnRenderControl: TOnRenderControl
      read FOnRenderControl write FOnRenderControl;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBidiMode;
    property ModalResult;
    {$IFDEF FPC}
    property OnChangeBounds;
    {$ENDIF}
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
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
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;

  { TBCSliceScalingOptions }

  TBCCustomSliceScalingOptions = class(TPersistent)
  protected
    FOwner: TControl;
    FBitmap: TBGRABitmap;
    FAutoDetectRepeat, FRepeatTop, FRepeatLeft, FRepeatMiddleHorizontal,
    FRepeatMiddleVertical, FRepeatRight, FRepeatBottom: boolean;
    FMarginTop, FMarginRight, FMarginBottom, FMarginLeft, FNumberOfItems: integer;
    FDirection: TSliceScalingDirection;
    FDrawMode: TDrawMode;
    FResampleMode: TResampleMode;
    FResampleFilter: TResampleFilter;
  private
    procedure SetFBitmap(AValue: TBGRABitmap);
    procedure SetFMarginBottom(AValue: integer);
    procedure SetFMarginLeft(AValue: integer);
    procedure SetFMarginRight(AValue: integer);
    procedure SetFMarginTop(AValue: integer);
    procedure SetFAutoDetectRepeat(AValue: boolean);
    procedure SetFDirection(AValue: TSliceScalingDirection);
    procedure SetFDrawMode(AValue: TDrawMode);
    procedure SetFNumberOfItems(AValue: integer);
    procedure SetFRepeatBottom(AValue: boolean);
    procedure SetFRepeatLeft(AValue: boolean);
    procedure SetFRepeatMiddleHorizontal(AValue: boolean);
    procedure SetFRepeatMiddleVertical(AValue: boolean);
    procedure SetFRepeatRight(AValue: boolean);
    procedure SetFRepeatTop(AValue: boolean);
    procedure SetFResampleFilter(AValue: TResampleFilter);
    procedure SetFResampleMode(AValue: TResampleMode);
  public
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
  published
    property Bitmap: TBGRABitmap read FBitmap write SetFBitmap;
    property AutoDetectRepeat: boolean read FAutoDetectRepeat
      write SetFAutoDetectRepeat default False;
    property RepeatTop: boolean read FRepeatTop write SetFRepeatTop default False;
    property RepeatLeft: boolean read FRepeatLeft write SetFRepeatLeft default False;
    property RepeatMiddleHorizontal: boolean
      read FRepeatMiddleHorizontal write SetFRepeatMiddleHorizontal default False;
    property RepeatMiddleVertical: boolean read FRepeatMiddleVertical
      write SetFRepeatMiddleVertical default False;
    property RepeatRight: boolean read FRepeatRight write SetFRepeatRight default False;
    property RepeatBottom: boolean
      read FRepeatBottom write SetFRepeatBottom default False;
    property MarginTop: integer read FMarginTop write SetFMarginTop default 0;
    property MarginRight: integer read FMarginRight write SetFMarginRight default 0;
    property MarginBottom: integer read FMarginBottom write SetFMarginBottom default 0;
    property MarginLeft: integer read FMarginLeft write SetFMarginLeft default 0;
    property NumberOfItems: integer
      read FNumberOfItems write SetFNumberOfItems default 1;
    property Direction: TSliceScalingDirection read FDirection write SetFDirection;
    property DrawMode: TDrawMode read FDrawMode write SetFDrawMode default
      dmDrawWithTransparency;
    property ResampleMode: TResampleMode read FResampleMode
      write SetFResampleMode default rmFineResample;
    property ResampleFilter: TResampleFilter read FResampleFilter
      write SetFResampleFilter default rfBestQuality;
  end;

  { TBCImageButtonSliceScalingOptions }

  TBCImageButtonSliceScalingOptions = class(TBCCustomSliceScalingOptions)
  private
    procedure SetFCenter(AValue: boolean);
    procedure SetFProportional(AValue: boolean);
    procedure SetFStretch(AValue: boolean);
  protected
    FCenter, FStretch, FProportional: boolean;
  published
    property NumberOfItems: integer read FNumberOfItems default 4;
    property Center: boolean read FCenter write SetFCenter default True;
    property Stretch: boolean read FStretch write SetFStretch default True;
    property Proportional: boolean
      read FProportional write SetFProportional default False;
  public
    constructor Create(AOwner: TControl);
    procedure Assign(Source: TPersistent); override;
  end;

  { TBCCustomImageButton }

  TBCCustomImageButton = class(TBCGraphicButton)
  private
    { Private declarations }
    FAlphaTest: boolean;
    FAlphaTestValue: byte;
    {$IFDEF INDEBUG}
    FDrawCount: integer;
    FRenderCount: integer;
    {$ENDIF}
    FBitmapOptions: TBCImageButtonSliceScalingOptions;
    FBGRAMultiSliceScaling: TBGRAMultiSliceScaling;
    FBGRANormal, FBGRAHover, FBGRAActive, FBGRADisabled: TBGRABitmap;
    FDestRect: TRect;
    FPressed: boolean;
    FTimer: TTimer;
    FFade: TFading;
    FAnimation: boolean;
    FBitmapFile: string;
    FTextVisible: boolean;
    FToggle: boolean;
    FMouse: TPoint;
    procedure SetFAlphaTest(AValue: boolean);
    procedure SetFAlphaTestValue(AValue: byte);
    procedure SetFAnimation(AValue: boolean);
    procedure SetFBitmapFile(AValue: string);
    procedure SetFBitmapOptions(AValue: TBCImageButtonSliceScalingOptions);
    procedure Fade({%H-}Sender: TObject);
    procedure SetFPressed(AValue: boolean);
    procedure SetFTextVisible(AValue: boolean);
    procedure SetFToggle(AValue: boolean);
  protected
    { Protected declarations }
    procedure DrawControl; override;
    procedure RenderControl; override;
    procedure TextChanged; override;
    procedure FontChanged(Sender: TObject); override;
    procedure CMChanged(var {%H-}Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message CM_CHANGED; {$IFDEF FPC}virtual;{$ENDIF}
    {$IFDEF INDEBUG}
    {$IFDEF FPC}
    function GetDebugText: string;
    {$ENDIF}
    {$ENDIF}
    procedure DoMouseDown; override;
    procedure DoMouseUp; override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure DoMouseMove(x, y: integer); override;
    procedure Click; override;
  public
    { Public declarations }
    property AlphaTest: boolean read FAlphaTest write SetFAlphaTest default True;
    property AlphaTestValue: byte
      read FAlphaTestValue write SetFAlphaTestValue default 255;
    property Toggle: boolean read FToggle write SetFToggle default False;
    property Pressed: boolean read FPressed write SetFPressed default False;
    //property State: TBCGraphicButtonState read FState;
    property BitmapOptions: TBCImageButtonSliceScalingOptions
      read FBitmapOptions write SetFBitmapOptions;
    property Animation: boolean read FAnimation write SetFAnimation default True;
    property BitmapFile: string read FBitmapFile write SetFBitmapFile;
    property TextVisible: boolean read FTextVisible write SetFTextVisible default True;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { It loads the 'BitmapFile' }
    procedure LoadFromBitmapResource(const Resource: string; ResourceType: PChar); overload;
    procedure LoadFromBitmapResource(const Resource: string); overload;
    procedure LoadFromBitmapFile;
    procedure Assign(Source: TPersistent); override;
    { Streaming }
    {$IFDEF FPC}
    procedure SaveToFile(AFileName: string); override;
    procedure LoadFromFile(AFileName: string); override;
    procedure AssignFromFile(AFileName: string); override;
    {$ENDIF}
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
  published
    { Published declarations }
  end;

  TBCImageButton = class(TBCCustomImageButton)
  published
    property AlphaTest;
    property AlphaTestValue;
    property Action;
    property Align;
    property Anchors;
    property Animation;
    property AutoSize;
    //property AutoSizeExtraHorizontal;
    //property AutoSizeExtraVertical;
    property BidiMode;
    //property Bitmap;
    property BitmapFile;
    property BitmapOptions;
    property BorderSpacing;
    property Caption;
    //property Checked;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ModalResult;
    {$IFDEF FPC}
    property OnChangeBounds;
    {$ENDIF}
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    //property OnPlaySound;
    //property OnRedraw;
    property OnResize;
    property OnStartDrag;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    //property Shadow;
    property ShowHint;
    //property Sound;
    //property SoundClick;
    //property SoundEnter;
    property TextVisible;
    property Toggle;
    property Pressed;
    property Visible;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

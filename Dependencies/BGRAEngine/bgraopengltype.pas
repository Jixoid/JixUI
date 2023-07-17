// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAOpenGLType;

{$mode objfpc}{$H+}

interface

uses
  BGRAGraphics, BGRABitmap, BGRABitmapTypes,
  FPimage, BGRAClasses, SysUtils, BGRATransform,
  BGRASSE, BGRAMatrix3D;

type
  TBGLTextureHandle = type Pointer;
  TOpenGLResampleFilter = (orfBox,orfLinear);
  TOpenGLBlendMode = (obmNormal, obmAdd, obmMultiply);
  TWaitForGPUOption = (wfgQueueAllCommands, wfgFinishAllCommands);
  TFaceCulling = BGRABitmapTypes.TFaceCulling;
  TOpenGLPrimitive = (opPoints,opLineStrip,opLineLoop,opLines,
                  opTriangleStrip,opTriangleFan,opTriangles);

const
  fcNone = BGRABitmapTypes.fcNone;
  fcKeepCW = BGRABitmapTypes.fcKeepCW;
  fcKeepCCW = BGRABitmapTypes.fcKeepCCW;

type

  { IBGLFont }

  IBGLFont = interface
    function GetClipped: boolean;
    function GetPadding: TRectF;
    function GetUseGradientColors: boolean;
    function GetHorizontalAlign: TAlignment;
    function GetJustify: boolean;
    function GetScale: single;
    function GetStepX: single;
    function GetVerticalAlign: TTextLayout;
    procedure SetClipped(AValue: boolean);
    procedure SetPadding(AValue: TRectF);
    procedure SetUseGradientColors(AValue: boolean);
    procedure SetHorizontalAlign(AValue: TAlignment);
    procedure SetJustify(AValue: boolean);
    procedure SetScale(AValue: single);
    procedure SetStepX(AValue: single);
    procedure SetVerticalAlign(AValue: TTextLayout);
    procedure TextOut(X, Y: Single; const Text : UTF8String); overload;
    procedure TextOut(X, Y: Single; const Text : UTF8String; AColor: TBGRAPixel); overload;
    procedure TextOut(X, Y: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop); overload;
    procedure TextOut(X, Y: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AColor: TBGRAPixel); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AVertAlign: TTextLayout); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String; AColor: TBGRAPixel); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String; AVertAlign: TTextLayout); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    function TextWidth(const Text: UTF8String): single;
    function TextHeight(const Text: UTF8String): single; overload;
    function TextHeight(const Text: UTF8String; AWidth: single): single; overload;
    procedure SetGradientColors(ATopLeft, ATopRight, ABottomRight, ABottomLeft: TBGRAPixel);

    property Scale: single read GetScale write SetScale;
    property StepX: single read GetStepX write SetStepX;
    property Justify: boolean read GetJustify write SetJustify;
    property Clipped: boolean read GetClipped write SetClipped;
    property HorizontalAlign: TAlignment read GetHorizontalAlign write SetHorizontalAlign;
    property VerticalAlign: TTextLayout read GetVerticalAlign write SetVerticalAlign;
    property GradientColors: boolean read GetUseGradientColors write SetUseGradientColors;
    property Padding: TRectF read GetPadding write SetPadding;
  end;

  { TBGLCustomFont }

  TBGLCustomFont = class(TInterfacedObject, IBGLFont)
  protected
    FScale, FStepX: single;
    FPadding: TRectF;
    FFlags: LongWord;
    FHorizontalAlign: TAlignment;
    FVerticalAlign: TTextLayout;
    FJustify: boolean;
    procedure Init; virtual;
    function LoadFromFile(AFilename: UTF8String): boolean; virtual; abstract;
    procedure FreeMemoryOnDestroy; virtual;

    function GetScale: single; virtual;
    function GetStepX: single; virtual;
    procedure SetScale(AValue: single); virtual;
    procedure SetStepX(AValue: single); virtual;
    function GetPadding: TRectF;
    procedure SetPadding(AValue: TRectF); virtual;

    function GetHorizontalAlign: TAlignment; virtual;
    function GetJustify: boolean; virtual;
    function GetVerticalAlign: TTextLayout; virtual;
    procedure SetHorizontalAlign(AValue: TAlignment); virtual;
    procedure SetJustify(AValue: boolean); virtual;
    procedure SetVerticalAlign(AValue: TTextLayout); virtual;

    function GetClipped: boolean; virtual; abstract;
    function GetUseGradientColors: boolean; virtual; abstract;
    procedure SetClipped(AValue: boolean); virtual; abstract;
    procedure SetUseGradientColors(AValue: boolean); virtual; abstract;

    procedure DoTextOut(X, Y: Single; const Text : UTF8String; AColor: TBGRAPixel); virtual; abstract;
    procedure DoTextRect(X, Y, Width, Height: Single; const Text : UTF8String; AColor: TBGRAPixel); virtual; abstract;

    function GetDefaultColor: TBGRAPixel; virtual;
    procedure SwapRectIfNeeded(var ARect: TRectF); overload;
    procedure SwapRectIfNeeded(var ARect: TRect); overload;
  public
    constructor Create(AFilename: UTF8String);
    procedure FreeMemory; virtual;
    destructor Destroy; override;
    procedure TextOut(X, Y: Single; const Text : UTF8String); overload;
    procedure TextOut(X, Y: Single; const Text : UTF8String; AColor: TBGRAPixel); overload;
    procedure TextOut(X, Y: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop); overload;
    procedure TextOut(X, Y: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AColor: TBGRAPixel); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AVertAlign: TTextLayout); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(ARect: TRect; const Text : UTF8String); overload;
    procedure TextRect(ARect: TRect; const Text : UTF8String; AColor: TBGRAPixel); overload;
    procedure TextRect(ARect: TRect; const Text : UTF8String; AVertAlign: TTextLayout); overload;
    procedure TextRect(ARect: TRect; const Text : UTF8String; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(ARect: TRect; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop); overload;
    procedure TextRect(ARect: TRect; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String; AColor: TBGRAPixel); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String; AVertAlign: TTextLayout); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop); overload;
    procedure TextRect(ARect: TRectF; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    function TextWidth(const Text: UTF8String): single; virtual; abstract;
    function TextHeight(const Text: UTF8String): single; overload; virtual; abstract;
    function TextHeight(const Text: UTF8String; AWidth: single): single; overload; virtual; abstract;
    procedure SetGradientColors(ATopLeft, ATopRight, ABottomRight, ABottomLeft: TBGRAPixel); virtual; abstract;

    property Scale: single read GetScale write SetScale;
    property StepX: single read GetStepX write SetStepX;
    property Justify: boolean read GetJustify write SetJustify;
    property Clipped: boolean read GetClipped write SetClipped;
    property HorizontalAlign: TAlignment read GetHorizontalAlign write SetHorizontalAlign;
    property VerticalAlign: TTextLayout read GetVerticalAlign write SetVerticalAlign;
    property GradientColors: boolean read GetUseGradientColors write SetUseGradientColors;
    property Padding: TRectF read GetPadding write SetPadding;
  end;

  { IBGLTexture }

  IBGLTexture = interface ['{BF2FF051-EBC6-4102-8268-37A9D0297B92}']
    function GetFlipX: IBGLTexture;
    function GetFlipY: IBGLTexture;
    function GetFrame(AIndex: integer): IBGLTexture;
    function GetFrameCount: integer;
    function GetFrameHeight: integer;
    function GetFrameWidth: integer;
    function GetHeight: integer;
    function GetImageCenter: TPointF;
    function GetMask: IBGLTexture;
    function GetOpenGLBlendMode: TOpenGLBlendMode;
    function GetOpenGLTexture: TBGLTextureHandle;
    function GetResampleFilter: TOpenGLResampleFilter;
    function GetUseGradientColors: boolean;
    function GetWidth: integer;

    procedure SetFrameSize(x,y: integer);
    procedure SetImageCenter(const AValue: TPointF);
    procedure SetOpenGLBlendMode(AValue: TOpenGLBlendMode);
    procedure SetResampleFilter(AValue: TOpenGLResampleFilter);
    procedure SetGradientColors(ATopLeft, ATopRight, ABottomRight, ABottomLeft: TBGRAPixel);
    procedure SetUseGradientColors(AValue: boolean);
    procedure Update(ARGBAData: PLongWord; AllocatedWidth, AllocatedHeight, ActualWidth,ActualHeight: integer; RGBAOrder: boolean = true);
    procedure ToggleFlipX;
    procedure ToggleFlipY;
    procedure ToggleMask;
    function FilterBlurMotion(ARadius: single; ABlurType: TRadialBlurType; ADirection: TPointF): IBGLTexture;
    function FilterBlurRadial(ARadius: single; ABlurType: TRadialBlurType): IBGLTexture;
    procedure SetFrame(AIndex: integer);
    procedure FreeMemory;
    procedure Bind(ATextureNumber: integer);

    procedure Draw(x,y: single; AAlpha: byte = 255); overload;
    procedure Draw(x,y: single; AColor: TBGRAPixel); overload;
    procedure Draw(x,y: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure Draw(x,y: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure StretchDraw(x,y,w,h: single; AAlpha: byte = 255); overload;
    procedure StretchDraw(x,y,w,h: single; AColor: TBGRAPixel); overload;
    procedure StretchDraw(x,y,w,h: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure StretchDraw(x,y,w,h: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure DrawAngle(x,y,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AAlpha: byte = 255); overload;
    procedure DrawAngle(x,y,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AColor: TBGRAPixel); overload;
    procedure DrawAngle(x,y,angleDeg: single; AAlpha: byte = 255); overload;
    procedure DrawAngle(x,y,angleDeg: single; AColor: TBGRAPixel); overload;
    procedure DrawAngle(x,y,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure DrawAngle(x,y,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AAlpha: byte = 255); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AColor: TBGRAPixel); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AAlpha: byte = 255); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AColor: TBGRAPixel); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure DrawAffine(const Origin, HAxis, VAxis: TPointF; AAlpha: byte = 255); overload;
    procedure DrawAffine(const Origin, HAxis, VAxis: TPointF; AColor: TBGRAPixel); overload;
    procedure DrawAffine(x,y: single; const AMatrix: TAffineMatrix; AAlpha: byte = 255); overload;
    procedure DrawAffine(x,y: single; const AMatrix: TAffineMatrix; AColor: TBGRAPixel); overload;
    procedure DrawTriangle(const APoints: array of TPointF; const ATexCoords: array of TPointF); overload;
    procedure DrawTriangle(const APoints: array of TPointF; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawTriangle(const APoints: array of TPointF; const APointsZ: array of Single; const ATexCoords: array of TPointF); overload;
    procedure DrawTriangle(const APoints: array of TPointF; const APointsZ: array of Single; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawTriangle(const APoints3D: array of TPoint3D_128; const ATexCoords: array of TPointF); overload;
    procedure DrawTriangle(const APoints3D: array of TPoint3D_128; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawTriangle(const APoints3D: array of TPoint3D_128; const ANormals3D: array of TPoint3D_128; const ATexCoords: array of TPointF); overload;
    procedure DrawTriangle(const APoints3D: array of TPoint3D_128; const ANormals3D: array of TPoint3D_128; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawQuad(const APoints: array of TPointF; const ATexCoords: array of TPointF); overload;
    procedure DrawQuad(const APoints: array of TPointF; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawQuad(const APoints: array of TPointF; const APointsZ: array of Single; const ATexCoords: array of TPointF); overload;
    procedure DrawQuad(const APoints: array of TPointF; const APointsZ: array of Single; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawQuad(const APoints3D: array of TPoint3D_128; const ATexCoords: array of TPointF); overload;
    procedure DrawQuad(const APoints3D: array of TPoint3D_128; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawQuad(const APoints3D: array of TPoint3D_128; const ANormals3D: array of TPoint3D_128; const ATexCoords: array of TPointF); overload;
    procedure DrawQuad(const APoints3D: array of TPoint3D_128; const ANormals3D: array of TPoint3D_128; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;

    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property FrameCount: integer read GetFrameCount;
    property Frame[AIndex: integer]: IBGLTexture read GetFrame;
    property FrameWidth: integer read GetFrameWidth;
    property FrameHeight: integer read GetFrameHeight;
    property FlipX: IBGLTexture read GetFlipX;
    property FlipY: IBGLTexture read GetFlipY;
    property Mask: IBGLTexture read GetMask;
    property Handle: TBGLTextureHandle read GetOpenGLTexture;
    property ImageCenter: TPointF read GetImageCenter write SetImageCenter;
    property ResampleFilter: TOpenGLResampleFilter read GetResampleFilter write SetResampleFilter;
    property BlendMode: TOpenGLBlendMode read GetOpenGLBlendMode write SetOpenGLBlendMode;
    property GradientColors: boolean read GetUseGradientColors write SetUseGradientColors;
  end;

  { TBGLCustomBitmap }

  TBGLCustomBitmap = class(TBGRABitmap)
  protected
    FActualWidth,FActualHeight,
    FAllocatedWidth,FAllocatedHeight: integer;
    FTextureInvalidated: boolean;
    FActualRect: TRect;
    FTexture: IBGLTexture;
    procedure Init; override;
    function GetTexture: IBGLTexture; virtual;
    function GetOpenGLMaxTexSize: integer; virtual; abstract;
    procedure NotifySizeTooBigForOpenGL; virtual;
    procedure NotifyOpenGLContextNotCreatedYet; virtual;
    function GetTextureGL: IUnknown; override;
    procedure SwapRedBlueWithoutInvalidate(ARect: TRect);
    procedure SetClipRect(const AValue: TRect); override;
  public
    procedure InvalidateBitmap; override;
    procedure Fill(const c: TBGRAPixel); override;
    procedure NoClip; override;
    destructor Destroy; override;
    procedure SwapRedBlue; overload; override;
    function Resample(newWidth, newHeight: integer; mode: TResampleMode=rmFineResample): TBGLCustomBitmap; override;
    procedure ApplyGlobalOpacity(alpha: byte); overload; override;
    procedure ReplaceColor(before, after: TColor); overload; override;
    procedure ReplaceColor(const ABefore, AAfter: TBGRAPixel); overload; override;
    procedure ReplaceTransparent(const AAfter: TBGRAPixel); overload; override;
    procedure SetSize(AWidth, AHeight: integer); override;
    property Width: integer read FActualWidth;
    property Height: integer read FActualHeight;
    property AllocatedWidth: integer read FAllocatedWidth;
    property AllocatedHeight: integer read FAllocatedHeight;
    function MakeTextureAndFree: IBGLTexture;
    property Texture: IBGLTexture read GetTexture;
    property MaxTextureSize: integer read GetOpenGLMaxTexSize;
  end;

  { TBGLCustomTexture }

  TBGLCustomTexture = class(TInterfacedObject, IBGLTexture)
  private
    function GetFlipX: IBGLTexture;
    function GetFlipY: IBGLTexture;
    function GetFrame(AIndex: integer): IBGLTexture;
    function GetFrameCount: integer;
    function GetFrameHeight: integer;
    function GetFrameWidth: integer;
    function GetHeight: integer;
    function GetMask: IBGLTexture;
    function GetOpenGLBlendMode: TOpenGLBlendMode;
    function GetOpenGLTexture: TBGLTextureHandle;
    function GetWidth: integer;
    function GetImageCenter: TPointF;
    procedure SetImageCenter(const AValue: TPointF);
    function GetResampleFilter: TOpenGLResampleFilter;
    procedure SetOpenGLBlendMode(AValue: TOpenGLBlendMode);
    procedure SetResampleFilter(AValue: TOpenGLResampleFilter);
  protected
    FOpenGLTexture: TBGLTextureHandle;
    FOpenGLTextureOwned: boolean;
    FResampleFilter: TOpenGLResampleFilter;
    FWidth,FHeight: integer;
    FImageCenter: TPointF;
    FFrame: integer;
    FFrameWidth,FFrameHeight: integer;
    FIsMask: boolean;
    FGradTopLeft, FGradTopRight, FGradBottomRight, FGradBottomLeft: TBGRAPixel;
    FUseGradientColor: boolean;
    FBlendMode: TOpenGLBlendMode;

    function GetOpenGLMaxTexSize: integer; virtual; abstract;
    function CreateOpenGLTexture(ARGBAData: PLongWord; AAllocatedWidth, AAllocatedHeight, AActualWidth, AActualHeight: integer; RGBAOrder: boolean): TBGLTextureHandle; virtual; abstract;
    procedure UpdateOpenGLTexture(ATexture: TBGLTextureHandle; ARGBAData: PLongWord; AAllocatedWidth, AAllocatedHeight, AActualWidth,AActualHeight: integer; RGBAOrder: boolean); virtual; abstract;
    class function SupportsBGRAOrder: boolean; virtual;
    procedure SetOpenGLTextureSize(ATexture: TBGLTextureHandle; AAllocatedWidth, AAllocatedHeight, AActualWidth, AActualHeight: integer); virtual; abstract;
    procedure ComputeOpenGLFramesCoord(ATexture: TBGLTextureHandle; FramesX: Integer=1; FramesY: Integer=1); virtual; abstract;
    function GetOpenGLFrameCount(ATexture: TBGLTextureHandle): integer; virtual; abstract;
    function GetEmptyTexture: TBGLTextureHandle; virtual; abstract;
    procedure FreeOpenGLTexture(ATexture: TBGLTextureHandle); virtual; abstract;
    procedure UpdateGLResampleFilter(ATexture: TBGLTextureHandle; AFilter: TOpenGLResampleFilter); virtual; abstract;
    function GetUseGradientColors: boolean; virtual;
    procedure SetUseGradientColors(AValue: boolean); virtual;

    procedure DoDrawTriangleOrQuad(const {%H-}Points: array of TPointF;
      const {%H-}APointsZ: array of Single; const {%H-}APoints3D: array of TPoint3D_128;
      const {%H-}ANormals3D: array of TPoint3D_128; const {%H-}TexCoords: array of TPointF;
      const {%H-}AColors: array of TColorF); virtual;
    procedure DoStretchDraw(x,y,w,h: single; AColor: TBGRAPixel); virtual; abstract;
    procedure DoStretchDrawAngle(x,y,w,h,angleDeg: single; rotationCenter: TPointF; AColor: TBGRAPixel); virtual; abstract;
    procedure DoDrawAffine(Origin, HAxis, VAxis: TPointF; AColor: TBGRAPixel); virtual; abstract;
    function NewEmpty: TBGLCustomTexture; virtual; abstract;
    function NewFromTexture(ATexture: TBGLTextureHandle; AWidth,AHeight: integer): TBGLCustomTexture; virtual; abstract;
    procedure NotifyInvalidFrameSize; virtual;
    procedure NotifyErrorLoadingFile({%H-}AFilename: string); virtual;

    procedure Init(ATexture: TBGLTextureHandle; AWidth,AHeight: integer; AOwned: boolean); virtual;
    function Duplicate: TBGLCustomTexture; virtual;
    procedure FreeMemoryOnDestroy; virtual;

    procedure InitEmpty;
    procedure InitFromData(ARGBAData: PLongWord; AllocatedWidth,AllocatedHeight, ActualWidth,ActualHeight: integer; RGBAOrder: boolean);
    procedure InitFromStream(AStream: TStream);
  public
    destructor Destroy; override;
    constructor Create; overload;
    constructor Create(ATexture: TBGLTextureHandle; AWidth,AHeight: integer); overload;
    constructor Create(ARGBAData: PLongWord; AllocatedWidth,AllocatedHeight, ActualWidth,ActualHeight: integer; RGBAOrder: boolean = true); overload;
    constructor Create(AFPImage: TFPCustomImage); overload;
    constructor Create(ABitmap: TBitmap); overload;
    constructor Create(AWidth, AHeight: integer; Color: TColor); overload;
    constructor Create(AWidth, AHeight: integer; Color: TBGRAPixel); overload;
    constructor Create(AFilenameUTF8: string); overload;
    constructor Create(AFilenameUTF8: string; AWidth,AHeight: integer; AResampleFilter: TResampleFilter); overload;
    constructor Create(AStream: TStream); overload;
    procedure ToggleFlipX; virtual; abstract;
    procedure ToggleFlipY; virtual; abstract;
    procedure ToggleMask; virtual;
    function FilterBlurMotion({%H-}ARadius: single; {%H-}ABlurType: TRadialBlurType; {%H-}ADirection: TPointF): IBGLTexture; virtual;
    function FilterBlurRadial({%H-}ARadius: single; {%H-}ABlurType: TRadialBlurType): IBGLTexture; virtual;

    procedure SetFrameSize(x,y: integer);
    procedure Update(ARGBAData: PLongWord; AllocatedWidth, AllocatedHeight, ActualWidth,ActualHeight: integer; RGBAOrder: boolean = true);
    procedure SetFrame(AIndex: integer);
    procedure SetGradientColors(ATopLeft, ATopRight, ABottomRight, ABottomLeft: TBGRAPixel);
    procedure FreeMemory;
    procedure Bind({%H-}ATextureNumber: integer); virtual;

    procedure Draw(x,y: single; AAlpha: byte = 255); overload;
    procedure Draw(x,y: single; AColor: TBGRAPixel); overload;
    procedure Draw(x,y: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure Draw(x,y: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure StretchDraw(x,y,w,h: single; AAlpha: byte = 255); overload;
    procedure StretchDraw(x,y,w,h: single; AColor: TBGRAPixel); overload;
    procedure StretchDraw(x,y,w,h: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure StretchDraw(x,y,w,h: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure DrawAngle(x,y,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AAlpha: byte = 255); overload;
    procedure DrawAngle(x,y,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AColor: TBGRAPixel); overload;
    procedure DrawAngle(x,y,angleDeg: single; AAlpha: byte = 255); overload;
    procedure DrawAngle(x,y,angleDeg: single; AColor: TBGRAPixel); overload;
    procedure DrawAngle(x,y,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure DrawAngle(x,y,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AAlpha: byte = 255); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AColor: TBGRAPixel); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AAlpha: byte = 255); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AColor: TBGRAPixel); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure DrawAffine(const Origin, HAxis, VAxis: TPointF; AAlpha: byte = 255); overload;
    procedure DrawAffine(const Origin, HAxis, VAxis: TPointF; AColor: TBGRAPixel); overload;
    procedure DrawAffine(x,y: single; const AMatrix: TAffineMatrix; AAlpha: byte = 255); overload;
    procedure DrawAffine(x,y: single; const AMatrix: TAffineMatrix; AColor: TBGRAPixel); overload;
    procedure DrawTriangle(const APoints: array of TPointF; const ATexCoords: array of TPointF); overload;
    procedure DrawTriangle(const APoints: array of TPointF; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawTriangle(const APoints: array of TPointF; const APointsZ: array of Single; const ATexCoords: array of TPointF); overload;
    procedure DrawTriangle(const APoints: array of TPointF; const APointsZ: array of Single; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawTriangle(const APoints3D: array of TPoint3D_128; const ATexCoords: array of TPointF); overload;
    procedure DrawTriangle(const APoints3D: array of TPoint3D_128; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawTriangle(const APoints3D: array of TPoint3D_128; const ANormals3D: array of TPoint3D_128; const ATexCoords: array of TPointF); overload;
    procedure DrawTriangle(const APoints3D: array of TPoint3D_128; const ANormals3D: array of TPoint3D_128; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawQuad(const APoints: array of TPointF; const ATexCoords: array of TPointF); overload;
    procedure DrawQuad(const APoints: array of TPointF; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawQuad(const APoints: array of TPointF; const APointsZ: array of Single; const ATexCoords: array of TPointF); overload;
    procedure DrawQuad(const APoints: array of TPointF; const APointsZ: array of Single; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawQuad(const APoints3D: array of TPoint3D_128; const ATexCoords: array of TPointF); overload;
    procedure DrawQuad(const APoints3D: array of TPoint3D_128; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;
    procedure DrawQuad(const APoints3D: array of TPoint3D_128; const ANormals3D: array of TPoint3D_128; const ATexCoords: array of TPointF); overload;
    procedure DrawQuad(const APoints3D: array of TPoint3D_128; const ANormals3D: array of TPoint3D_128; const ATexCoords: array of TPointF; const AColors: array of TColorF); overload;

    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property FrameCount: integer read GetFrameCount;
    property Frame[AIndex: integer]: IBGLTexture read GetFrame;
    property FrameWidth: integer read GetFrameWidth;
    property FrameHeight: integer read GetFrameHeight;
    property FlipX: IBGLTexture read GetFlipX;
    property FlipY: IBGLTexture read GetFlipY;
    property Mask: IBGLTexture read GetMask;
    property Handle: TBGLTextureHandle read GetOpenGLTexture;
    property ResampleFilter: TOpenGLResampleFilter read GetResampleFilter write SetResampleFilter;
    property BlendMode: TOpenGLBlendMode read GetOpenGLBlendMode write SetOpenGLBlendMode;
    property GradientColors: boolean read GetUseGradientColors write SetUseGradientColors;
  end;

  { TBGLCustomFrameBuffer }

  TBGLCustomFrameBuffer = class
  protected
    FCanvas: pointer;
    function GetTexture: IBGLTexture; virtual; abstract;
    function GetHandle: pointer; virtual; abstract;
    function GetMatrix: TAffineMatrix; virtual; abstract;
    function GetHeight: integer; virtual; abstract;
    function GetProjectionMatrix: TMatrix4D; virtual; abstract;
    function GetWidth: integer; virtual; abstract;
    procedure SetMatrix(AValue: TAffineMatrix); virtual; abstract;
    procedure SetProjectionMatrix(AValue: TMatrix4D); virtual; abstract;

  public
    procedure UseOrthoProjection; overload; virtual;
    procedure UseOrthoProjection(AMinX,AMinY,AMaxX,AMaxY: single); overload; virtual;
    function MakeTextureAndFree: IBGLTexture; virtual;

    procedure SetCanvas(ACanvas: Pointer); //for internal use
    property Matrix: TAffineMatrix read GetMatrix write SetMatrix;
    property ProjectionMatrix: TMatrix4D read GetProjectionMatrix write SetProjectionMatrix;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property Handle: pointer read GetHandle;
    property Texture: IBGLTexture read GetTexture;
  end;

type
  TBGLBitmapAny = class of TBGLCustomBitmap;
  TBGLTextureAny = class of TBGLCustomTexture;

var
  BGLBitmapFactory : TBGLBitmapAny;
  BGLTextureFactory: TBGLTextureAny;

function OrthoProjectionToOpenGL(AMinX,AMinY,AMaxX,AMaxY: Single): TMatrix4D;
function GetPowerOfTwo( Value : Integer ) : Integer;

Implementation
End.

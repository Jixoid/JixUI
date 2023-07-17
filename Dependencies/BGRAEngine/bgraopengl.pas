// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAOpenGL;

{$mode objfpc}{$H+}
{$I bgrabitmap.map.inc}

interface

uses
  BGRAClasses, SysUtils, FPimage, BGRAGraphics,
  BGRAOpenGLType, BGRASpriteGL, BGRACanvasGL, GL, GLext, GLU, BGRABitmapTypes,
  BGRAFontGL, BGRASSE, BGRAMatrix3D;

type
  TBGLCustomCanvas = BGRACanvasGL.TBGLCustomCanvas;
  TBGLSprite = TBGLDefaultSprite;
  IBGLTexture = BGRAOpenGLType.IBGLTexture;
  IBGLFont = BGRAOpenGLType.IBGLFont;
  IBGLRenderedFont = BGRAFontGL.IBGLRenderedFont;
  TOpenGLResampleFilter = BGRAOpenGLType.TOpenGLResampleFilter;
  TOpenGLBlendMode = BGRAOpenGLType.TOpenGLBlendMode;
  TBGLPath = BGRACanvasGL.TBGLPath;
  TWaitForGPUOption = BGRAOpenGLType.TWaitForGPUOption;
  TBGLCustomElementArray = BGRACanvasGL.TBGLCustomElementArray;
  TBGLCustomArray = BGRACanvasGL.TBGLCustomArray;
  TOpenGLPrimitive = BGRAOpenGLType.TOpenGLPrimitive;
  TTextLayout = BGRAGraphics.TTextLayout;

const
  tlTop = BGRAGraphics.tlTop;
  tlCenter = BGRAGraphics.tlCenter;
  tlBottom = BGRAGraphics.tlBottom;

type
  { TBGLContext }

  TBGLContext = object
  private
    function GetHeight: integer;
    function GetWidth: integer;
  public
    Canvas: TBGLCustomCanvas;
    Sprites: TBGLCustomSpriteEngine;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
  end;

  { TBGLFrameBuffer }

  TBGLFrameBuffer = class(TBGLCustomFrameBuffer)
  protected
    FHeight: integer;
    FMatrix: TAffineMatrix;
    FProjectionMatrix: TMatrix4D;
    FTexture: IBGLTexture;
    FFrameBufferId, FRenderBufferId: GLuint;
    FWidth: integer;
    FSettingMatrices: boolean;
    function GetTexture: IBGLTexture; override;
    function GetHandle: pointer; override;
    function GetHeight: integer; override;
    function GetMatrix: TAffineMatrix; override;
    function GetProjectionMatrix: TMatrix4D; override;
    function GetWidth: integer; override;
    procedure SetMatrix(AValue: TAffineMatrix); override;
    procedure SetProjectionMatrix(AValue: TMatrix4D); override;
  public
    constructor Create(AWidth,AHeight: integer);
    function MakeTextureAndFree: IBGLTexture; override;
    destructor Destroy; override;
  end;

const
  orfBox = BGRAOpenGLType.orfBox;
  orfLinear = BGRAOpenGLType.orfLinear;
  obmNormal = BGRAOpenGLType.obmNormal;
  obmAdd = BGRAOpenGLType.obmAdd;
  obmMultiply = BGRAOpenGLType.obmMultiply;
  wfgQueueAllCommands = BGRAOpenGLType.wfgQueueAllCommands;
  wfgFinishAllCommands = BGRAOpenGLType.wfgFinishAllCommands;
  opPoints = BGRAOpenGLType.opPoints;
  opLineStrip = BGRAOpenGLType.opLineStrip;
  opLineLoop = BGRAOpenGLType.opLineLoop;
  opLines = BGRAOpenGLType.opLines;
  opTriangleStrip = BGRAOpenGLType.opTriangleStrip;
  opTriangleFan = BGRAOpenGLType.opTriangleFan;
  opTriangles = BGRAOpenGLType.opTriangles;

type
  { TBGLBitmap }

  TBGLBitmap = class(TBGLCustomBitmap)
  protected
    function GetOpenGLMaxTexSize: integer; override;
  public
    function NewBitmap: TBGLBitmap; overload; override;
    function NewBitmap(AWidth, AHeight: integer): TBGLBitmap; overload; override;
    function NewBitmap(AWidth, AHeight: integer; const Color: TBGRAPixel): TBGLBitmap; overload; override;
    function NewBitmap(AWidth, AHeight: integer; AColor: Pointer): TBGLBitmap; overload; override;
    function NewBitmap(Filename: string): TBGLBitmap; overload; override;
    function NewBitmap(Filename: string; AIsUtf8: boolean): TBGLBitmap; overload; override;
    function NewBitmap(Filename: string; AIsUtf8: boolean; AOptions: TBGRALoadingOptions): TBGLBitmap; overload; override;
    function NewBitmap(AFPImage: TFPCustomImage): TBGLBitmap; overload; override;
    function NewReference: TBGLBitmap; override;
    function GetUnique: TBGLBitmap; override;
    function Duplicate(DuplicateProperties: Boolean = False): TBGLBitmap; overload; override;
    function Duplicate(DuplicateProperties, DuplicateXorMask: Boolean) : TBGLBitmap; overload; override;
    function GetPart(const ARect: TRect): TBGLBitmap; override;
    function CreateBrushTexture(ABrushStyle: TBrushStyle; APatternColor, ABackgroundColor: TBGRAPixel;
                AWidth: integer = 8; AHeight: integer = 8; APenWidth: single = 1): TBGLBitmap; override;
    function Resample(newWidth, newHeight: integer;
      mode: TResampleMode = rmFineResample): TBGLBitmap; override;
    function FilterSmartZoom3(Option: TMedianOption): TBGLBitmap; override;
    function FilterMedian(Option: TMedianOption): TBGLBitmap; override;
    function FilterSmooth: TBGLBitmap; override;
    function FilterSharpen(Amount: single = 1): TBGLBitmap; overload; override;
    function FilterSharpen(ABounds: TRect; Amount: single = 1): TBGLBitmap; overload; override;
    function FilterContour(AGammaCorrection: boolean = false): TBGLBitmap; override;
    function FilterPixelate(pixelSize: integer; useResample: boolean; filter: TResampleFilter = rfLinear): TBGLBitmap; override;
    function FilterBlurRadial(radius: single; blurType: TRadialBlurType): TBGLBitmap; overload; override;
    function FilterBlurRadial(const ABounds: TRect; radius: single; blurType: TRadialBlurType): TBGLBitmap; overload; override;
    function FilterBlurRadial(radiusX, radiusY: single; blurType: TRadialBlurType): TBGLBitmap; overload; override;
    function FilterBlurRadial(const ABounds: TRect; radiusX, radiusY: single; blurType: TRadialBlurType): TBGLBitmap; overload; override;
    function FilterBlurMotion(distance: single; angle: single; oriented: boolean): TBGLBitmap; overload; override;
    function FilterBlurMotion(const ABounds: TRect; distance: single; angle: single; oriented: boolean): TBGLBitmap; overload; override;
    function FilterCustomBlur(mask: TCustomUniversalBitmap): TBGLBitmap; overload; override;
    function FilterCustomBlur(const ABounds: TRect; mask: TCustomUniversalBitmap): TBGLBitmap; overload; override;
    function FilterEmboss(angle: single; AStrength: integer= 64; AOptions: TEmbossOptions = []): TBGLBitmap; overload; override;
    function FilterEmboss(angle: single; ABounds: TRect; AStrength: integer= 64; AOptions: TEmbossOptions = []): TBGLBitmap; overload; override;
    function FilterEmbossHighlight(FillSelection: boolean): TBGLBitmap; overload; override;
    function FilterEmbossHighlight(FillSelection: boolean; BorderColor: TBGRAPixel): TBGLBitmap; overload; override;
    function FilterEmbossHighlight(FillSelection: boolean; BorderColor: TBGRAPixel; var Offset: TPoint): TBGLBitmap; overload; override;
    function FilterGrayscale: TBGLBitmap; overload; override;
    function FilterGrayscale(ABounds: TRect): TBGLBitmap; overload; override;
    function FilterNormalize(eachChannel: boolean = True): TBGLBitmap; overload; override;
    function FilterNormalize(ABounds: TRect; eachChannel: boolean = True): TBGLBitmap; overload; override;
    function FilterRotate(origin: TPointF; angle: single; correctBlur: boolean = false): TBGLBitmap; override;
    function FilterAffine(AMatrix: TAffineMatrix; correctBlur: boolean = false): TBGLBitmap; override;
    function FilterSphere: TBGLBitmap; override;
    function FilterTwirl(ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGLBitmap; overload; override;
    function FilterTwirl(ABounds: TRect; ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGLBitmap; overload; override;
    function FilterCylinder: TBGLBitmap; override;
    function FilterPlane: TBGLBitmap; override;
  end;

function BGLTexture(ARGBAData: PLongWord; AllocatedWidth,AllocatedHeight, ActualWidth,ActualHeight: integer): IBGLTexture; overload;
function BGLTexture(AFPImage: TFPCustomImage): IBGLTexture; overload;
function BGLTexture(ABitmap: TBitmap): IBGLTexture; overload;
function BGLTexture(AWidth, AHeight: integer; Color: TColor): IBGLTexture; overload;
function BGLTexture(AWidth, AHeight: integer; Color: TBGRAPixel): IBGLTexture; overload;
function BGLTexture(AFilenameUTF8: string): IBGLTexture; overload;
function BGLTexture(AFilenameUTF8: string; AWidth, AHeight: integer; AResampleFilter: TResampleFilter = rfBox): IBGLTexture; overload;
function BGLTexture(AStream: TStream): IBGLTexture; overload;

function BGLSpriteEngine: TBGLCustomSpriteEngine;

function BGLCanvas: TBGLCustomCanvas;

procedure BGLViewPort(AWidth,AHeight: integer); overload;
procedure BGLViewPort(AWidth,AHeight: integer; AColor: TBGRAPixel); overload;

function BGLFont({%H-}AName: string; {%H-}AEmHeight: integer; {%H-}AStyle: TFontStyles = []): IBGLRenderedFont; overload;
function BGLFont({%H-}AName: string; {%H-}AEmHeight: integer; {%H-}AColor: TBGRAPixel; {%H-}AStyle: TFontStyles = []): IBGLRenderedFont; overload;
function BGLFont({%H-}AName: string; {%H-}AEmHeight: integer; {%H-}AColor: TBGRAPixel; {%H-}AOutlineColor: TBGRAPixel; {%H-}AStyle: TFontStyles = []): IBGLRenderedFont; overload;
function BGLFont({%H-}AName: string; {%H-}AEmHeight: integer; ARenderer: TBGRACustomFontRenderer; ARendererOwned: boolean = true): IBGLRenderedFont; overload;

type
  { TBGLElementArray }

  TBGLElementArray = class(TBGLCustomElementArray)
  protected
    FElements: packed array of GLuint;
    FBuffer: GLuint;
    function GetCount: integer; override;
  public
    constructor Create(const AElements: array of integer); override;
    procedure Draw(ACanvas: TBGLCustomCanvas; APrimitive: TOpenGLPrimitive; AAttributes: array of TAttributeVariable); override;
    destructor Destroy; override;
  end;

  { TBGLArray }

  TBGLArray = class(TBGLCustomArray)
  protected
    FBufferAddress: pointer;
    FCount: integer;
    FRecordSize: integer;
    function GetCount: integer; override;
    function GetRecordSize: integer; override;
  public
    constructor Create(ABufferAddress: Pointer; ACount: integer; ARecordSize: integer); override;
    destructor Destroy; override;
  end;

Implementation
End.

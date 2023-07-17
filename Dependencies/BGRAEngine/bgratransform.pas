// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRATransform;

{$mode objfpc}
{$WARN 5093 off : function result variable of a managed type does not seem to be initialized}
interface

{ This unit contains bitmap transformations as classes and the TAffineMatrix record and functions. }

uses
  BGRAClasses, SysUtils, BGRABitmapTypes;

type
  { Contains an affine matrix, i.e. a matrix to transform linearly and translate TPointF coordinates }
  TAffineMatrix = BGRABitmapTypes.TAffineMatrix;
  { Contains an affine base and information on the resulting box }
  TAffineBox = BGRABitmapTypes.TAffineBox;

  { TBGRAAffineScannerTransform allow to transform any scanner. To use it,
    create this object with a scanner as parameter, call transformation
    procedures, and finally, use the newly created object as a scanner.

    You can transform a gradient or a bitmap. See TBGRAAffineBitmapTransform
    for bitmap specific transformation. }

  { TBGRAAffineScannerTransform }

  TBGRAAffineScannerTransform = class(TBGRACustomScanner)
  protected
    FScanner: IBGRAScanner;
    FScanAtFunc: TScanAtFunction;
    FCur: TPointF;
    FEmptyMatrix: Boolean;
    FMatrix: TAffineMatrix;
    procedure SetMatrix(AMatrix: TAffineMatrix);
    function InternalScanCurrentPixel: TBGRAPixel; virtual;
    function GetViewMatrix: TAffineMatrix;
    procedure SetViewMatrix(AValue: TAffineMatrix);
  public
    GlobalOpacity: Byte;
    constructor Create(AScanner: IBGRAScanner);
    procedure Reset;
    procedure Invert;
    procedure Translate(OfsX,OfsY: Single);
    procedure RotateDeg(AngleCW: Single);
    procedure RotateRad(AngleCCW: Single);
    procedure MultiplyBy(AMatrix: TAffineMatrix);
    procedure Fit(Origin,HAxis,VAxis: TPointF); virtual;
    procedure Scale(sx,sy: single); overload;
    procedure Scale(factor: single); overload;
    function GetScanCustomColorspace: TColorspaceAny; override;
    procedure ScanMoveTo(X, Y: Integer); override;
    procedure ScanMoveToF(X, Y: single); inline;
    function ScanNextPixel: TBGRAPixel; override;
    procedure ScanSkipPixels(ACount: integer); override;
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    property Matrix: TAffineMatrix read FMatrix write SetMatrix;
    property ViewMatrix: TAffineMatrix read GetViewMatrix write SetViewMatrix;
  end;

  { If you don't want the bitmap to repeats itself, or want to specify the
    resample filter, or want to fit easily the bitmap on axes,
    use TBGRAAffineBitmapTransform instead of TBGRAAffineScannerTransform }

  { TBGRAAffineBitmapTransform }

  TBGRAAffineBitmapTransform = class(TBGRAAffineScannerTransform)
  protected
    FBitmap: TBGRACustomBitmap;
    FRepeatImageX,FRepeatImageY: boolean;
    FResampleFilter : TResampleFilter;
    FBuffer: PBGRAPixel;
    FBufferSize: Int32or64;
    FIncludeEdges: boolean;
    procedure Init(ABitmap: TBGRACustomBitmap; ARepeatImageX: Boolean= false; ARepeatImageY: Boolean= false; AResampleFilter: TResampleFilter = rfLinear; AIncludeEdges: boolean = false);
  public
    constructor Create(ABitmap: TBGRACustomBitmap; ARepeatImage: Boolean= false; AResampleFilter: TResampleFilter = rfLinear; AIncludeEdges: boolean = false); overload;
    constructor Create(ABitmap: TBGRACustomBitmap; ARepeatImageX: Boolean; ARepeatImageY: Boolean; AResampleFilter: TResampleFilter = rfLinear; AIncludeEdges: boolean = false); overload;
    destructor Destroy; override;
    function InternalScanCurrentPixel: TBGRAPixel; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
    function IsScanPutPixelsDefined: boolean; override;
    procedure Fit(Origin, HAxis, VAxis: TPointF); override;
  end;

  { TBGRAQuadLinearScanner }

  TBGRAQuadLinearScanner = class(TBGRACustomScanner)
  private
    FPadding: boolean;
    FPoints,FVectors: array[0..3] of TPointF;
    FInvLengths,FDets: array[0..3] of single;
    FCoeffs: array[0..3] of TPointF;
    aa,bb0,cc0,inv2aa: double;
    FSource: IBGRAScanner;
    FSourceMatrix: TAffineMatrix;
    FUVVector: TPointF;

    ScanParaBB, ScanParaCC, ScanParaBBInv: double;

    ScanVertV0,ScanVertVStep0,ScanVertDenom0,ScanVertDenomStep0: double;

    FHasC1, FHasC2: boolean;
    FShowC1, FShowC2: boolean;
    FScanFunc: TScanNextPixelFunction;
    FCurXF,FCurYF: single;
    FBuffer: PBGRAPixel;
    FBufferSize: Int32or64;
    FTextureInterpolation: Boolean;
    function GetCulling: TFaceCulling;
    function ScanNone: TBGRAPixel;
    function ScanGeneral: TBGRAPixel;
    procedure PrepareScanVert0;
    function ScanVert0: TBGRAPixel;
    procedure PrepareScanPara;
    function ScanPara: TBGRAPixel;
    procedure GetTexColorAt(u,v: Single; out AColor: TBGRAPixel; out AIsPadding: boolean); inline;
    function GetTexColorAt(u,v: Single; detNeg: boolean): TBGRAPixel; inline;
    procedure ScanMoveToF(X,Y: single); inline;
    procedure SetCulling(AValue: TFaceCulling);
    procedure Init(ASource: IBGRAScanner; const APoints: array of TPointF;
         ATextureInterpolation: boolean);
  public
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
    function IsScanPutPixelsDefined: boolean; override;
    procedure ScanMoveTo(X, Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    procedure ScanSkipPixels(ACount: integer); override;
    constructor Create(ASource: IBGRAScanner;
      ASourceMatrix: TAffineMatrix; const APoints: array of TPointF;
      ATextureInterpolation: boolean = true); overload;
    constructor Create(ASource: IBGRAScanner;
      const ATexCoords: array of TPointF; const APoints: array of TPointF;
      ATextureInterpolation: boolean = true); overload;
    destructor Destroy; override;
    property Culling: TFaceCulling read GetCulling write SetCulling;
    property Padding: boolean read FPadding write FPadding;
  end;

  { TBGRABitmapScanner }

  TBGRABitmapScanner = class(TBGRACustomScanner)
  protected
    FSource: TBGRACustomBitmap;
    FRepeatX,FRepeatY: boolean;
    FScanline: PBGRAPixel;
    FCurX: integer;
    FOrigin: TPoint;
  public
    constructor Create(ASource: TBGRACustomBitmap; ARepeatX,ARepeatY: boolean; AOrigin: TPoint);
    procedure ScanMoveTo(X, Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    procedure ScanSkipPixels(ACount: integer); override;
  end;

  { TBGRAExtendedBorderScanner }

  TBGRAExtendedBorderScanner = class(TBGRACustomScanner)
  protected
    FSource: IBGRAScanner;
    FBounds: TRect;
  public
    constructor Create(ASource: IBGRAScanner; ABounds: TRect);
    function ScanAt(X,Y: Single): TBGRAPixel; override;
  end;

  { TBGRAScannerOffset }

  TBGRAScannerOffset = class(TBGRACustomScanner)
  protected
    FSource: IBGRAScanner;
    FOffset: TPoint;
  public
    constructor Create(ASource: IBGRAScanner; AOffset: TPoint);
    destructor Destroy; override;
    procedure ScanMoveTo(X, Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    function IsScanPutPixelsDefined: boolean; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
    procedure ScanSkipPixels(ACount: integer); override;
  end;


{---------------------- Affine matrix functions -------------------}
//fill a matrix
function AffineMatrix(m11,m12,m13,m21,m22,m23: single): TAffineMatrix; overload;
function AffineMatrix(AU,AV: TPointF; ATranslation: TPointF): TAffineMatrix; overload;

//matrix multiplication
operator *(M,N: TAffineMatrix): TAffineMatrix;
operator =(M,N: TAffineMatrix): boolean;

//matrix multiplication by a vector (apply transformation to that vector)
operator *(M: TAffineMatrix; V: TPointF): TPointF;
operator *(M: TAffineMatrix; A: array of TPointF): ArrayOfTPointF;
operator *(M: TAffineMatrix; ab: TAffineBox): TAffineBox;

//check if matrix is inversible
function IsAffineMatrixInversible(M: TAffineMatrix): boolean;

//check if the matrix is a translation (including the identity)
function IsAffineMatrixTranslation(M: TAffineMatrix): boolean;

//check if the matrix is a scaling (including a projection i.e. with factor 0)
function IsAffineMatrixScale(M: TAffineMatrix): boolean;

//check if the matrix is the identity
function IsAffineMatrixIdentity(M: TAffineMatrix): boolean;

//compute inverse (check if inversible before)
function AffineMatrixInverse(M: TAffineMatrix): TAffineMatrix;

//define a translation matrix
function AffineMatrixTranslation(OfsX,OfsY: Single): TAffineMatrix;

//define a scaling matrix
function AffineMatrixScale(sx,sy: single): TAffineMatrix;
function AffineMatrixScaledRotation(ASourceVector, ATargetVector: TPointF): TAffineMatrix;
function AffineMatrixScaledRotation(ASourcePoint, ATargetPoint, AOrigin: TPointF): TAffineMatrix;

function AffineMatrixSkewXDeg(AngleCW: single): TAffineMatrix;
function AffineMatrixSkewYDeg(AngleCW: single): TAffineMatrix;
function AffineMatrixSkewXRad(AngleCCW: single): TAffineMatrix;
function AffineMatrixSkewYRad(AngleCCW: single): TAffineMatrix;

//define a linear matrix
function AffineMatrixLinear(v1,v2: TPointF): TAffineMatrix; overload;
function AffineMatrixLinear(const AMatrix: TAffineMatrix): TAffineMatrix; overload;

//define a rotation matrix (positive radians are counter-clockwise)
//(assuming the y-axis is pointing down)
function AffineMatrixRotationRad(AngleCCW: Single): TAffineMatrix;

//Positive degrees are clockwise
//(assuming the y-axis is pointing down)
function AffineMatrixRotationDeg(AngleCW: Single): TAffineMatrix;

//define the identity matrix (that do nothing)
function AffineMatrixIdentity: TAffineMatrix;

function IsAffineMatrixOrthogonal(M: TAffineMatrix): boolean;
function IsAffineMatrixScaledRotation(M: TAffineMatrix): boolean;

type
  { TBGRATriangleLinearMapping is a scanner that provides
    an optimized transformation for linear texture mapping
    on triangles }

  { TBGRATriangleLinearMapping }

  TBGRATriangleLinearMapping = class(TBGRACustomScanner)
  protected
    FScanner: IBGRAScanner;
    FMatrix: TAffineMatrix;
    FTexCoord1,FDiff2,FDiff3,FStep: TPointF;
    FCurTexCoord: TPointF;
    FScanAtFunc: TScanAtFunction;
  public
    constructor Create(AScanner: IBGRAScanner; pt1,pt2,pt3: TPointF; tex1,tex2,tex3: TPointF);
    procedure ScanMoveTo(X,Y: Integer); override;
    procedure ScanMoveToF(X,Y: Single);
    function ScanAt(X,Y: Single): TBGRAPixel; override;
    function ScanNextPixel: TBGRAPixel; override;
    procedure ScanSkipPixels(ACount: integer); override;
  end;

type
  TPerspectiveTransform = class;

  { TBGRAPerspectiveScannerTransform }

  TBGRAPerspectiveScannerTransform = class(TBGRACustomScanner)
  private
    FTexture: IBGRAScanner;
    FMatrix: TPerspectiveTransform;
    FScanAtProc: TScanAtFunction;
    function GetIncludeOppositePlane: boolean;
    procedure SetIncludeOppositePlane(AValue: boolean);
  public
    constructor Create(texture: IBGRAScanner; texCoord1,texCoord2: TPointF; const quad: array of TPointF); overload;
    constructor Create(texture: IBGRAScanner; const texCoordsQuad: array of TPointF; const quad: array of TPointF); overload;
    destructor Destroy; override;
    procedure ScanMoveTo(X, Y: Integer); override;
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    function ScanNextPixel: TBGRAPixel; override;
    procedure ScanSkipPixels(ACount: integer); override;
    property IncludeOppositePlane: boolean read GetIncludeOppositePlane write SetIncludeOppositePlane;
  end;

  { TPerspectiveTransform }

  TPerspectiveTransform = class
  private
    sx ,shy ,w0 ,shx ,sy ,w1 ,tx ,ty ,w2 : single;
    scanDenom,scanNumX,scanNumY: single;
    FOutsideValue: TPointF;
    FIncludeOppositePlane: boolean;
    procedure Init;
  public
    constructor Create; overload;
    constructor Create(x1,y1,x2,y2: single; const quad: array of TPointF); overload;
    constructor Create(const quad: array of TPointF; x1,y1,x2,y2: single); overload;
    constructor Create(const srcQuad,destQuad: array of TPointF); overload;
    function MapQuadToQuad(const srcQuad,destQuad: array of TPointF): boolean;
    function MapRectToQuad(x1,y1,x2,y2: single; const quad: array of TPointF): boolean;
    function MapQuadToRect(const quad: array of TPointF; x1,y1,x2,y2: single): boolean;
    function MapSquareToQuad(const quad: array of TPointF): boolean;
    function MapQuadToSquare(const quad: array of TPointF): boolean;
    procedure AssignIdentity;
    function Invert: boolean;
    procedure Translate(dx,dy: single);
    procedure MultiplyBy(a: TPerspectiveTransform);
    procedure PremultiplyBy(b: TPerspectiveTransform);
    function Duplicate: TPerspectiveTransform;
    function Apply(pt: TPointF): TPointF;
    procedure ScanMoveTo(x,y:single);
    function ScanNext: TPointF;
    procedure ScanSkip(ACount: integer);
    property OutsideValue: TPointF read FOutsideValue write FOutsideValue;
    property IncludeOppositePlane: boolean read FIncludeOppositePlane write FIncludeOppositePlane;
  end;

type
  { TBGRATwirlScanner applies a twirl transformation.

    Note : this scanner handles integer coordinates only, so
    any further transformation applied after this one may not
    render correctly. }

  { TBGRATwirlScanner }

  TBGRATwirlScanner = Class(TBGRACustomScanner)
  protected
    FScanner: IBGRAScanner;
    FScanAtFunc: TScanAtFunction;
    FCenter: TPoint;
    FTurn, FRadius, FExponent: Single;
  public
    constructor Create(AScanner: IBGRAScanner; ACenter: TPoint; ARadius: single; ATurn: single = 1; AExponent: single = 3);
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    property Radius: Single read FRadius;
    property Center: TPoint read FCenter;
    property Exponent: Single read FExponent;
  end;

  { TBGRASphereDeformationScanner }

  TBGRASphereDeformationScanner = Class(TBGRACustomScanner)
  protected
    FScanner: IBGRAScanner;
    FScanAtFunc: TScanAtFunction;
    FCenter: TPointF;
    FRadiusX, FRadiusY: Single;
  public
    constructor Create(AScanner: IBGRAScanner; ACenter: TPointF; ARadiusX,ARadiusY: single);
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    property RadiusX: Single read FRadiusX;
    property RadiusY: Single read FRadiusY;
  end;

  { TBGRAVerticalCylinderDeformationScanner }

  TBGRAVerticalCylinderDeformationScanner = Class(TBGRACustomScanner)
  protected
    FScanner: IBGRAScanner;
    FScanAtFunc: TScanAtFunction;
    FCenterX: single;
    FRadiusX: Single;
  public
    constructor Create(AScanner: IBGRAScanner; ACenterX: single; ARadiusX: single);
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    property RadiusX: Single read FRadiusX;
  end;


Implementation
End.

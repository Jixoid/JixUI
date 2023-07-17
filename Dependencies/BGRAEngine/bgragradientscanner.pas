// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAGradientScanner;

{$mode objfpc}{$H+}

interface

{ This unit contains scanners that generate gradients }

uses
  SysUtils, BGRABitmapTypes, BGRATransform;

type
  TBGRAColorInterpolation = (ciStdRGB, ciLinearRGB, ciLinearHSLPositive, ciLinearHSLNegative, ciGSBPositive, ciGSBNegative);
  TBGRAGradientRepetition = (grPad, grRepeat, grReflect, grSine);

  { TBGRASimpleGradient }

  TBGRASimpleGradient = class(TBGRACustomGradient)
  protected
    FColor1,FColor2: TBGRAPixel;
    ec1,ec2: TExpandedPixel;
    FRepetition: TBGRAGradientRepetition;
    function InterpolateToBGRA(position: word): TBGRAPixel; virtual; abstract;
    function InterpolateToExpanded(position: word): TExpandedPixel; virtual; abstract;
  public
    class function CreateAny(AInterpolation: TBGRAColorInterpolation; AColor1,AColor2: TBGRAPixel; ARepetition: TBGRAGradientRepetition): TBGRASimpleGradient; overload; static;
    class function CreateAny(AInterpolation: TBGRAColorInterpolation; AColor1,AColor2: TExpandedPixel; ARepetition: TBGRAGradientRepetition): TBGRASimpleGradient; overload; static;
    constructor Create(AColor1,AColor2: TBGRAPixel; ARepetition: TBGRAGradientRepetition); overload; //do not call directly
    constructor Create(AColor1,AColor2: TExpandedPixel; ARepetition: TBGRAGradientRepetition); overload; //do not call directly
    function GetColorAt(position: integer): TBGRAPixel; override;
    function GetColorAtF(position: single): TBGRAPixel; override;
    function GetExpandedColorAt(position: integer): TExpandedPixel; override;
    function GetExpandedColorAtF(position: single): TExpandedPixel; override;
    function GetAverageColor: TBGRAPixel; override;
    function GetAverageExpandedColor: TExpandedPixel; override;
    function GetMonochrome: boolean; override;
    property Repetition: TBGRAGradientRepetition read FRepetition write FRepetition;
  end;

  { TBGRASimpleGradientWithoutGammaCorrection }

  TBGRASimpleGradientWithoutGammaCorrection = class(TBGRASimpleGradient)
  protected
    function InterpolateToBGRA(position: word): TBGRAPixel; override;
    function InterpolateToExpanded(position: word): TExpandedPixel; override;
  public
    constructor Create(Color1,Color2: TBGRAPixel; ARepetition: TBGRAGradientRepetition = grPad); overload;
    constructor Create(Color1,Color2: TExpandedPixel; ARepetition: TBGRAGradientRepetition = grPad); overload;
  end;

  { TBGRASimpleGradientWithGammaCorrection }

  TBGRASimpleGradientWithGammaCorrection = class(TBGRASimpleGradient)
  protected
    function InterpolateToBGRA(position: word): TBGRAPixel; override;
    function InterpolateToExpanded(position: word): TExpandedPixel; override;
  public
    constructor Create(Color1,Color2: TBGRAPixel; ARepetition: TBGRAGradientRepetition = grPad); overload;
    constructor Create(Color1,Color2: TExpandedPixel; ARepetition: TBGRAGradientRepetition = grPad); overload;
  end;

  THueGradientOption = (hgoRepeat, hgoReflect,                       //repetition
                        hgoPositiveDirection, hgoNegativeDirection,  //hue orientation
                        hgoHueCorrection, hgoLightnessCorrection);   //color interpolation
  THueGradientOptions = set of THueGradientOption;

  { TBGRAHueGradient }

  TBGRAHueGradient = class(TBGRASimpleGradient)
  private
    hsla1,hsla2: THSLAPixel;
    hue1,hue2: LongWord;
    FOptions: THueGradientOptions;
    procedure Init(c1,c2: THSLAPixel; AOptions: THueGradientOptions);
    function InterpolateToHSLA(position: word): THSLAPixel;
  protected
    function InterpolateToBGRA(position: word): TBGRAPixel; override;
    function InterpolateToExpanded(position: word): TExpandedPixel; override;
  public
    constructor Create(Color1,Color2: TBGRAPixel; options: THueGradientOptions); overload;
    constructor Create(Color1,Color2: TExpandedPixel; options: THueGradientOptions); overload;
    constructor Create(Color1,Color2: THSLAPixel; options: THueGradientOptions); overload;
    constructor Create(AHue1,AHue2: Word; Saturation,Lightness: Word; options: THueGradientOptions); overload;
    function GetMonochrome: boolean; override;
  end;

  TGradientInterpolationFunction = function(t: single): single of object;

  { TBGRAMultiGradient }

  TBGRAMultiGradient = class(TBGRACustomGradient)
  private
    FColors: array of TBGRAPixel;
    FPositions: array of integer;
    FPositionsF: array of single;
    FEColors: array of TExpandedPixel;
    FCycle: Boolean;
    FInterpolationFunction: TGradientInterpolationFunction;
    procedure Init(Colors: array of TBGRAPixel; Positions0To1: array of single; AGammaCorrection, ACycle: boolean);
  public
    GammaCorrection: boolean;
    function CosineInterpolation(t: single): single;
    function HalfCosineInterpolation(t: single): single;
    constructor Create(Colors: array of TBGRAPixel; Positions0To1: array of single; AGammaCorrection: boolean; ACycle: boolean = false);
    function GetColorAt(position: integer): TBGRAPixel; override;
    function GetExpandedColorAt(position: integer): TExpandedPixel; override;
    function GetAverageColor: TBGRAPixel; override;
    function GetMonochrome: boolean; override;
    property InterpolationFunction: TGradientInterpolationFunction read FInterpolationFunction write FInterpolationFunction;
  end;

  { TBGRABufferedGradient }

  TBGRABufferedGradient = class(TBGRACustomGradient)
  protected
    FGradient: TBGRACustomGradient;
    FGradientOwned: boolean;
    FPadded: boolean;
    FAverageColorComputed: boolean;
    FAverageColorExpanded: TExpandedPixel;
    FMonochromeComputed: boolean;
    FMonochrome: boolean;
    FBufferSize, FBufferShift: integer;
    FColorTab: array of TBGRAPixel;
    FColorComputed: bitpacked array[0..65535] of boolean;
    FRepetition: TBGRAGradientRepetition;
  public
    constructor Create(AGradient: TBGRACustomGradient; AOwner: boolean; APadded: boolean;
      ABufferSize: integer);
    destructor Destroy; override;
    {** Returns the color at a given ''position''. The reference range is
        from 0 to 65535, however values beyond are possible as well }
    function GetColorAt(position: integer): TBGRAPixel; override;
    {** Returns the average color of the gradient }
    function GetAverageColor: TBGRAPixel; override;
    function GetAverageExpandedColor: TExpandedPixel; override;
    function GetMonochrome: boolean; override;
  end;

  TBGRAGradientScannerInternalScanNextFunc = function():single of object;
  TBGRAGradientScannerInternalScanAtFunc = function(const p: TPointF):single of object;

  { TBGRAGradientScanner }

  TBGRAGradientScanner = class(TBGRACustomScanner)
  protected
    FGradientType: TGradientType;
    FOrigin,FDir1,FDir2: TPointF;
    FRelativeFocal: TPointF;
    FRadius, FFocalRadius: single;
    FTransform, FHiddenTransform: TAffineMatrix;
    FSinus: Boolean;
    FGradient: TBGRACustomGradient;
    FGradientOwner: boolean;
    FFlipGradient: boolean;

    FMatrix: TAffineMatrix;
    FRepeatHoriz, FIsAverage: boolean;
    FAverageColor: TBGRAPixel;
    FAverageExpandedColor: TExpandedPixel;
    FScanNextFunc: TBGRAGradientScannerInternalScanNextFunc;
    FScanAtFunc: TBGRAGradientScannerInternalScanAtFunc;
    FGetGradientColor: TBGRAGradientGetColorAtFloatFunc;
    FGetGradientExpandedColor: TBGRAGradientGetExpandedColorAtFloatFunc;
    FFocalDistance: single;
    FFocalDirection, FFocalNormal: TPointF;
    FRadialDenominator, FRadialDeltaSign, maxW1, maxW2: single;

    FPosition: TPointF;
    FHorizColor: TBGRAPixel;
    FHorizExpandedColor: TExpandedPixel;

    procedure Init(AGradientType: TGradientType; AOrigin, d1: TPointF; ATransform: TAffineMatrix; Sinus: Boolean=False); overload;
    procedure Init(AGradientType: TGradientType; AOrigin, d1, d2: TPointF; ATransform: TAffineMatrix; Sinus: Boolean=False); overload;
    procedure Init(AOrigin: TPointF; ARadius: single; AFocal: TPointF; AFocalRadius: single; ATransform: TAffineMatrix; AHiddenTransform: TAffineMatrix); overload;

    procedure InitGradientType;
    procedure InitTransform;
    procedure InitGradient;

    function ComputeRadialFocal(const p: TPointF): single;

    function ScanNextLinear: single;
    function ScanNextReflected: single;
    function ScanNextDiamond: single;
    function ScanNextRadial: single;
    function ScanNextRadial2: single;
    function ScanNextRadialFocal: single;
    function ScanNextAngular: single;

    function ScanAtLinear(const p: TPointF): single;
    function ScanAtReflected(const p: TPointF): single;
    function ScanAtDiamond(const p: TPointF): single;
    function ScanAtRadial(const p: TPointF): single;
    function ScanAtRadial2(const p: TPointF): single;
    function ScanAtRadialFocal(const p: TPointF): single;
    function ScanAtAngular(const p: TPointF): single;

    function ScanNextInline: TBGRAPixel; inline;
    function ScanNextExpandedInline: TExpandedPixel; inline;
    procedure SetTransform(AValue: TAffineMatrix);
    procedure SetFlipGradient(AValue: boolean);
    procedure SetSinus(AValue: boolean);
    function GetGradientColor(a: single): TBGRAPixel;
    function GetGradientExpandedColor(a: single): TExpandedPixel;
    function GetGradientColorFlipped(a: single): TBGRAPixel;
    function GetGradientExpandedColorFlipped(a: single): TExpandedPixel;
    function GetGradientColorSinus(a: single): TBGRAPixel;
    function GetGradientExpandedColorSinus(a: single): TExpandedPixel;
    procedure UpdateGetGradientColorFunctions;
  public
    constructor Create(AGradientType: TGradientType; AOrigin, d1: TPointF); overload;
    constructor Create(AGradientType: TGradientType; AOrigin, d1, d2: TPointF); overload;
    constructor Create(AOrigin, d1, d2, AFocal: TPointF; ARadiusRatio: single = 1; AFocalRadiusRatio: single = 0); overload;
    constructor Create(AOrigin: TPointF; ARadius: single; AFocal: TPointF; AFocalRadius: single); overload;

    constructor Create(c1, c2: TBGRAPixel; AGradientType: TGradientType; AOrigin, d1: TPointF;
                       gammaColorCorrection: boolean = True; Sinus: Boolean=False); overload;
    constructor Create(c1, c2: TBGRAPixel; AGradientType: TGradientType; AOrigin, d1, d2: TPointF;
                       gammaColorCorrection: boolean = True; Sinus: Boolean=False); overload;

    constructor Create(gradient: TBGRACustomGradient; AGradientType: TGradientType; AOrigin, d1: TPointF;
                       Sinus: Boolean=False; AGradientOwner: Boolean=False); overload;
    constructor Create(gradient: TBGRACustomGradient; AGradientType: TGradientType; AOrigin, d1, d2: TPointF;
                       Sinus: Boolean=False; AGradientOwner: Boolean=False); overload;
    constructor Create(gradient: TBGRACustomGradient; AOrigin: TPointF; ARadius: single; AFocal: TPointF;
                       AFocalRadius: single; AGradientOwner: Boolean=False); overload;

    procedure SetGradient(c1,c2: TBGRAPixel; AGammaCorrection: boolean = true); overload;
    procedure SetGradient(AGradient: TBGRACustomGradient; AOwner: boolean); overload;
    destructor Destroy; override;
    procedure ScanMoveTo(X, Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanNextExpandedPixel: TExpandedPixel; override;
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    function ScanAtExpanded(X, Y: Single): TExpandedPixel; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
    procedure ScanSkipPixels(ACount: integer); override;
    function IsScanPutPixelsDefined: boolean; override;
    property Transform: TAffineMatrix read FTransform write SetTransform;
    property Gradient: TBGRACustomGradient read FGradient;
    property FlipGradient: boolean read FFlipGradient write SetFlipGradient;
    property Sinus: boolean Read FSinus write SetSinus;
  end;

  { TBGRAConstantScanner }

  TBGRAConstantScanner = class(TBGRAGradientScanner)
    constructor Create(c: TBGRAPixel);
  end;

  { TBGRARandomScanner }

  TBGRARandomScanner = class(TBGRACustomScanner)
  private
    FOpacity: byte;
    FGrayscale: boolean;
    FRandomBuffer, FRandomBufferCount: integer;
  public
    constructor Create(AGrayscale: Boolean; AOpacity: byte);
    function ScanAtInteger({%H-}X, {%H-}Y: integer): TBGRAPixel; override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt({%H-}X, {%H-}Y: Single): TBGRAPixel; override;
  end;

  { TBGRAGradientTriangleScanner }

  TBGRAGradientTriangleScanner= class(TBGRACustomScanner)
  protected
    FMatrix: TAffineMatrix;
    FColor1,FDiff2,FDiff3,FStep: TColorF;
    FCurColor: TColorF;
  public
    constructor Create(pt1,pt2,pt3: TPointF; c1,c2,c3: TBGRAPixel);
    procedure ScanMoveTo(X,Y: Integer); override;
    procedure ScanMoveToF(X,Y: Single);
    function ScanAt(X,Y: Single): TBGRAPixel; override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanNextExpandedPixel: TExpandedPixel; override;
    procedure ScanSkipPixels(ACount: integer); override;
  end;

  { TBGRASolidColorMaskScanner }

  TBGRASolidColorMaskScanner = class(TBGRACustomScanner)
  private
    FOffset: TPoint;
    FMask: IBGRAScanner;
    FSolidColor: TBGRAPixel;
  public
    constructor Create(AMask: IBGRAScanner; AOffset: TPoint; ASolidColor: TBGRAPixel);
    destructor Destroy; override;
    function IsScanPutPixelsDefined: boolean; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
    procedure ScanSkipPixels(ACount: integer); override;
    procedure ScanMoveTo(X,Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt(X,Y: Single): TBGRAPixel; override;
    property Color: TBGRAPixel read FSolidColor write FSolidColor;
  end;

  { TBGRATextureMaskScanner }

  TBGRATextureMaskScanner = class(TBGRACustomScanner)
  private
    FOffset: TPoint;
    FMask: IBGRAScanner;
    FTexture: IBGRAScanner;
    FTextureScanNext : TScanNextPixelFunction;
    FTextureScanAt : TScanAtFunction;
    FGlobalOpacity: Byte;
    FMemTex: packed array of TBGRAPixel;
  public
    constructor Create(AMask: IBGRAScanner; AOffset: TPoint; ATexture: IBGRAScanner; AGlobalOpacity: Byte = 255);
    destructor Destroy; override;
    function IsScanPutPixelsDefined: boolean; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
    procedure ScanSkipPixels(ACount: integer); override;
    procedure ScanMoveTo(X,Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt(X,Y: Single): TBGRAPixel; override;
  end;

  { TBGRAOpacityScanner }

  TBGRAOpacityScanner = class(TBGRACustomScanner)
  private
      FTexture: IBGRAScanner;
      FOwnedScanner: TBGRACustomScanner;
      FGlobalOpacity: Byte;
      FScanNext : TScanNextPixelFunction;
      FScanAt : TScanAtFunction;
      FMemTex: packed array of TBGRAPixel;
  public
    constructor Create(ATexture: IBGRAScanner; AGlobalOpacity: Byte = 255);
    constructor Create(ATexture: TBGRACustomScanner; AGlobalOpacity: Byte; AOwned: boolean);
    destructor Destroy; override;
    function IsScanPutPixelsDefined: boolean; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
    procedure ScanSkipPixels(ACount: integer); override;
    procedure ScanMoveTo(X,Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt(X,Y: Single): TBGRAPixel; override;
  end;

Implementation
End.

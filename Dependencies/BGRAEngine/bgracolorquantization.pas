// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAColorQuantization;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRAPalette, BGRABitmapTypes;

type
  TBGRAColorBox = class;
  TBGRAColorTree = class;
  TBGRAApproxPalette = class;
  TBiggestLeafMethod = (blMix, blApparentInterval, blWeight);

  { TDimensionMinMax }

  TDimensionMinMax = object
    Minimum: UInt32;
    Maximum: UInt32;
    function Size: UInt32;
    function Contains(AValue: UInt32): boolean;
    function PointLike: boolean;
    procedure SetAsPoint(AValue: UInt32);
    function GetCenter: UInt32;
    procedure GrowToInclude(AValue: UInt32);
  end;

  TColorDimension = (cdFast,cdRed,cdGreen,cdBlue,cdAlpha,cdRGB,cdRG,cdGB,cdRB,cdRInvG,cdGInvB,cdRInvB,cdRInvGB,cdGInvRB,cdBInvRG,
                     cdSaturation);
  TColorDimensions = set of TColorDimension;

  { TBGRAColorQuantizer }

  TBGRAColorQuantizer = class(TBGRACustomColorQuantizer)
  private
    FColors: ArrayOfWeightedColor;
    FPalette: TBGRAApproxPalette;
    FReductionColorCount: Integer;
    FReductionKeepContrast: boolean;
    FSeparateAlphaChannel: boolean;
    procedure Init(ABox: TBGRAColorBox);
    procedure NormalizeArrayOfColors(AColors: ArrayOfTBGRAPixel; ARedBounds, AGreenBounds, ABlueBounds, AAlphaBounds: TDimensionMinMax; AUniform: boolean); overload;
    procedure NormalizeArrayOfColors(AColors: ArrayOfTBGRAPixel; AColorBounds, AAlphaBounds: TDimensionMinMax); overload;
  protected
    function GetPalette: TBGRACustomApproxPalette; override;
    function GetSourceColor(AIndex: integer): TBGRAPixel; override;
    function GetSourceColorCount: Integer; override;
    function GetReductionColorCount: integer; override;
    procedure SetReductionColorCount(AValue: Integer); override;
  public
    constructor Create(APalette: TBGRACustomPalette; ASeparateAlphaChannel: boolean); override;
    constructor Create(ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption); override;
    constructor Create(APalette: TBGRACustomPalette; ASeparateAlphaChannel: boolean; AReductionColorCount: integer); override;
    constructor Create(ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption; AReductionColorCount: integer); override;
    destructor Destroy; override;
    procedure ApplyDitheringInplace(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; ABounds: TRect); override;
    function GetDitheredBitmap(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; ABounds: TRect): TBGRACustomBitmap; overload; override;
    function GetDitheredBitmapIndexedData(ABitDepth: integer; AByteOrder: TRawImageByteOrder; AAlgorithm: TDitheringAlgorithm;
      ABitmap: TBGRACustomBitmap; out AScanlineSize: PtrInt): Pointer; overload; override;
    procedure SaveBitmapToStream(AAlgorithm: TDitheringAlgorithm;
      ABitmap: TBGRACustomBitmap; AStream: TStream; AFormat: TBGRAImageFormat); override;
  end;

  { TBGRAApproxPalette }

  TBGRAApproxPalette = class(TBGRACustomApproxPalette)
  private
    FTree: TBGRAColorTree;
    FColors: ArrayOfWeightedColor;
  protected
    function GetCount: integer; override;
    function GetColorByIndex(AIndex: integer): TBGRAPixel; override;
    function GetWeightByIndex(AIndex: Integer): UInt32; override;
    procedure Init(const AColors: ArrayOfTBGRAPixel);
  public
    constructor Create(const AColors: ArrayOfTBGRAPixel); overload;
    constructor Create(const AColors: ArrayOfWeightedColor); overload;
    constructor Create(AOwnedSplitTree: TBGRAColorTree); overload;
    destructor Destroy; override;
    function ContainsColor(AValue: TBGRAPixel): boolean; override;
    function IndexOfColor(AValue: TBGRAPixel): integer; override;
    function FindNearestColor(AValue: TBGRAPixel): TBGRAPixel; override;
    function FindNearestColorIndex(AValue: TBGRAPixel): integer; override;
    function GetAsArrayOfColor: ArrayOfTBGRAPixel; override;
    function GetAsArrayOfWeightedColor: ArrayOfWeightedColor; override;
  end;

  { TBGRAApproxPaletteViaLargerPalette }

  TBGRAApproxPaletteViaLargerPalette = class(TBGRAApproxPalette)
  private
    FLarger: TBGRACustomApproxPalette;
    FLargerColors: array of record
      approxColor: TBGRAPixel;
      approxColorIndex: integer;
    end;
    FLargerOwned: boolean;
    FTransparentColorIndex: integer;
  protected
    function FindNearestLargerColorIndex(AValue: TBGRAPixel): integer; virtual;
    function SlowFindNearestColorIndex(AValue: TBGRAPixel): integer;
  public
    constructor Create(const AColors: ArrayOfTBGRAPixel; ALarger: TBGRACustomApproxPalette; ALargerOwned: boolean);
    destructor Destroy; override;
    function FindNearestColor(AValue: TBGRAPixel): TBGRAPixel; override;
    function FindNearestColorIndex(AValue: TBGRAPixel): integer; override;
    function GetAsArrayOfWeightedColor: ArrayOfWeightedColor; override;
  end;

  TIsChannelStrictlyGreaterFunc = TBGRAPixelComparer;
  TIsChannelGreaterThanOrEqualToValueFunc = function (p : PBGRAPixel; v: UInt32): boolean;

  TColorBoxBounds = array[TColorDimension] of TDimensionMinMax;

  { TBGRAColorBox }

  TBGRAColorBox = class
  private
    FBounds: TColorBoxBounds;
    FTotalWeight: UInt32;
    FColors: ArrayOfWeightedColor;
    FDimensions: TColorDimensions;
    FPureTransparentColorCount: integer;
    function GetApparentInterval(ADimension: TColorDimension): UInt32;
    function GetAverageColor: TBGRAPixel;
    function GetAverageColorOrMainColor: TBGRAPixel;
    function GetBounds(ADimension: TColorDimension): TDimensionMinMax;
    function GetColorCount(ACountPureTransparent: boolean): integer;
    function GetHasPureTransparentColor: boolean;
    function GetInferiorColor: TBGRAPixel;
    function GetLargestApparentDimension: TColorDimension;
    function GetLargestApparentInterval: UInt32;
    function GetPointLike: boolean;
    function GetSuperiorColor: TBGRAPixel;
    procedure Init(AColors: ArrayOfWeightedColor; AOwner: boolean);
    procedure SortBy(ADimension: TColorDimension);
    function GetMedianIndex(ADimension : TColorDimension; AMinValue, AMaxValue: UInt32): integer;
  public
    constructor Create(ADimensions: TColorDimensions; AColors: ArrayOfWeightedColor; AOwner: boolean); overload;
    constructor Create(ADimensions: TColorDimensions; const AColors: ArrayOfTBGRAPixel; AAlpha: TAlphaChannelPaletteOption = acFullChannelInPalette); overload;
    constructor Create(ADimensions: TColorDimensions; ABounds: TColorBoxBounds); overload;
    constructor Create(ADimensions: TColorDimensions; APalette: TBGRACustomPalette); overload;
    constructor Create(ADimensions: TColorDimensions; ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption); overload;
    constructor Create(ADimensions: TColorDimensions; AColors: PBGRAPixel; ANbPixels: integer; AAlpha: TAlphaChannelPaletteOption); overload;
    function BoundsContain(AColor: TBGRAPixel): boolean;
    function MedianCut(ADimension: TColorDimension; out SuperiorMiddle: UInt32): TBGRAColorBox;
    function Duplicate : TBGRAColorBox;
    property Bounds[ADimension: TColorDimension]: TDimensionMinMax read GetBounds;
    property ApparentInterval[AChannel: TColorDimension]: UInt32 read GetApparentInterval;
    property LargestApparentDimension: TColorDimension read GetLargestApparentDimension;
    property LargestApparentInterval: UInt32 read GetLargestApparentInterval;
    property PointLike: boolean read GetPointLike;
    property AverageColor: TBGRAPixel read GetAverageColor;
    property SuperiorColor: TBGRAPixel read GetSuperiorColor;
    property InferiorColor: TBGRAPixel read GetInferiorColor;
    property AverageColorOrMainColor: TBGRAPixel read GetAverageColorOrMainColor;
    function GetAsArrayOfColors(AIncludePureTransparent: boolean): ArrayOfTBGRAPixel;
    property TotalWeight: UInt32 read FTotalWeight;
    property ColorCount[ACountPureTransparent: boolean]: integer read GetColorCount;
    property HasPureTransparentColor: boolean read GetHasPureTransparentColor;
    property PureTransparentColorCount: integer read FPureTransparentColorCount;
  end;

  TBGRALeafColorMode = (lcAverage, lcCenter, lcExtremum, lcMix);

  { TBGRAColorTree }

  TBGRAColorTree = class
  private
    FLeaf: TBGRAColorBox;
    FIsLeaf: boolean;
    FLargestApparentInterval: integer;
    FWeight: UInt32;

    FLeafColor: TBGRAPixel;
    FLeafColorIndex: integer;
    FLeafColorComputed: boolean;
    FMinBorder, FMaxBorder: array[TColorDimension] of boolean;
    FCenterColor: TBGRAPixel;
    FAverageColor: TBGRAPixel;

    FPureTransparentColorCount: integer;
    FPureTransparentColorIndex: integer;
    FDimension: TColorDimension;
    FPixelValueComparer: TIsChannelGreaterThanOrEqualToValueFunc;
    FSuperiorMiddle: UInt32;
    FInferiorBranch, FSuperiorBranch: TBGRAColorTree;
    function GetApproximatedColorCount: integer;
    function GetHasPureTransparentColor: boolean;
    function GetLeafCount: integer;
    procedure Init(ALeaf: TBGRAColorBox; AOwned: boolean);
    procedure InternalComputeLeavesColor(ALeafColor: TBGRALeafColorMode; var AStartIndex: integer);
    procedure CheckColorComputed;
  public
    constructor Create(ABox: TBGRAColorBox; AOwned: boolean); overload;
    constructor Create(ADimensions: TColorDimensions; APalette: TBGRACustomPalette); overload;
    constructor Create(ADimensions: TColorDimensions; ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption); overload;
    destructor Destroy; override;
    procedure FreeLeaves;
    function FindBiggestLeaf(AMethod: TBiggestLeafMethod): TBGRAColorTree;
    property LargestApparentInterval: integer read FLargestApparentInterval;
    property Weight: UInt32 read FWeight;
    property IsLeaf: boolean read FIsLeaf;
    function TrySplitLeaf: boolean;
    procedure ComputeLeavesColor(ALeafColor: TBGRALeafColorMode);
    function ApproximateColor(AColor: TBGRAPixel): TBGRAPixel;
    function ApproximateColorIndex(AColor: TBGRAPixel): integer;
    function GetAsArrayOfApproximatedColors: ArrayOfTBGRAPixel;
    function GetAsArrayOfWeightedColors: ArrayOfWeightedColor;
    procedure SplitIntoPalette(ACount: integer; AMethod: TBiggestLeafMethod;
      ALeafColor: TBGRALeafColorMode);
    function SplitIntoPaletteWithSubPalette(ACount: integer; AMethod: TBiggestLeafMethod;
      ALeafColor: TBGRALeafColorMode; ASubPaletteCount: integer): ArrayOfTBGRAPixel;
    property LeafCount: integer read GetLeafCount;
    property ApproximatedColorCount: integer read GetApproximatedColorCount;
    property HasPureTransparentColor: boolean read GetHasPureTransparentColor;
    property PureTransparentColorCount: integer read FPureTransparentColorCount;
  end;

function GetPixelStrictComparer(ADimension: TColorDimension): TIsChannelStrictlyGreaterFunc;
function GetPixelValueComparer(ADimension: TColorDimension): TIsChannelGreaterThanOrEqualToValueFunc;
function BGRAColorCount(ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption): integer;

const AllColorDimensions = [cdRed,cdGreen,cdBlue,cdAlpha,cdRGB,cdRG,cdGB,cdRB,cdRInvG,cdGInvB,cdRInvB,cdRInvGB,cdGInvRB,cdBInvRG,
                            cdSaturation];

Implementation
End.

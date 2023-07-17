// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAPalette;

{$mode objfpc}{$H+}
{$WARN 5093 off : function result variable of a managed type does not seem to be initialized}
{$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
interface

uses
  BGRAClasses, SysUtils, Avl_Tree, BGRABitmapTypes, FPimage;

const
  MaxLastAddedColors = 10;

type
  TBGRAPaletteFormat = integer;

const
  palUnknown : TBGRAPaletteFormat = 0;
  palPaintDotNet : TBGRAPaletteFormat = 1;
  palGimp : TBGRAPaletteFormat = 2;
  palAdobeSwatchExchange : TBGRAPaletteFormat = 3;
  palKOffice : TBGRAPaletteFormat = 4;
  palJascPSP : TBGRAPaletteFormat = 5;
  palCustom : TBGRAPaletteFormat = 100;

type
  TBGRAIndexedPaletteEntry = packed record
    Color: TBGRAPixel;
    Index: UInt32;
  end;
  PBGRAIndexedPaletteEntry = ^TBGRAIndexedPaletteEntry;
  TBGRAWeightedPaletteEntry = packed record
    Color: TBGRAPixel;
    Weight: UInt32;
  end;
  PBGRAWeightedPaletteEntry = ^TBGRAWeightedPaletteEntry;
  ArrayOfWeightedColor = array of TBGRAWeightedPaletteEntry;

  TBGRAPixelComparer = function (p1,p2 : PBGRAPixel): boolean;

  { TBGRACustomPalette }

  TBGRACustomPalette = class
  private
    function GetDominantColor: TBGRAPixel;
  protected
    function GetCount: integer; virtual; abstract;
    function GetColorByIndex(AIndex: integer): TBGRAPixel; virtual; abstract;
  public
    function ContainsColor(AValue: TBGRAPixel): boolean; virtual; abstract;
    function IndexOfColor(AValue: TBGRAPixel): integer; virtual; abstract;
    function GetAsArrayOfColor: ArrayOfTBGRAPixel; virtual; abstract;
    function GetAsArrayOfWeightedColor: ArrayOfWeightedColor; virtual; abstract;
    procedure AssignTo(AImage: TFPCustomImage); overload;
    procedure AssignTo(APalette: TFPPalette); overload;
    property DominantColor: TBGRAPixel read GetDominantColor;
    property Count: integer read GetCount;
    property Color[AIndex: integer]: TBGRAPixel read GetColorByIndex;
  end;

type
  { TBGRAAvgLvlPalette }

  TBGRAAvgLvlPalette = class(TBGRACustomPalette)
  protected
    FTree: TAVLTree;
    FArray: array of PBGRAPixel;
    FLastAddedColors: packed array[0..MaxLastAddedColors-1] of PBGRAPixel;
    FLastAddedColorCount: integer;
    function GetCount: integer; override;
    function GetColorByIndex(AIndex: integer): TBGRAPixel; override;
    procedure FreeEntry(AEntry: PBGRAPixel); virtual; abstract;
    procedure NeedArray; virtual;
    procedure ClearArray; virtual;
    procedure AddLastColor(AColor: PBGRAPixel);
    function GetLastColor(AValue: TBGRAPixel): PBGRAPixel;
    procedure ClearLastColors;
  public
    constructor Create; overload;
    function ContainsColor(AValue: TBGRAPixel): boolean; override;
    function IndexOfColor(AValue: TBGRAPixel): integer; override;
    procedure Clear; virtual;
    destructor Destroy; override;
    function GetAsArrayOfColor: ArrayOfTBGRAPixel; override;
    function GetAsArrayOfWeightedColor: ArrayOfWeightedColor; override;
  end;

  { TBGRAPalette }

  TBGRAPalette = class(TBGRAAvgLvlPalette)
  protected
    function CreateEntry(AColor: TBGRAPixel): PBGRAPixel; virtual;
    procedure FreeEntry(AEntry: PBGRAPixel); override;
    procedure IncludePixel(PPixel: PBGRAPixel); virtual;
    procedure ExceptionUnknownPaletteFormat;
    procedure ExceptionInvalidPaletteFormat;
  public
    constructor Create(ABitmap: TBGRACustomBitmap); overload; virtual;
    constructor Create(APalette: TBGRACustomPalette); overload; virtual;
    constructor Create(AColors: ArrayOfTBGRAPixel); overload; virtual;
    constructor Create(AColors: ArrayOfWeightedColor); overload; virtual;
    function AddColor(AValue: TBGRAPixel): boolean; virtual;
    procedure AddColors(ABitmap: TBGRACustomBitmap); overload; virtual;
    procedure AddColors(APalette: TBGRACustomPalette); overload; virtual;
    function RemoveColor(AValue: TBGRAPixel): boolean; virtual;
    procedure LoadFromFile(AFilenameUTF8: string); virtual;
    procedure LoadFromStream(AStream: TStream; AFormat: TBGRAPaletteFormat); virtual;
    procedure LoadFromResource(AFilename: string; AFormat: TBGRAPaletteFormat);
    procedure SaveToFile(AFilenameUTF8: string); virtual;
    procedure SaveToStream(AStream: TStream; AFormat: TBGRAPaletteFormat); virtual;
    function DetectPaletteFormat(AStream: TStream): TBGRAPaletteFormat; overload; virtual;
    function DetectPaletteFormat(AFilenameUTF8: string): TBGRAPaletteFormat; overload;
    function SuggestPaletteFormat(AFilenameUTF8: string): TBGRAPaletteFormat; virtual;
  end;

  { TBGRAIndexedPalette }

  TBGRAIndexedPalette = class(TBGRAPalette)
  private
    FCurrentIndex: UInt32;
  protected
    procedure NeedArray; override;
    function CreateEntry(AColor: TBGRAPixel): PBGRAPixel; override;
    procedure FreeEntry(AEntry: PBGRAPixel); override;
  public
    function RemoveColor({%H-}AValue: TBGRAPixel): boolean; override;
    function IndexOfColor(AValue: TBGRAPixel): integer; override;
    procedure Clear; override;
  end;

  { TBGRAWeightedPalette }

  TBGRAWeightedPalette = class(TBGRAPalette)
  private
  protected
    function CreateEntry(AColor: TBGRAPixel): PBGRAPixel; override;
    procedure FreeEntry(AEntry: PBGRAPixel); override;
    function GetWeightByIndex(AIndex: Integer): UInt32; virtual;
    procedure IncludePixel(PPixel: PBGRAPixel); override;
  public
    constructor Create(AColors: ArrayOfWeightedColor); override;
    function GetAsArrayOfWeightedColor: ArrayOfWeightedColor; override;
    function IncColor(AValue: TBGRAPixel; out NewWeight: UInt32): boolean;
    function DecColor(AValue: TBGRAPixel; out NewWeight: UInt32): boolean;
    property Weight[AIndex: Integer]: UInt32 read GetWeightByIndex;
  end;

  { TBGRAReferencePalette }

  TBGRAReferencePalette = class(TBGRAAvgLvlPalette)
  protected
    procedure FreeEntry({%H-}AEntry: PBGRAPixel); override;
  public
    function AddColor(AValue: PBGRAPixel): boolean;
    function RemoveColor(AValue: PBGRAPixel): boolean;
  end;

  { TBGRACustomApproxPalette }

  TBGRACustomApproxPalette = class(TBGRACustomPalette)
  private
    function FindNearestColorIgnoreAlpha(AValue: TBGRAPixel): TBGRAPixel; inline;
    function FindNearestColorIndexIgnoreAlpha(AValue: TBGRAPixel): integer; inline;
  protected
    function GetWeightByIndex({%H-}AIndex: Integer): UInt32; virtual;
  public
    function FindNearestColor(AValue: TBGRAPixel; AIgnoreAlpha: boolean): TBGRAPixel; overload;
    function FindNearestColor(AValue: TBGRAPixel): TBGRAPixel; overload; virtual; abstract;
    function FindNearestColorIndex(AValue: TBGRAPixel; AIgnoreAlpha: boolean): integer; overload;
    function FindNearestColorIndex(AValue: TBGRAPixel): integer; overload; virtual; abstract;
    property Weight[AIndex: Integer]: UInt32 read GetWeightByIndex;
  end;

  { TBGRA16BitPalette }

  TBGRA16BitPalette = class(TBGRACustomApproxPalette)
  protected
    function GetCount: integer; override;
    function GetColorByIndex(AIndex: integer): TBGRAPixel; override;
  public
    function ContainsColor(AValue: TBGRAPixel): boolean; override;
    function IndexOfColor(AValue: TBGRAPixel): integer; override;
    function GetAsArrayOfColor: ArrayOfTBGRAPixel; override;
    function GetAsArrayOfWeightedColor: ArrayOfWeightedColor; override;
    function FindNearestColor(AValue: TBGRAPixel): TBGRAPixel; override;
    function FindNearestColorIndex(AValue: TBGRAPixel): integer; override;
  end;

  { TBGRACustomColorQuantizer }

  TBGRACustomColorQuantizer = class
  protected
    function GetDominantColor: TBGRAPixel; virtual;
    function GetPalette: TBGRACustomApproxPalette; virtual; abstract;
    function GetSourceColor(AIndex: integer): TBGRAPixel; virtual; abstract;
    function GetSourceColorCount: Integer; virtual; abstract;
    function GetReductionColorCount: integer; virtual; abstract;
    procedure SetReductionColorCount(AValue: Integer); virtual; abstract;
  public
    constructor Create(APalette: TBGRACustomPalette; ASeparateAlphaChannel: boolean); overload; virtual; abstract;
    constructor Create(AColors: array of TBGRAPixel; ASeparateAlphaChannel: boolean); overload;
    constructor Create(ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption); overload; virtual; abstract;
    constructor Create(APalette: TBGRACustomPalette; ASeparateAlphaChannel: boolean; AReductionColorCount: integer); overload; virtual; abstract;
    constructor Create(AColors: array of TBGRAPixel; ASeparateAlphaChannel: boolean; AReductionColorCount: integer); overload;
    constructor Create(ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption; AReductionColorCount: integer); overload; virtual; abstract;
    procedure ApplyDitheringInplace(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; ABounds: TRect); overload; virtual; abstract;
    procedure ApplyDitheringInplace(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap); overload;
    function GetDitheredBitmap(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; ABounds: TRect): TBGRACustomBitmap; overload; virtual; abstract;
    function GetDitheredBitmap(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap): TBGRACustomBitmap; overload;
    procedure SaveBitmapToFile(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; AFilenameUTF8: string); overload;
    procedure SaveBitmapToFile(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; AFilenameUTF8: string; AFormat: TBGRAImageFormat); overload;
    procedure SaveBitmapToStream(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; AStream: TStream; AFormat: TBGRAImageFormat); virtual; abstract;
    function GetDitheredBitmapIndexedData(ABitDepth: integer; AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; out AScanlineSize: PtrInt): Pointer; overload;
    function GetDitheredBitmapIndexedData(ABitDepth: integer; AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap): Pointer; overload;
    function GetDitheredBitmapIndexedData(ABitDepth: integer; AByteOrder: TRawImageByteOrder; AAlgorithm: TDitheringAlgorithm;
      ABitmap: TBGRACustomBitmap; out AScanlineSize: PtrInt): Pointer; overload; virtual; abstract;
    property SourceColorCount: Integer read GetSourceColorCount;
    property SourceColor[AIndex: integer]: TBGRAPixel read GetSourceColor;
    property ReductionColorCount: Integer read GetReductionColorCount write SetReductionColorCount;
    property ReducedPalette: TBGRACustomApproxPalette read GetPalette;
    property DominantColor: TBGRAPixel read GetDominantColor;
  end;

  TBGRAColorQuantizerAny = class of TBGRACustomColorQuantizer;

var
  BGRAColorQuantizerFactory: TBGRAColorQuantizerAny;

function BGRARequiredBitDepth(ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption): integer; overload;
function BGRARequiredBitDepth(APalette: TBGRACustomPalette): integer; overload;

type
  TPaletteReaderProc = function(APalette: TBGRAPalette; AStream: TStream): boolean;
  TPaletteWriterProc = procedure(APalette: TBGRAPalette; AStream: TStream);
  TCheckPaletteFormatProc = function(ABuf256: string): boolean;

procedure BGRARegisterPaletteFormat(AFormatIndex: TBGRAPaletteFormat; AExtension: string; ADescription: string;
  AReadProc: TPaletteReaderProc; AWriteProc: TPaletteWriterProc; ACheckFormatProc: TCheckPaletteFormatProc);
function BGRARegisteredPaletteFormatFilter(AAllSupportedDescription: string) : string;

procedure ArrayOfWeightedColor_QuickSort(AColors: ArrayOfWeightedColor; AMinIndex,
  AMaxIndex: Int32or64; AComparer: TBGRAPixelComparer = nil);

procedure ArrayOfWeightedColor_InsertionSort(AColors: ArrayOfWeightedColor; AMinIndex,
  AMaxIndex: Int32or64; AComparer: TBGRAPixelComparer = nil);

procedure ArrayOfTBGRAPixel_QuickSort(AColors: ArrayOfTBGRAPixel; AMinIndex,
  AMaxIndex: Int32or64; AComparer: TBGRAPixelComparer = nil);

procedure ArrayOfTBGRAPixel_InsertionSort(AColors: ArrayOfTBGRAPixel; AMinIndex,
  AMaxIndex: Int32or64; AComparer: TBGRAPixelComparer = nil);

Implementation
End.

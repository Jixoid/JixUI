// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRADithering;

{$mode objfpc}{$H+}
{$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
interface

uses
  BGRAClasses, SysUtils, BGRAFilterType, BGRAPalette, BGRABitmapTypes;

type
  TOutputPixelProc = procedure(X,Y: Int32or64; AColorIndex: Int32or64; AColor: TBGRAPixel) of object;

  { TDitheringTask }

  TDitheringTask = class(TFilterTask)
  protected
    FBounds: TRect;
    FIgnoreAlpha: boolean;
    FPalette: TBGRACustomApproxPalette;
    FCurrentOutputScanline: PBGRAPixel;
    FCurrentOutputY: Int32or64;
    FOutputPixel : TOutputPixelProc;
    FDrawMode: TDrawMode;
    procedure UpdateCurrentLine(Y: Int32or64); inline;
    procedure OutputPixel(X,Y: Int32or64; {%H-}AColorIndex: Int32or64; AColor: TBGRAPixel); virtual;
    procedure OutputPixelSet(X,Y: Int32or64; {%H-}AColorIndex: Int32or64; AColor: TBGRAPixel); virtual;
    procedure OutputPixelSetExceptTransparent(X,Y: Int32or64; {%H-}AColorIndex: Int32or64; AColor: TBGRAPixel); virtual;
    procedure OutputPixelLinearBlend(X,Y: Int32or64; {%H-}AColorIndex: Int32or64; AColor: TBGRAPixel); virtual;
    procedure OutputPixelDraw(X,Y: Int32or64; {%H-}AColorIndex: Int32or64; AColor: TBGRAPixel); virtual;
    procedure OutputPixelXor(X,Y: Int32or64; {%H-}AColorIndex: Int32or64; AColor: TBGRAPixel); virtual;
    procedure ApproximateColor(const AColor: TBGRAPixel; out AApproxColor: TBGRAPixel; out AIndex: integer);
    procedure SetDrawMode(AValue: TDrawMode);
    procedure UpdateOutputPixel;
  public
    constructor Create(ASource: IBGRAScanner; APalette: TBGRACustomApproxPalette; ADestination: TBGRACustomBitmap; AIgnoreAlpha: boolean; ABounds: TRect); overload;
    constructor Create(bmp: TBGRACustomBitmap; APalette: TBGRACustomApproxPalette; AInPlace: boolean; AIgnoreAlpha: boolean; ABounds: TRect); overload;
    constructor Create(bmp: TBGRACustomBitmap; APalette: TBGRACustomApproxPalette; AInPlace: boolean; AIgnoreAlpha: boolean); overload;
    property OnOutputPixel: TOutputPixelProc read FOutputPixel write FOutputPixel;
    property DrawMode: TDrawMode read FDrawMode write SetDrawMode;
  end;

  { TNearestColorTask }

  TNearestColorTask = class(TDitheringTask)
  protected
    procedure DoExecute; override;
  end;

  { TFloydSteinbergDitheringTask }

  TFloydSteinbergDitheringTask = class(TDitheringTask)
  protected
    procedure DoExecute; override;
  end;

  { TDitheringToIndexedImage }

  TDitheringToIndexedImage = class
  protected
    FBitOrder: TRawImageBitOrder;
    FByteOrder: TRawImageByteOrder;
    FBitsPerPixel: integer;
    FLineOrder: TRawImageLineOrder;
    FPalette: TBGRACustomApproxPalette;
    FIgnoreAlpha: boolean;
    FTransparentColorIndex: Int32or64;

    //following variables are used during dithering
    FCurrentScanlineSize: PtrInt;
    FCurrentData: PByte;
    FCurrentOutputY: Int32or64;
    FCurrentOutputScanline: PByte;
    FCurrentBitOrderMask: Int32or64;
    FCurrentMaxY: Int32or64;

    procedure SetPalette(AValue: TBGRACustomApproxPalette);
    procedure SetIgnoreAlpha(AValue: boolean);
    procedure SetLineOrder(AValue: TRawImageLineOrder);
    procedure SetBitOrder(AValue: TRawImageBitOrder); virtual;
    procedure SetBitsPerPixel(AValue: integer); virtual;
    procedure SetByteOrder(AValue: TRawImageByteOrder); virtual;
    procedure OutputPixelSubByte(X,Y: Int32or64; AColorIndex: Int32or64; {%H-}AColor: TBGRAPixel); virtual;
    procedure OutputPixelFullByte(X,Y: Int32or64; AColorIndex: Int32or64; {%H-}AColor: TBGRAPixel); virtual;
    function GetScanline(Y: Int32or64): Pointer; virtual;
    function GetTransparentColorIndex: integer;
    procedure SetTransparentColorIndex(AValue: integer);
  public
    constructor Create(APalette: TBGRACustomApproxPalette; AIgnoreAlpha: boolean; ABitsPerPixelForIndices: integer); overload; //use platform byte order
    constructor Create(APalette: TBGRACustomApproxPalette; AIgnoreAlpha: boolean; ABitsPerPixelForIndices: integer; AByteOrder: TRawImageByteOrder); overload; //maybe necessary if larger than 8 bits per pixel

    function DitherImage(AAlgorithm: TDitheringAlgorithm; AImage: TBGRACustomBitmap): Pointer; overload; //use minimum scanline size
    function DitherImage(AAlgorithm: TDitheringAlgorithm; AImage: TBGRACustomBitmap; AScanlineSize: PtrInt): Pointer; overload;
    procedure DitherImageTo(AAlgorithm: TDitheringAlgorithm; AImage: TBGRACustomBitmap; AData: Pointer); overload; //use minimum scanline size
    procedure DitherImageTo(AAlgorithm: TDitheringAlgorithm; AImage: TBGRACustomBitmap; AData: Pointer; AScanlineSize: PtrInt); overload;
    function ComputeMinimumScanlineSize(AWidthInPixels: integer): PtrInt;
    function AllocateSpaceForIndexedData(AImage: TBGRACustomBitmap; AScanlineSize: PtrInt): pointer;

    //optional customization of format
    property BitsPerPixel: integer read FBitsPerPixel write SetBitsPerPixel;
    property BitOrder: TRawImageBitOrder read FBitOrder write SetBitOrder;
    property ByteOrder: TRawImageByteOrder read FByteOrder write SetByteOrder;
    property LineOrder: TRawImageLineOrder read FLineOrder write SetLineOrder;

    property Palette: TBGRACustomApproxPalette read FPalette write SetPalette;
    property IgnoreAlpha: boolean read FIgnoreAlpha write SetIgnoreAlpha;

    //when there is no transparent color in the palette, or that IgnoreAlpha is set to True,
    //this allows to define the index for the fully transparent color
    property DefaultTransparentColorIndex: integer read GetTransparentColorIndex write SetTransparentColorIndex;
  end;

function CreateDitheringTask(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; APalette: TBGRACustomApproxPalette;
  AIgnoreAlpha: boolean): TDitheringTask; overload;
function CreateDitheringTask(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; APalette: TBGRACustomApproxPalette;
  AIgnoreAlpha: boolean; ABounds: TRect): TDitheringTask; overload;
function CreateDitheringTask(AAlgorithm: TDitheringAlgorithm; ASource: IBGRAScanner; ADestination: TBGRACustomBitmap; ABounds: TRect): TDitheringTask; overload;
function CreateDitheringTask(AAlgorithm: TDitheringAlgorithm; ASource: IBGRAScanner; ADestination: TBGRACustomBitmap; APalette: TBGRACustomApproxPalette;
    AIgnoreAlpha: boolean; ABounds: TRect): TDitheringTask; overload;

function DitherImageTo16Bit(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap): TBGRACustomBitmap;

Implementation
End.

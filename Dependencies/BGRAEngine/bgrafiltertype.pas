// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAFilterType;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, BGRABitmapTypes;

const
    FilterScannerChunkSize = 16;

type
  TCheckShouldStopFunc = function(ACurrentY: integer) : boolean of object;

  { TFilterTask }

  TFilterTask = class
  private
    FCheckShouldStop: TCheckShouldStopFunc;
    FScanOffset: TPoint;
    procedure SetDestination(AValue: TBGRACustomBitmap);
    function GetInplace: boolean;
    procedure SetInplace(AValue: boolean);
  protected
    FDestination: TBGRACustomBitmap;
    FSource: TBGRACustomBitmap;
    FSourceScanner: IBGRAScanner;
    FCurrentY: integer;
    function GetShouldStop(ACurrentY: integer): boolean;
    procedure DoExecute; virtual; abstract;
    function RequestSourceScanLine(X,Y,Count: Integer): PBGRAPixel;
    procedure ReleaseSourceScanLine(P: PBGRAPixel);
    function RequestSourceExpandedScanLine(X,Y,Count: Integer): PExpandedPixel;
    procedure ReleaseSourceExpandedScanLine(P: PExpandedPixel);
    procedure SetSource(ABitmap: TBGRACustomBitmap); overload;
    procedure SetSource(AScanner: IBGRAScanner); overload;
  public
    function Execute: TBGRACustomBitmap;
    property Destination: TBGRACustomBitmap read FDestination write SetDestination;
    property CheckShouldStop: TCheckShouldStopFunc read FCheckShouldStop write FCheckShouldStop;
    property CurrentY: integer read FCurrentY;
    property ScanOffset: TPoint read FScanOffset write FScanOffset;
    property Inplace: boolean read GetInplace write SetInplace;
  end;

  { TBGRAFilterScanner }

  TBGRAFilterScanner = class(TBGRACustomScanner)
  private
    FAllowDirectRead: boolean;
    FCurX,FCurY: integer;
    FSource: IBGRAScanner;
    FOffset: TPoint;
    FVariablePixelBuffer: TBGRAPixelBuffer;
    FOutputBuffer: packed array[0..FilterScannerChunkSize-1] of TBGRAPixel;
    FOutputBufferPos: integer;
  public
    constructor Create(ASource: IBGRAScanner; AOffset: TPoint);
    procedure ComputeFilter(ASource: IBGRAScanner; X,Y: Integer; ADest: PBGRAPixel; ACount: integer); virtual; abstract;
    function ScanAtInteger(X,Y: integer): TBGRAPixel; override;
    procedure ScanMoveTo(X,Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
    procedure ScanSkipPixels(ACount: integer); override;
    function IsScanPutPixelsDefined: boolean; override;
    function ScanAt(X,Y: Single): TBGRAPixel; override;
    property Source: IBGRAScanner read FSource;
    property Offset: TPoint read FOffset;
    property AllowDirectRead: boolean read FAllowDirectRead write FAllowDirectRead;
  end;

  { TBGRAFilterScannerPixelwise }

  TBGRAFilterScannerPixelwise = class(TBGRAFilterScanner)
  private
    FBuffer: TBGRAPixelBuffer;
    FGammaCorrection: boolean;
  protected
    procedure DoComputeFilterAt(ASource: PBGRAPixel; ADest: PBGRAPixel;
            ACount: integer; AGammaCorrection: boolean); virtual;
  public
    constructor Create(ASource: IBGRAScanner; AOffset: TPoint; AGammaCorrection: boolean = true);
    procedure ComputeFilter(ASource: IBGRAScanner; X, Y: Integer; ADest: PBGRAPixel;
      ACount: integer); override;
    class procedure ComputeFilterAt(ASource: PBGRAPixel; ADest: PBGRAPixel;
      ACount: integer; AGammaCorrection: boolean); virtual; abstract;
    class procedure ComputeFilterInplace(ABitmap: TBGRACustomBitmap; ABounds: TRect;
      AGammaCorrection: boolean); virtual;
    property GammaCorrection: boolean read FGammaCorrection write FGammaCorrection;
  end;

  { TBGRAFilterScannerMultipixel }

  TBGRAFilterScannerMultipixel = class(TBGRAFilterScanner)
  private
    FSourceBounds: TRect;
    FKernelWidth,FKernelHeight: integer;
    FCurBufferY: integer;
    FCurBufferYDefined: boolean;
    FBuffers: array of TBGRAPixelBuffer;
    FPBuffers: array of PBGRAPixel;
  protected
    procedure DoComputeFilter(BufferX: Integer; const Buffers: array of PBGRAPixel;
      BufferWidth: integer; ADest: PBGRAPixel; ACount: integer); virtual; abstract;
    procedure LoadBuffer(ASource: IBGRAScanner; X,Y: Integer; BufferIndex: Integer; ACount: integer); virtual;
  public
    constructor Create(ASource: IBGRAScanner; ASourceBounds: TRect; AOffset: TPoint;
      AKernelWidth,AKernelHeight: Integer);
    procedure ComputeFilter(ASource: IBGRAScanner; X, Y: Integer; ADest: PBGRAPixel;
      ACount: integer); override;
    property KernelWidth: integer read FKernelWidth;
    property KernelHeight: integer read FKernelHeight;
    property SourceBounds: TRect read FSourceBounds;
  end;

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAAnimatedGif;

{$mode objfpc}{$H+}
{$i bgrabitmap.map.inc}

interface

uses
  BGRAClasses, SysUtils, BGRAGraphics, FPImage, BGRABitmap, BGRABitmapTypes,
  BGRAPalette, BGRAGifFormat{$IFDEF BGRABITMAP_USE_LCL}, ExtCtrls{$ENDIF};

type
  TDisposeMode = BGRAGifFormat.TDisposeMode;
  TGifSubImage = BGRAGifFormat.TGifSubImage;
  TGifSubImageArray = BGRAGifFormat.TGifSubImageArray;

  //how to deal with the background under the GIF animation
  TGifBackgroundMode = (gbmSimplePaint, gbmEraseBackground,
    gbmSaveBackgroundOnce, gbmUpdateBackgroundContinuously);

  { TBGRAAnimatedGif }

  TBGRAAnimatedGif = class(TGraphic)
  private
    FAspectRatio: single;
    FWidth, FHeight:  integer;
    FBackgroundColor: TColor;

    FPrevDate: TDateTime;
    FPaused:   boolean;
    FTimeAccumulator: double;
    FCurrentImage, FWantedImage: integer;
    FTotalAnimationTime: int64;
    FPreviousDisposeMode: TDisposeMode;

    FBackgroundImage, FPreviousVirtualScreen, FStretchedVirtualScreen,
    FInternalVirtualScreen, FRestoreImage: TBGRABitmap;
    FImageChanged: boolean;

    {$IFDEF BGRABITMAP_USE_LCL}
    FTimer: TTimer;
    {$ENDIF}

    procedure CheckFrameIndex(AIndex: integer);
    function GetAverageDelayMs: integer;
    function GetCount: integer;
    function GetFrameDelayMs(AIndex: integer): integer;
    function GetFrameDisposeMode(AIndex: integer): TDisposeMode;
    function GetFrameHasLocalPalette(AIndex: integer): boolean;
    function GetFrameImage(AIndex: integer): TBGRABitmap;
    function GetFrameImagePos(AIndex: integer): TPoint;
    function GetTimeUntilNextImage: integer;
    {$IFDEF BGRABITMAP_USE_LCL}
    procedure OnTimer(Sender: TObject);
    {$ENDIF}
    procedure Render(StretchWidth, StretchHeight: integer);
    procedure SetAspectRatio(AValue: single);
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetFrameDelayMs(AIndex: integer; AValue: integer);
    procedure SetFrameDisposeMode(AIndex: integer; AValue: TDisposeMode);
    procedure SetFrameHasLocalPalette(AIndex: integer; AValue: boolean);
    procedure SetFrameImage(AIndex: integer; AValue: TBGRABitmap);
    procedure SetFrameImagePos(AIndex: integer; AValue: TPoint);
    procedure UpdateSimple(Canvas: TCanvas; ARect: TRect;
      DrawOnlyIfChanged: boolean = True);
    procedure UpdateEraseBackground(Canvas: TCanvas; ARect: TRect;
      DrawOnlyIfChanged: boolean = True);
    procedure Init;
    function GetBitmap: TBitmap;
    function GetMemBitmap: TBGRABitmap;
    procedure SaveBackgroundOnce(Canvas: TCanvas; ARect: TRect);
    procedure SetCurrentImage(Index: integer);

  protected
    FImages: TGifSubImageArray;
    FDestroying: boolean;

    {TGraphic}
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetEmpty: boolean; override;
    function GetHeight: integer; override;
    function GetTransparent: boolean; override;
    function GetWidth: integer; override;
    procedure SetHeight({%H-}Value: integer); override;
    procedure SetTransparent({%H-}Value: boolean); override;
    procedure SetWidth({%H-}Value: integer); override;
    procedure ClearViewer; virtual;
    procedure Changed(Sender: TObject); override;
    procedure EnsureNextFrameRec(AIndex: integer);
    procedure AssignTo(Dest: TPersistent); override;

  public
    EraseColor:     TColor;
    BackgroundMode: TGifBackgroundMode;
    LoopCount:      Word;
    LoopDone:       Integer;

    constructor Create(filenameUTF8: string); overload;
    constructor Create(stream: TStream); overload;
    constructor Create(stream: TStream; AMaxImageCount: integer); overload;
    constructor Create; overload; override;
    function Duplicate: TBGRAAnimatedGif;
    procedure Assign(ASource: TPersistent); override;
    function AddFrame(AImage: TFPCustomImage; X,Y: integer; ADelayMs: integer;
      ADisposeMode: TDisposeMode = dmErase; AHasLocalPalette: boolean = false) : integer;
    procedure InsertFrame(AIndex: integer; AImage: TFPCustomImage; X,Y: integer; ADelayMs: integer;
      ADisposeMode: TDisposeMode = dmErase; AHasLocalPalette: boolean = false);
    procedure DeleteFrame(AIndex: integer; AEnsureNextFrameDoesNotChange: boolean);

    //add a frame that replaces completely the previous one
    function AddFullFrame(AImage: TFPCustomImage; ADelayMs: integer;
                          AHasLocalPalette: boolean = true): integer;
    procedure InsertFullFrame(AIndex: integer;
                              AImage: TFPCustomImage; ADelayMs: integer;
                              AHasLocalPalette: boolean = true);
    procedure ReplaceFullFrame(AIndex: integer;
                              AImage: TFPCustomImage; ADelayMs: integer;
                              AHasLocalPalette: boolean = true);
    procedure OptimizeFrames;

    {TGraphic}
    procedure LoadFromStream(Stream: TStream); overload; override;
    procedure LoadFromStream(Stream: TStream; AMaxImageCount: integer); overload;
    procedure LoadFromResource(AFilename: string);
    procedure SaveToStream(Stream: TStream); overload; override;
    procedure LoadFromFile(const AFilenameUTF8: string); override;
    procedure SaveToFile(const AFilenameUTF8: string); override;
    class function GetFileExtensions: string; override;

    procedure SetSize(AWidth,AHeight: integer); virtual;
    procedure SaveToStream(Stream: TStream; AQuantizer: TBGRAColorQuantizerAny;
      ADitheringAlgorithm: TDitheringAlgorithm); overload; virtual;
    procedure Clear; override;
    destructor Destroy; override;
    procedure Pause;
    procedure Resume;

    procedure Show(Canvas: TCanvas; ARect: TRect); overload;
    procedure Update(Canvas: TCanvas; ARect: TRect); overload;
    procedure Hide(Canvas: TCanvas; ARect: TRect); overload;
    function MakeBitmapCopy(ABackground: TColor = clNone): TBitmap;

    property BackgroundColor: TColor Read FBackgroundColor write SetBackgroundColor;
    property Count: integer Read GetCount;
    property Width: integer Read FWidth;
    property Height: integer Read FHeight;
    property Paused: boolean Read FPaused;
    property Bitmap: TBitmap Read GetBitmap;
    property MemBitmap: TBGRABitmap Read GetMemBitmap;
    property CurrentImage: integer Read FCurrentImage Write SetCurrentImage;
    property TimeUntilNextImageMs: integer read GetTimeUntilNextImage;
    property FrameImage[AIndex: integer]: TBGRABitmap read GetFrameImage write SetFrameImage;
    property FrameHasLocalPalette[AIndex: integer]: boolean read GetFrameHasLocalPalette write SetFrameHasLocalPalette;
    property FrameImagePos[AIndex: integer]: TPoint read GetFrameImagePos write SetFrameImagePos;
    property FrameDelayMs[AIndex: integer]: integer read GetFrameDelayMs write SetFrameDelayMs;
    property FrameDisposeMode[AIndex: integer]: TDisposeMode read GetFrameDisposeMode write SetFrameDisposeMode;
    property AspectRatio: single read FAspectRatio write SetAspectRatio;
    property TotalAnimationTimeMs: Int64 read FTotalAnimationTime;
    property AverageDelayMs: integer read GetAverageDelayMs;
  end;

  { TBGRAReaderGIF }

  TBGRAReaderGIF = class(TFPCustomImageReader)
  protected
    procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Str: TStream): boolean; override;
  end;

  { TBGRAWriterGIF }

  TBGRAWriterGIF = class(TFPCustomImageWriter)
  protected
    procedure InternalWrite(Str: TStream; Img: TFPCustomImage); override;
  end;

const
  GifBackgroundModeStr: array[TGifBackgroundMode] of string =
    ('gbmSimplePaint', 'gbmEraseBackground', 'gbmSaveBackgroundOnce',
    'gbmUpdateBackgroundContinuously');

Implementation
End.

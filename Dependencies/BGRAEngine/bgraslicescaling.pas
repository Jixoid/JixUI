// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRASliceScaling;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRAGraphics, BGRABitmap, BGRABitmapTypes, IniFiles;

type
  TMargins = record
    top, right, bottom, left: integer;
  end;
  TSlicePosition = (spTopLeft, spTop, spTopRight, spLeft, spMiddle, spRight,
    spBottomLeft, spBottom, spBottomRight);
  TSliceBitmapArray = array[TSlicePosition] of TBGRABitmap;
  TSliceRectArray = array[TSlicePosition] of TRect;
  TSliceRepeatPosition = (srpTop, srpLeft, srpMiddleHorizontal,
    srpMiddleVertical, srpRight, srpBottom);
  TSliceRepeatArray = array[TSliceRepeatPosition] of boolean;

const
  SliceRepeatPositionStr : array[TSliceRepeatPosition] of string =
      ('Top','Left','MiddleHorizontal','MiddleVertical','Right','Bottom');

function Margins(ATop, ARight, ABottom, ALeft: integer): TMargins;

type

  { TBGRASliceScaling }

  TBGRASliceScaling = class
  private
    FSliceRectArray: TSliceRectArray;
    FSliceBitmapArray: TSliceBitmapArray;
    FSliceRepeat: TSliceRepeatArray;
    FBitmap: TBGRABitmap;
    FBitmapOwned: boolean;
    FBitmapSourceRect: TRect;
    FMargins: TMargins;
    FDrawMode: TDrawMode;
    FResampleMode: TResampleMode;
    FResampleFilter: TResampleFilter;
    function GetBitmapHeight: integer;
    function GetBitmapWidth: integer;
    function GetSlice(APosition: TSlicePosition): TBGRABitmap;
    function GetSliceRepeat(Aposition: TSliceRepeatPosition): boolean;
    function GetSliceRepeatAsString: string;
    procedure SetBitmap(AValue: TBGRABitmap);
    procedure SetBitmapSourceRect(AValue: TRect);
    procedure SetDrawMode(AValue: TDrawMode);
    procedure SetResampleFilter(AValue: TResampleFilter);
    procedure SetResampleMode(AValue: TResampleMode);
    procedure SetSliceRepeat(Aposition: TSliceRepeatPosition; AValue: boolean);
    procedure SetSliceRepeatAsString(AValue: string);
  protected
    // Stuff
    procedure UpdateSliceRectArray;
    function ComputeSliceRectArray(ARect: TRect): TSliceRectArray;
    procedure SliceScalingDraw(ADest: TBGRABitmap; ADestRect: TRect;
      DrawGrid: boolean = False);
    procedure Init;
    procedure ClearBitmapArray;
  public
    // Create an instance and stores the bitmap, either as a reference to a TBGRABitmap from the caller,
    // or as a local owned copy in other cases
    constructor Create(ABitmap: TBGRABitmap;
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft: integer; ABitmapOwner: boolean = false); overload;
    constructor Create(ABitmap: TBitmap;
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft: integer); overload;
    constructor Create(AFilename: string;
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft: integer); overload;
    constructor Create(AFilename: string; AIsUtf8: boolean;
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft: integer); overload;
    constructor Create(AStream: TStream;
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft: integer); overload;
    constructor Create(ABitmap: TBGRABitmap; ABitmapOwner: boolean = false); overload;
    constructor Create(ABitmap: TBitmap); overload;
    constructor Create(AFilename: string); overload;
    constructor Create(AFilename: string; AIsUtf8: boolean); overload;
    constructor Create(AStream: TStream); overload;
    constructor Create; overload;
    procedure SetMargins(AMarginTop, AMarginRight, AMarginBottom, AMarginLeft: integer); overload;
    procedure SetMargins(AMargins: TMargins); overload;
    destructor Destroy; override;
  public
    procedure NotifyBitmapChanged; //to notify the source bitmap has changed
    //so new bitmaps should be used
    // Draw
    procedure Draw(ABitmap: TBGRABitmap; ARect: TRect; DrawGrid: boolean = False); overload;
    procedure Draw(ABitmap: TBGRABitmap; ALeft, ATop, AWidth, AHeight: integer;
      DrawGrid: boolean = False); overload;
    procedure AutodetectRepeat;
  public
    // Property
    property DrawMode: TDrawMode read FDrawMode write SetDrawMode;
    property ResampleMode: TResampleMode read FResampleMode write SetResampleMode;
    property ResampleFilter: TResampleFilter read FResampleFilter
      write SetResampleFilter;
    property BitmapWidth: integer read GetBitmapWidth;
    property BitmapHeight: integer read GetBitmapHeight;
    property BitmapSource: TBGRABitmap read FBitmap write SetBitmap;
    property BitmapSourceRect: TRect read FBitmapSourceRect write SetBitmapSourceRect;
    property Margins: TMargins read FMargins write SetMargins;
    property SliceBitmap[APosition: TSlicePosition]: TBGRABitmap read GetSlice;
    property SliceRepeat[Aposition: TSliceRepeatPosition]: boolean
      read GetSliceRepeat write SetSliceRepeat;
    property SliceRepeatAsString: string read GetSliceRepeatAsString write SetSliceRepeatAsString;
  end;

  TSliceScalingArray = array of TBGRASliceScaling;
  TSliceScalingDirection = (sdHorizontal, sdVertical);
  TBGRABitmapArray = array of TBGRABitmap;

  { TBGRAMultiSliceScaling }

  TBGRAMultiSliceScaling = class
  private
    FSliceScalingArray: TSliceScalingArray;
    FBitmapOwned: boolean;
    FBitmap: TBGRABitmap;
    function GetCount: integer;
    procedure SetFSliceScalingArray(AValue: TSliceScalingArray);
  public
    constructor Create(ABitmap: TBGRABitmap;
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft, NumberOfItems: integer;
      Direction: TSliceScalingDirection; ABitmapOwner: boolean = false); overload;
    constructor Create(ABitmap: TBitmap;
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft, NumberOfItems: integer;
      Direction: TSliceScalingDirection); overload;
    constructor Create(ABitmapFilename: string;
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft, NumberOfItems: integer;
      Direction: TSliceScalingDirection); overload;
    constructor Create(ABitmapFilename: string; AIsUtf8: boolean;
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft, NumberOfItems: integer;
      Direction: TSliceScalingDirection); overload;
    constructor Create(AStream: TStream;
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft, NumberOfItems: integer;
      Direction: TSliceScalingDirection); overload;
    destructor Destroy; override;
    constructor Create(AIniFilename, ASection: string; AIsUtf8Filename: boolean= false); overload;
  public
    procedure Draw(ItemNumber: integer; ABitmap: TBGRABitmap;
      ARect: TRect; DrawGrid: boolean = False); overload;
    procedure Draw(ItemNumber: integer; ABitmap: TBGRABitmap;
      ALeft, ATop, AWidth, AHeight: integer; DrawGrid: boolean = False); overload;
  public
    property Count: integer read GetCount;
    property SliceScalingArray: TSliceScalingArray
      read FSliceScalingArray write SetFSliceScalingArray;
  end;

Implementation
End.

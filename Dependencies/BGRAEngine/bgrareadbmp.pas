// SPDX-License-Identifier: LGPL-3.0-linking-exception
{*****************************************************************************}
{
    This original file was part of the Free Pascal's "Free Components Library".
    Copyright (c) 2003 by Mazen NEIFER of the Free Pascal development team

    BMP reader implementation modified by circular.
}
{*****************************************************************************}
{ 08/2005 by Giulio Bernardi:
   - Added support for 16 and 15 bpp bitmaps.
   - If we have bpp <= 8 make an indexed image instead of converting it to RGB
   - Support for RLE4 and RLE8 decoding
   - Support for top-down bitmaps

  03/2014 by circular:
   - RLE optimisation using a read buffer
   - direct access to pixels with TBGRABitmap
   - vertical shrink option with MinifyHeight,WantedHeight,OutputHeight (useful for thumbnails)
  01/2017 by circular:
   - support for OS/2 1.x format
   - support for headerless files
}

{$mode objfpc}
{$h+}

unit BGRAReadBMP;

interface

uses FPImage, BGRAClasses, SysUtils, BMPcomn, BGRABitmapTypes;

type
  TBMPTransparencyOption = (toAuto, toTransparent, toOpaque);
  TBitMapInfoHeader = BMPcomn.TBitMapInfoHeader;
  TBitMapFileHeader = BMPcomn.TBitMapFileHeader;
  TOS2BitmapHeader = packed record
    bcSize: LongWord;
    bcWidth: Word;
    bcHeight: Word;
    bcPlanes: Word;
    bcBitCount: Word;
  end;
  TMinimumBitmapHeader = packed record
    Size:longint;
    Width:longint;
    Height:longint;
    Planes:word;
    BitCount:word;
  end;
  TBitmapSubFormat = (bsfWithFileHeader, bsfHeaderless, bsfHeaderlessWithMask);
  TReadScanlineProc = procedure(Row : Integer; Stream : TStream) of object;
  TWriteScanlineProc = procedure(Row : Integer; Img : TFPCustomImage) of object;
  TProgressProc = procedure(Percent: integer; var ShouldContinue: boolean) of object;


  { TBGRAReaderBMP }

  TBGRAReaderBMP = class (TBGRAImageReader)
    Private
      DeltaX, DeltaY : integer; // Used for the never-used delta option in RLE
      TopDown : boolean;        // If set, bitmap is stored top down instead of bottom up
      Procedure FreeBufs;       // Free (and nil) buffers.
    protected
      ReadSize : Integer;       // Size (in bytes) of 1 scanline.
      BFH: TBitMapFileHeader;    // The file header
      BFI: TBitMapInfoHeader;  // The header as read from the stream.
      FPaletteEntrySize: integer;  // 4 for Windows, 3 for OS/2 1.x
      FPalette : PFPcolor;      // Buffer with Palette entries. (useless now)
      FBGRAPalette : PBGRAPixel;
      LineBuf : PByte;          // Buffer for 1 scanline. Can be Byte, Word, TColorRGB or TColorRGBA
      RedMask, GreenMask, BlueMask : LongWord; //Used if Compression=bi_bitfields
      RedShift, GreenShift, BlueShift : shortint;
      FOutputHeight: integer;
      FOriginalHeight: Integer;
      FTransparencyOption: TBMPTransparencyOption;
      FBuffer: packed array of byte;
      FBufferPos, FBufferSize: integer;
      FBufferStream: TStream;
      FHasAlphaValues: boolean;
      FMaskData: PByte;
      FMaskDataSize: integer;
      // SetupRead will allocate the needed buffers, and read the colormap if needed.
      procedure SetupRead(nPalette, nRowBits: Integer; Stream : TStream); virtual;
      function CountBits(Value : byte) : shortint;
      function ShiftCount(Mask : LongWord) : shortint;
      function ExpandColor(value : LongWord) : TFPColor;
      function ExpandColorBGRA(value : LongWord) : TBGRAPixel;
      procedure ExpandRLE8ScanLine(Row : Integer; Stream : TStream);
      procedure ExpandRLE4ScanLine(Row : Integer; Stream : TStream);
      procedure ReadScanLine(Row : Integer; Stream : TStream); virtual;
      procedure SkipScanLine(Row : Integer; Stream : TStream); virtual;
      procedure WriteScanLine(Row : Integer; Img : TFPCustomImage); virtual;
      procedure WriteScanLineBGRA(Row : Integer; Img : TFPCustomImage); virtual;
      procedure ReadMaskLine({%H-}Row : Integer; Stream : TStream); virtual;
      procedure SkipMaskLine({%H-}Row : Integer; Stream : TStream); virtual;
      procedure WriteMaskLine(Row : Integer; Img : TFPCustomImage); virtual;
      // required by TFPCustomImageReader
      procedure InternalRead  (Stream:TStream; Img:TFPCustomImage); override;
      function  InternalCheck (Stream:TStream) : boolean; override;
      procedure InitReadBuffer(AStream: TStream; ASize: integer);
      procedure CloseReadBuffer;
      function GetNextBufferByte: byte;
      procedure MakeOpaque(Img: TFPCustomImage);
      procedure LoadMask(Stream:TStream; Img:TFPCustomImage; var ShouldContinue: boolean);
      procedure MainProgressProc(Percent: integer; var ShouldContinue: boolean);
      procedure ImageVerticalLoop(Stream:TStream; Img:TFPCustomImage;
        ReadProc, SkipProc: TReadScanlineProc; WriteProc: TWriteScanlineProc;
        ProgressProc: TProgressProc; var ShouldContinue: boolean);
    public
      MinifyHeight,WantedHeight: integer;
      Hotspot: TPoint;
      Subformat: TBitmapSubFormat;
      constructor Create; override;
      destructor Destroy; override;
      property OriginalHeight: integer read FOriginalHeight;
      property OutputHeight: integer read FOutputHeight;
      property TransparencyOption: TBMPTransparencyOption read FTransparencyOption write FTransparencyOption;
      function GetQuickInfo(AStream: TStream): TQuickImageInfo; override;
      function GetBitmapDraft(AStream: TStream; {%H-}AMaxWidth, AMaxHeight: integer; out AOriginalWidth,AOriginalHeight: integer): TBGRACustomBitmap; override;
  end;

function MakeBitmapFileHeader(AData: TStream): TBitMapFileHeader;

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
    This file is originally part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    PNG reader implementation modified by circular.

 **********************************************************************

  Optimisations applied:
  - using "const" parameter for TColorData
  - direct pixel access with TBGRABitmap when possible
  - some fixes of hints and of initializations
  - vertical shrink option with MinifyHeight, OriginalHeight and VerticalShrinkFactor (useful for thumbnails)
 }
{$mode objfpc}{$h+}
unit BGRAReadPng;

interface

uses
  SysUtils,BGRAClasses, FPImage, FPImgCmn, PNGComn, ZStream, BGRABitmapTypes;

Type

  TSetPixelProc = procedure (x,y:integer; const CD : TColordata) of object;
  TConvertColorProc = function (const CD:TColorData) : TFPColor of object;
  TBGRAConvertColorProc = function (const CD:TColorData) : TBGRAPixel of object;
  THandleScanLineProc = procedure (const y : integer; const ScanLine : PByteArray) of object;

  { TBGRAReaderPNG }

  TBGRAReaderPNG = class (TBGRAImageReader)
    private
      FHeader : THeaderChunk;
      ZData : TMemoryStream;  // holds compressed data until all blocks are read
      Decompress : TDeCompressionStream; // decompresses the data
      FPltte : boolean;     // if palette is used
      FCountScanlines : EightLong; //Number of scanlines to process for each pass
      FScanLineLength : EightLong; //Length of scanline for each pass
      FCurrentPass : byte;
      ByteWidth : byte;          // number of bytes to read for pixel information
      BitsUsed : EightLong; // bitmasks to use to split a byte into smaller parts
      BitShift : byte;  // shift right to do of the bits extracted with BitsUsed for 1 element
      CountBitsUsed : byte;  // number of bit groups (1 pixel) per byte (when bytewidth = 1)
      //CFmt : TColorFormat; // format of the colors to convert from
      StartX,StartY, DeltaX,DeltaY, StartPass,EndPass : integer;  // number and format of passes
      FPalette : TFPPalette;
      FSetPixel : TSetPixelProc;
      FConvertColor : TConvertColorProc;
      FBGRAConvertColor : TBGRAConvertColorProc;
      FHandleScanLine: THandleScanLineProc;
      FVerticalShrinkMask: LongWord;
      FVerticalShrinkShr: Integer;
      FGammaCorrection: single;
      FGammaCorrectionTable: packed array of word;
      FGammaCorrectionTableComputed: boolean;
      function GetOriginalHeight: integer;
      function GetOriginalWidth: integer;
      function GetVerticalShrinkFactor: integer;
      function ReadChunk: boolean;
      procedure HandleData;
      procedure HandleUnknown;
      function ColorGray1 (const CD:TColorData) : TFPColor;
      function ColorGray2 (const CD:TColorData) : TFPColor;
      function ColorGray4 (const CD:TColorData) : TFPColor;
      function ColorGray8 (const CD:TColorData) : TFPColor;
      function ColorGray16 (const CD:TColorData) : TFPColor;
      function ColorGrayAlpha8 (const CD:TColorData) : TFPColor;
      function ColorGrayAlpha16 (const CD:TColorData) : TFPColor;
      function ColorColor8 (const CD:TColorData) : TFPColor;
      function ColorColor16 (const CD:TColorData) : TFPColor;
      function ColorColorAlpha8 (const CD:TColorData) : TFPColor;
      function ColorColorAlpha16 (const CD:TColorData) : TFPColor;
      function CheckGammaCorrection: boolean;
      procedure ApplyGammaCorrection(var AColor: TFPColor);

      function BGRAColorGray1 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorGray2 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorGray4 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorGray8 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorGray16 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorGrayAlpha8 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorGrayAlpha16 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorColor8 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorColor16 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorColorAlpha8 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorColorAlpha16 (const CD:TColorData) : TBGRAPixel;
    protected
      Chunk : TChunk;
      UseTransparent, EndOfFile : boolean;
      TransparentDataValue : TColorData;
      UsingBitGroup : byte;
      DataIndex : LongWord;
      DataBytes : TColorData;
      procedure HandleChunk; virtual;
      procedure HandlePalette; virtual;
      procedure HandleAlpha; virtual;
      procedure HandleStdRGB; virtual;
      procedure HandleGamma; virtual;
      function CalcX (relX:integer) : integer;
      function CalcY (relY:integer) : integer;
      function CalcColor(const ScanLine : PByteArray): TColorData;
      procedure HandleScanLine (const y : integer; const ScanLine : PByteArray); virtual;
      procedure BGRAHandleScanLine(const y: integer; const ScanLine: PByteArray);
      procedure BGRAHandleScanLineTr(const y: integer; const ScanLine: PByteArray);
      procedure DoDecompress; virtual;
      procedure SetPalettePixel (x,y:integer; const CD : TColordata);
      procedure SetPalColPixel (x,y:integer; const CD : TColordata);
      procedure SetColorPixel (x,y:integer; const CD : TColordata);
      procedure SetColorTrPixel (x,y:integer; const CD : TColordata);
      procedure SetBGRAColorPixel (x,y:integer; const CD : TColordata);
      procedure SetBGRAColorTrPixel (x,y:integer; const CD : TColordata);
      function DecideSetPixel : TSetPixelProc; virtual;
      procedure InternalRead  ({%H-}Str:TStream; Img:TFPCustomImage); override;
      function  InternalCheck (Str:TStream) : boolean; override;
      //property ColorFormat : TColorformat read CFmt;
      property ConvertColor : TConvertColorProc read FConvertColor;
      property CurrentPass : byte read FCurrentPass;
      property Pltte : boolean read FPltte;
      property ThePalette : TFPPalette read FPalette;
      property Header : THeaderChunk read FHeader;
      property CountScanlines : EightLong read FCountScanlines;
      property ScanLineLength : EightLong read FScanLineLength;
    public
      MinifyHeight: integer;
      constructor create; override;
      destructor destroy; override;
      property VerticalShrinkFactor: integer read GetVerticalShrinkFactor;
      property OriginalWidth: integer read GetOriginalWidth;
      property OriginalHeight: integer read GetOriginalHeight;
      function GetQuickInfo(AStream: TStream): TQuickImageInfo; override;
      function GetBitmapDraft(AStream: TStream; {%H-}AMaxWidth, AMaxHeight: integer; out AOriginalWidth,AOriginalHeight: integer): TBGRACustomBitmap; override;
  end;

Implementation
End.

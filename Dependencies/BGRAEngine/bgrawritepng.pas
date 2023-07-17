// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
    The original file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    PNG writer class modified by circular.

 **********************************************************************

 Fix for images with grayscale and alpha,
 and for images with transparent pixels
 }
unit BGRAWritePNG;

{$mode objfpc}{$H+}

interface


uses sysutils, BGRAClasses, FPImage, FPImgCmn, PNGcomn, ZStream, BGRABitmapTypes;

type
  THeaderChunk = packed record
    Width, height : LongWord;
    BitDepth, ColorType, Compression, Filter, Interlace : byte;
  end;

  TGetPixelFunc = function (x,y : LongWord) : TColorData of object;
  TGetPixelBGRAFunc = function (p: PBGRAPixel) : TColorData of object;

  TColorFormatFunction = function (color:TFPColor) : TColorData of object;

  { TBGRAWriterPNG }

  TBGRAWriterPNG = class (TBGRACustomWriterPNG)
    private
      FUsetRNS, FCompressedText, FWordSized, FIndexed,
      FUseAlpha, FGrayScale : boolean;
      FByteWidth : byte;
      FChunk : TChunk;
      CFmt : TColorFormat; // format of the colors to convert from
      FFmtColor : TColorFormatFunction;
      FTransparentColor : TFPColor;
      FTransparentColorOk: boolean;
      FSwitchLine, FCurrentLine, FPreviousLine : pByteArray;
      FPalette : TFPPalette;
      OwnsPalette : boolean;
      FHeader : THeaderChunk;
      FGetPixel : TGetPixelFunc;
      FGetPixelBGRA : TGetPixelBGRAFunc;
      FDatalineLength : LongWord;
      ZData : TMemoryStream;  // holds uncompressed data until all blocks are written
      Compressor : TCompressionStream; // compresses the data
      FCompressionLevel : TCompressionLevel;
      procedure WriteChunk;
      function GetColorPixel (x,y:LongWord) : TColorData;
      function GetPalettePixel (x,y:LongWord) : TColorData;
      function GetColPalPixel (x,y:LongWord) : TColorData;
      function GetColorPixelBGRA (p: PBGRAPixel) : TColorData;
      function GetPalettePixelBGRA (p: PBGRAPixel) : TColorData;
      function GetColPalPixelBGRA (p: PBGRAPixel) : TColorData;
      procedure InitWriteIDAT;
      procedure Gatherdata;
      procedure WriteCompressedData;
      procedure FinalWriteIDAT;
    protected
      property Header : THeaderChunk read FHeader;
      procedure InternalWrite ({%H-}Str:TStream; {%H-}Img:TFPCustomImage); override;
      function GetUseAlpha: boolean; override;
      procedure SetUseAlpha(AValue: boolean); override;
      procedure WriteIHDR; virtual;
      procedure WritePLTE; virtual;
      procedure WritetRNS; virtual;
      procedure WriteIDAT; virtual;
      procedure WriteTexts; virtual;
      procedure WriteIEND; virtual;
      function CurrentLine (x:LongWord) : byte; inline;
      function PrevSample (x:LongWord): byte; inline;
      function PreviousLine (x:LongWord) : byte; inline;
      function PrevLinePrevSample (x:LongWord): byte; inline;
      function  DoFilter (LineFilter:byte;index:LongWord; b:byte) : byte; virtual;
      procedure SetChunkLength (aValue : LongWord);
      procedure SetChunkType (ct : TChunkTypes); overload;
      procedure SetChunkType (ct : TChunkCode); overload;
      function DecideGetPixel : TGetPixelFunc; virtual;
      function DecideGetPixelBGRA : TGetPixelBGRAFunc; virtual;
      procedure DetermineHeader (var AHeader : THeaderChunk); virtual;
      function DetermineFilter ({%H-}Current, {%H-}Previous:PByteArray; {%H-}linelength:LongWord):byte; virtual;
      procedure FillScanLine (y : integer; ScanLine : pByteArray); virtual;
      function ColorDataGrayB(color:TFPColor) : TColorData;
      function ColorDataColorB(color:TFPColor) : TColorData;
      function ColorDataGrayW(color:TFPColor) : TColorData;
      function ColorDataColorW(color:TFPColor) : TColorData;
      function ColorDataGrayAB(color:TFPColor) : TColorData;
      function ColorDataColorAB(color:TFPColor) : TColorData;
      function ColorDataGrayAW(color:TFPColor) : TColorData;
      function ColorDataColorAW(color:TFPColor) : TColorData;
      property ChunkDataBuffer : pByteArray read FChunk.data;
      property UsetRNS : boolean read FUsetRNS;
      property SingleTransparentColor : TFPColor read FTransparentColor;
      property SingleTransparentColorOk : boolean read FTransparentColorOk;
      property ThePalette : TFPPalette read FPalette;
      property ColorFormat : TColorformat read CFmt;
      property ColorFormatFunc : TColorFormatFunction read FFmtColor;
      property byteWidth : byte read FByteWidth;
      property DatalineLength : LongWord read FDatalineLength;
    public
      constructor create; override;
      destructor destroy; override;
      property GrayScale : boolean read FGrayscale write FGrayScale;
      property Indexed : boolean read FIndexed write FIndexed;
      property CompressedText : boolean read FCompressedText write FCompressedText;
      property WordSized : boolean read FWordSized write FWordSized;
      property CompressionLevel : TCompressionLevel read FCompressionLevel write FCompressionLevel;
  end;

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Encode/Decode avif images from/to BGRABitmap. }
{ Author: Domingo Galmes <dgalmesp@gmail.com>  01-11-2021 }

unit avifbgra;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, libavif;

type
  EAvifException = class(Exception);
  avifPixelFormat = libavif.avifPixelFormat;

const
  AVIF_DEFAULT_QUALITY = 30;

procedure AvifLoadFromStream(AStream: TStream; aBitmap: TBGRACustomBitmap);
procedure AvifLoadFromFile(const AFilename: string; aBitmap: TBGRACustomBitmap);
procedure AvifLoadFromFileNative(const AFilename: string; aBitmap: TBGRACustomBitmap);
procedure AvifLoadFromMemory(AData: Pointer; ASize: cardinal; aBitmap: TBGRACustomBitmap);
//a Quality0to100=100 LOSSLESS
function AvifSaveToStream(aBitmap: TBGRACustomBitmap; AStream: TStream; aQuality0to100: integer = 30;aSpeed0to10:integer=AVIF_SPEED_DEFAULT;aPixelFormat:avifPixelFormat=AVIF_PIXEL_FORMAT_YUV420;aIgnoreAlpha:boolean=false): NativeUInt;
function AvifSaveToFile(aBitmap: TBGRACustomBitmap; const AFilename: string; aQuality0to100: integer = 30;aSpeed0to10:integer=AVIF_SPEED_DEFAULT;aPixelFormat:avifPixelFormat=AVIF_PIXEL_FORMAT_YUV420;aIgnoreAlpha:boolean=false): NativeUInt;
//returns size of the resulting bitmap.
function AvifSaveToMemory(aBitmap: TBGRACustomBitmap; AData: Pointer; ASize: cardinal; aQuality0to100: integer = 30;aSpeed0to10:integer=AVIF_SPEED_DEFAULT;aPixelFormat:avifPixelFormat=AVIF_PIXEL_FORMAT_YUV420;aIgnoreAlpha:boolean=false): NativeUInt;
//aBuffer  12 first bytes of file.
function AvifValidateHeaderSignature(aBuffer: Pointer): boolean;

Implementation
End.

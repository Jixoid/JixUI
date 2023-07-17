// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAThumbnail;

{$mode objfpc}{$H+}
{$i bgrabitmap.map.inc}

interface

uses
  BGRAClasses, SysUtils, BGRABitmap, BGRABitmapTypes, FPimage;

function GetBitmapThumbnail(ABitmap: TBGRACustomBitmap; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil; AVerticalShrink: single = 1; AHorizShrink: single = 1): TBGRABitmap; overload;
function GetBitmapThumbnail(ABitmap: TBGRACustomBitmap; AFormat: TBGRAImageFormat; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil; AVerticalShrink: single = 1; AHorizShrink: single = 1): TBGRABitmap; overload;
function GetFileThumbnail(AFilenameUTF8: string; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetStreamThumbnail(AStream: TStream; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ASuggestedExtensionUTF8: string = ''; ADest: TBGRABitmap= nil): TBGRABitmap; overload;
function GetStreamThumbnail(AStream: TStream; AReader: TFPCustomImageReader; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap; overload;

function GetOpenRasterThumbnail(AStream: TStream; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetLazPaintThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetPhoxoThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetJpegThumbnail(AStream: TStream; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetPsdThumbnail(AStream: TStream; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetPngThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetPaintDotNetThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetBmpThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetIcoThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetCurThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;

function GetPcxThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetTargaThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetTiffThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetGifThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetXwdThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetXPixMapThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetBmpMioMapThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;

procedure DrawThumbnailCheckers(bmp: TBGRABitmap; ARect: TRect; AIconCheckers: boolean = false);
procedure DrawThumbnailCheckers(bmp: TBGRABitmap; ARect: TRect; AIconCheckers: boolean; AScale: single);

var
  ImageCheckersColor1,ImageCheckersColor2  : TBGRAPixel;
  IconCheckersColor1,IconCheckersColor2  : TBGRAPixel;
  CheckersScale: single = 1;

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAPhoxo;

{$mode objfpc}{$H+}

interface

uses
  BGRABitmapTypes, FPImage, BGRALayers, BGRABitmap, BGRAClasses, SysUtils, BMPcomn;

const
  PhoxoHeaderMagic : packed array[1..4] of char = 'oXo ';
  PhoxoBlock_CanvasSize = 1;
  PhoxoBlock_Layer = 2;
  PhoxoBlock_TextLayer = 3;
  PhoxoBlock_DPI = 4;
  PhoxoBlock_LayerCaption = 5;
  PhoxoBlock_LazPaintBlendMode = 128;
  PhoxoBlock_EndOfFile = 255;

type
  TPhoxoHeader = packed record
    magic: packed array[1..4] of char;
    version: LongWord;
  end;

  TPhoxoBlockHeader = packed record
    blockType : LongWord;
    blockSize : LongWord;
  end;

  TPhoxoLayerHeader = packed record
    layerVisible: LongWord;
    layerLimited: LongWord;
    opacityPercent: LongWord;
    bmpHeader: TBitMapInfoHeader;
    redMask,greenMask,blueMask: LongWord;
  end;

  { TBGRAPhoxoDocument }

  TBGRAPhoxoDocument = class(TBGRALayeredBitmap)
  private
    FDPIX,FDPIY: integer;
  protected
    function GetMimeType: string; override;
    procedure AddLayerFromPhoxoData(const ABlockHeader: TPhoxoBlockHeader; ABlockData: PByte);
    procedure InternalLoadFromStream(AStream: TStream);
    procedure InternalSaveToStream(AStream: TStream);
  public
    constructor Create; overload; override;
    constructor Create(AWidth, AHeight: integer); overload; override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure LoadFromFile(const filenameUTF8: string); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure SaveToFile(const filenameUTF8: string); override;
    class function CheckFormat(Stream: TStream; ARestorePosition: boolean): boolean; static;
    class function ReadBlock(Stream: TStream; out AHeader: TPhoxoBlockHeader; out ABlockData: PByte): boolean; static;
    property DPIX: integer read FDPIX;
    property DPIY: integer read FDPIY;
  end;

  { TBGRAReaderOXO }

  TBGRAReaderOXO = class(TFPCustomImageReader)
  private
    FWidth,FHeight,FNbLayers: integer;
    FDPIX,FDPIY: integer;
  protected
    function InternalCheck(Stream: TStream): boolean; override;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
  public
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property NbLayers: integer read FNbLayers;
    property DPIX: integer read FDPIX;
    property DPIY: integer read FDPIY;
  end;

  { TBGRAWriterOXO }

  TBGRAWriterOXO = class(TFPCustomImageWriter)
    protected
      procedure InternalWrite (Str:TStream; Img:TFPCustomImage); override;
  end;

procedure RegisterPhoxoFormat;

Implementation
End.

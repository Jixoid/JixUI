// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAWriteBmpMioMap;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPimage, BGRABitmapTypes, BGRAReadBmpMioMap;

type

  { TBGRAWriterBmpMioMap }

  TBGRAWriterBmpMioMap = class (TFPCustomImageWriter)
  protected
    FHeader: TMioHeader;
    FPalette: packed array of record
        ColorValue: Word;
        AlphaValue: Byte;
        Padding: Byte;
      end;
    FPaletteIndexes: packed array[0..65535] of Int32or64;
    FPaletteOffset: Int32or64;
    FPaletteAlpha: boolean;
    FChunks: array of TMemoryStream;
    FCurrentChunk: TMemoryStream;
    FMaxChunkSize: Word;
    function IndexOfColor(const AColor: TBGRAPixel): Int32or64;
    procedure InitHeader(Img: TFPCustomImage);
    procedure InitPalette;
    procedure InitChunks;
    procedure FlushChunk;
    procedure FreeChunks;
    procedure NeedChunk;
    procedure AppendToChunks(const Buffer; Count: integer);
    procedure BuildPaletteAndChunks(Img: TFPCustomImage);
    procedure WriteHeader(Str: TStream);
    procedure WritePalette(Str: TStream);
    procedure WriteChunks(Str: TStream);
    procedure ReadScanline(Img: TFPCustomImage; Y: integer; ADest: PBGRAPixel);
    procedure InternalWrite(Str: TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
    property MaxChunkSize: Word read FMaxChunkSize write FMaxChunkSize;
  end;

Implementation
End.

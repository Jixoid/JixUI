// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRALzpCommon;

{$mode objfpc}{$H+}
{$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
interface

uses
  BGRAClasses, SysUtils;

const
  LAZPAINT_COMPRESSION_MODE_ZSTREAM = 1;
  LAZPAINT_COMPRESSION_MODE_RLE = 2;
  LAZPAINT_COMPRESSION_MASK = 255;
  LAZPAINT_THUMBNAIL_PNG = 256;
  LAZPAINT_MAGIC_HEADER : array[0..7] of char = 'LazPaint';

  LazpaintChannelGreenFromRed  = 1;
  LazpaintChannelBlueFromRed   = 2;
  LazpaintChannelBlueFromGreen = 4;
  LazpaintChannelNoAlpha       = 8;
  LazpaintPalettedRGB          = 16;

  LazPaintThumbMaxWidth = 128;
  LazPaintThumbMaxHeight = 128;

type
  TLzpCompression = (lzpZStream, //slower and not necessarily better
                     lzpRLE);    //custom RLE for lzp files

  { TLazPaintImageHeader }

  TLazPaintImageHeader = packed record
    magic: packed array[0..7] of char;
    zero1, headerSize: LongWord;
    width, height, nbLayers, previewOffset: LongWord;
    zero2, compressionMode, reserved1, layersOffset: LongWord;
  end;

procedure LazPaintImageHeader_SwapEndianIfNeeded(AHeader: TLazPaintImageHeader);

//routines to compress and uncompress byte-sized values (you need to
//separate the channels to obtain any compression)

procedure EncodeLazRLE(var sourceBuffer; size:PtrInt; ADest: TStream);
function DecodeLazRLE(ASource: TStream; var destBuffer; availableOutputSize: PtrInt; availableInputSize: int64 = -1): PtrInt;

Implementation
End.

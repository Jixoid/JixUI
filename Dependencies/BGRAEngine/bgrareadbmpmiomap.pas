// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAReadBmpMioMap;

{$mode objfpc}{$H+}
{$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
{$WARN 5093 off : function result variable of a managed type does not seem to be initialized}
interface

uses
  BGRAClasses, SysUtils, FPimage, BGRABitmapTypes;

const
  MioMapMagicValue = 'RL';
  MioMapTransparentColor = $F81F;

type
  TMioHeader = packed record
    magic: packed array[1..2] of char;
    format: word;
    width,height,nbColors,nbChunks: word;
  end;

  TPixelArray = array of TBGRAPixel;

  { TBGRAReaderBmpMioMap }

  TBGRAReaderBmpMioMap = class(TFPCustomImageReader)
  private
    function ReadHeader(Stream: TStream; out header: TMioHeader): boolean;
    function ReadPalette(Stream: TStream; nbColors: integer; alphaChannel: boolean): TPixelArray;
    procedure UncompressChunks(Stream: TStream; nbChunks: integer; palette: TPixelArray; img: TFPCustomImage);
  public
    procedure InternalRead  (Stream:TStream; Img:TFPCustomImage); override;
    function  InternalCheck (Stream:TStream) : boolean; override;
  end;

function MioMapToBGRA(AColor: Word): TBGRAPixel;
function BGRAToMioMap(const AColor: TBGRAPixel): Word;
function MioMapToAlpha(AValue: Byte): Byte;
function AlphaToMioMap(AValue: Byte): Byte;

Implementation
End.

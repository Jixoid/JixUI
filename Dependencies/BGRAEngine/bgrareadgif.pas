// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ This unit provides some optimisations of TFPReaderGif: decompression algorithm and direct pixel access of TBGRABitmap.
  Note: to read an animation use TBGRAAnimatedGif instead. }

unit BGRAReadGif;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPimage, FPReadGif;

type
  PGifRGB = ^TGifRGB;

  { TBGRAReaderGif }

  TBGRAReaderGif = class(TFPReaderGif)
  protected
    procedure ReadPaletteAtOnce(Stream: TStream; Size: integer);
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function ReadScanLine(Stream: TStream): boolean; override;
    function WriteScanLineBGRA(Img: TFPCustomImage): Boolean; virtual;
  end;

Implementation
End.

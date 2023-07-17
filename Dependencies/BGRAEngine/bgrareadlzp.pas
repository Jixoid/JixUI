// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAReadLzp;

{$mode objfpc}{$H+}
{$WARN 5092 off : Variable "$1" of a managed type does not seem to be initialized}
{$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
interface

uses
  BGRAClasses, SysUtils, FPimage, BGRALzpCommon, BGRABitmapTypes, BGRABitmap;

type

  { TBGRAReaderLazPaint }

  TBGRAReaderLazPaint = class(TFPCustomImageReader)
  private
    FHeight: integer;
    FNbLayers: integer;
    FWidth: integer;
    FCaption: string;
    FDimensionsAlreadyFetched: boolean;
  protected
    procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
    procedure InternalReadLayers({%H-}str: TStream;{%H-}Img: TFPCustomImage); virtual;
    procedure InternalReadCompressableBitmap(str: TStream; Img: TFPCustomImage); virtual;
    function InternalCheck(Str: TStream): boolean; override;
  public
    WantThumbnail: boolean;
    class procedure LoadRLEImage(Str: TStream; Img: TFPCustomImage; out ACaption: string); static;
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property NbLayers: integer read FNbLayers;
    property Caption: string read FCaption;
  end;

Implementation
End.

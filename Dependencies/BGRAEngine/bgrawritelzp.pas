// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAWriteLzp;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPimage, BGRALzpCommon, BGRABitmapTypes, BGRABitmap;

type
  { TBGRAWriterLazPaint }

  TBGRAWriterLazPaint = class(TFPCustomImageWriter)
  private
    function GetCompression: TLzpCompression;
    function GetIncludeThumbnail: boolean;
    procedure SetCompression(AValue: TLzpCompression);
    procedure SetIncludeThumbnail(AValue: boolean);
    function WriteThumbnail(Str: TStream; Img: TFPCustomImage): boolean;
  protected
    CompressionMode: LongWord;
    procedure InternalWrite(Str: TStream; Img: TFPCustomImage); override;
    function InternalWriteLayers({%H-}Str: TStream; {%H-}Img: TFPCustomImage): boolean; virtual;
    function GetNbLayers: integer; virtual;
  public
    Caption: string;
    constructor Create; override;
    class procedure WriteRLEImage(Str: TStream; Img: TFPCustomImage; ACaption: string= ''); static;
    property Compression: TLzpCompression read GetCompression write SetCompression;
    property IncludeThumbnail: boolean read GetIncludeThumbnail write SetIncludeThumbnail;
  end;

Implementation
End.

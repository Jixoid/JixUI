// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAPaintNet;

{$mode objfpc}{$H+}

interface

{ This unit reads Paint.NET files. It needs BGRADNetDeserial to deserialize binary .Net objects.

  A Paint.NET image consists in three parts :
  - Xml header
  - Binary serialized information (contains layer information)
  - Compressed data (pixel data)

  The class TPaintDotNetFile do not read the Xml header. ComputeFlatImage builds the resulting image
  by using blending operations to merge layers.

  The unit registers a TFPCustomImageReader so that it can be read by any image reading function of FreePascal,
  and also registers a reader for BGRALayers }

uses
  BGRAClasses, SysUtils, BGRADNetDeserial, FPImage, BGRABitmapTypes, BGRABitmap, BGRALayers;

type

  { TPaintDotNetFile }

  TPaintDotNetFile = class(TBGRACustomLayeredBitmap)
  public
    procedure LoadFromFile(const filenameUTF8: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure Clear; override;
    function ToString: ansistring; override;
    function GetLayerBitmapCopy(layer: integer): TBGRABitmap; override;
    constructor Create; override;
  protected
    procedure InternalLoadFromStream(stream: TStream);
    function GetWidth: integer; override;
    function GetHeight: integer; override;
    function GetNbLayers: integer; override;
    function GetBlendOperation(Layer: integer): TBlendOperation; override;
    function GetLayerVisible(layer: integer): boolean; override;
    function GetLayerOpacity(layer: integer): byte; override;
    function GetLayerName(layer: integer): string; override;
  private
    Content:   TDotNetDeserialization;
    Document:  TSerializedClass;
    Layers:    TSerializedClass;
    LayerData: array of TMemoryStream;
    function InternalGetLayer(num: integer): TSerializedClass;
    function InternalGetBlendOperation(layer: TSerializedClass): TBlendOperation;
    function InternalGetLayerName(layer: TSerializedClass): string;
    function InternalGetLayerVisible(layer: TSerializedClass): boolean;
    function InternalGetLayerOpacity(layer: TSerializedClass): byte;
    function LayerDataSize(numLayer: integer): int64;
    procedure LoadLayer(dest: TMemoryStream; src: TStream; uncompressedSize: int64);
  end;

  { TFPReaderPaintDotNet }

  TFPReaderPaintDotNet = class(TFPCustomImageReader)
    private
      FWidth,FHeight,FNbLayers: integer;
    protected
      function InternalCheck(Stream: TStream): boolean; override;
      procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    public
      property Width: integer read FWidth;
      property Height: integer read FHeight;
      property NbLayers: integer read FNbLayers;
  end;

function IsPaintDotNetFile(filename: string): boolean;
function IsPaintDotNetFileUTF8(filenameUTF8: string): boolean;
function IsPaintDotNetStream(stream: TStream): boolean;
function LoadPaintDotNetFile(filename: string): TBGRABitmap;
function LoadPaintDotNetFileUTF8(filenameUTF8: string): TBGRABitmap;

procedure RegisterPaintNetFormat;

Implementation
End.

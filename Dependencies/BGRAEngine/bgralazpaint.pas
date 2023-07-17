// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRALazPaint;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRALayers, BGRABitmapTypes, BGRAReadLzp, BGRAWriteLzp,
  BGRALzpCommon, FPimage;

type
  TLzpCompression = BGRALzpCommon.TLzpCompression;

  { TBGRALazPaintImage }

  TBGRALazPaintImage = class(TBGRALayeredBitmap)
  private
    FSelectedLayerIndex: integer;
  protected
    procedure InternalLoadFromStream(AStream: TStream);
    procedure InternalSaveToStream(AStream: TStream);
  public
    constructor Create; overload; override;
    constructor Create(AWidth, AHeight: integer); overload; override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure LoadFromFile(const filenameUTF8: string); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure SaveToFile(const filenameUTF8: string); override;
    property SelectedLayerIndex: integer read FSelectedLayerIndex write FSelectedLayerIndex;
  end;

  { TBGRAWriterLazPaintWithLayers }

  TBGRAWriterLazPaintWithLayers = class(TBGRAWriterLazPaint)
    protected
      FLayers: TBGRALayeredBitmap;
      FSelectedLayerIndex: integer;
      FCompression: TLzpCompression;
      function GetNbLayers: integer; override;
      function InternalWriteLayers(Str: TStream; {%H-}Img: TFPCustomImage): boolean; override;
    public
      constructor Create(ALayers: TBGRALayeredBitmap); overload;
      property SelectedLayerIndex: integer read FSelectedLayerIndex write FSelectedLayerIndex;
      property Compression: TLzpCompression read FCompression write FCompression;
  end;

  { TBGRAReaderLazPaintWithLayers }

  TBGRAReaderLazPaintWithLayers = class(TBGRAReaderLazPaint)
    protected
      FLayers: TBGRALayeredBitmap;
      FLayersLoaded: boolean;
      FSelectedLayerIndex: integer;
      procedure InternalReadLayers(str: TStream; {%H-}Img: TFPCustomImage); override;
    public
      constructor Create(ALayers: TBGRALayeredBitmap); overload;
      property LayersLoaded: boolean read FLayersLoaded;
      property SelectedLayerIndex: integer read FSelectedLayerIndex;
  end;

procedure RegisterLazPaintFormat;

Implementation
End.

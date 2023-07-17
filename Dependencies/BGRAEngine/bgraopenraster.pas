// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAOpenRaster;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRALayers, zipper, DOM, BGRABitmap, BGRALayerOriginal,
  BGRASVGShapes, FPImage, BGRASVG;

const
  OpenRasterMimeType = 'image/openraster'; //do not change, it's part of the file format
  OpenRasterSVGDefaultDPI = 90;

type

  { TBGRAOpenRasterDocument }

  TBGRAOpenRasterDocument = class(TBGRALayeredBitmap)
  private
    FFiles: array of record
      Filename: string;
      Stream: TMemoryStream;
    end;
    FStackXML: TXMLDocument;
    FZipInputStream: TStream;
    procedure SetMimeType(AValue: string);
  protected
    Procedure ZipOnCreateStream(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
    Procedure ZipOnDoneStream(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
    Procedure ZipOnOpenInputStream(Sender : TObject; var AStream : TStream);
    Procedure ZipOnCloseInputStream(Sender : TObject; var AStream : TStream);
    procedure ClearFiles;
    function GetMemoryStream(AFilename: string): TMemoryStream;
    procedure SetMemoryStream(AFilename: string; AStream: TMemoryStream);
    function AddLayerFromMemoryStream(ALayerFilename: string): integer;
    function CopyRasterLayerToMemoryStream(ALayerIndex: integer; ALayerFilename: string): boolean;
    procedure CopySVGToMemoryStream(ASVG: TBGRASVG; ASVGMatrix: TAffineMatrix; AOutFilename: string; out AOffset: TPoint);
    function CopyBitmapToMemoryStream(ABitmap: TBGRABitmap; AFilename: string): boolean;
    procedure SetMemoryStreamAsString(AFilename: string; AContent: string);
    function GetMemoryStreamAsString(AFilename: string): string;
    procedure UnzipFromStream(AStream: TStream; AFileList: TStrings = nil);
    procedure UnzipFromFile(AFilenameUTF8: string);
    procedure ZipToFile(AFilenameUTF8: string);
    procedure ZipToStream(AStream: TStream);
    procedure CopyThumbnailToMemoryStream(AMaxWidth, AMaxHeight: integer);
    procedure AnalyzeZip; virtual;
    procedure PrepareZipToSave; virtual;
    function GetMimeType: string; override;
    procedure InternalLoadFromStream(AStream: TStream);
    procedure InternalSaveToStream(AStream: TStream);

  public
    constructor Create; overload; override;
    constructor Create(AWidth, AHeight: integer); overload; override;
    procedure Clear; override;
    function CheckMimeType(AStream: TStream): boolean;
    procedure LoadFlatImageFromStream(AStream: TStream;
              out ANbLayers: integer;
              out ABitmap: TBGRABitmap);
    procedure LoadFromStream(AStream: TStream); override;
    procedure LoadFromFile(const filenameUTF8: string); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure SaveToFile(const filenameUTF8: string); override;
    property MimeType : string read GetMimeType write SetMimeType;
    property StackXML : TXMLDocument read FStackXML;
  end;

  { TFPReaderOpenRaster }

  TFPReaderOpenRaster = class(TFPCustomImageReader)
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

  { TFPWriterOpenRaster }

  TFPWriterOpenRaster = class(TFPCustomImageWriter)
    protected
      procedure InternalWrite (Str:TStream; Img:TFPCustomImage); override;
  end;

procedure RegisterOpenRasterFormat;

Implementation
End.

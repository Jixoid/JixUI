// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
    The original file is part of the Free Pascal run time library.
    Copyright (c) 2012-2013 by the Free Pascal development team

    Tiff reader for fpImage modified by circular.

 **********************************************************************

 Working:
   Sample bitdepth: 1, 4, 8, 12, 16
   Color format: black and white, grayscale, RGB, colormap, L*a*b*
   Alpha channel: none, premultiplied, separated
   Compression: packbits, LZW, deflate
   Endian-ness: little endian and big endian
   Orientation: any corner can be (0,0) and x/y can be flipped
   Planar configuration: 1 (channels together)
   Fill order: any (for 1 bit per sample images)
   Skipping thumbnail by reading biggest image
   Multiple images
   Strips and tiles

 ToDo:
   Compression: FAX, Jpeg...
   Color format: YCbCr, ITU L*a*b*
   PlanarConfiguration: 2 (one chunk for each channel)
   bigtiff 64bit offsets
   XMP tag 700
   ICC profile tag 34675

 Not to do:
   Separate mask (deprecated)

}
unit BGRAReadTiff;

{$mode objfpc}{$H+}

{$inline on}

interface

uses
  Math, BGRAClasses, SysUtils, ctypes, zinflate, zbase, FPimage, FPTiffCmn,
  BGRABitmapTypes;

type
  TBGRAReaderTiff = class;

  TTiffCreateCompatibleImgEvent = procedure(Sender: TBGRAReaderTiff;
                                            ImgFileDir: TTiffIFD) of object;

  TTiffCheckIFDOrder = (
    tcioSmart,
    tcioAlways,
    tcioNever
    );

  { TBGRAReaderTiff }

  TBGRAReaderTiff = class(TFPCustomImageReader)
  private
    FCheckIFDOrder: TTiffCheckIFDOrder;
    FFirstIFDStart: LongWord;
    FOnCreateImage: TTiffCreateCompatibleImgEvent;
    {$ifdef FPC_Debug_Image}
    FDebug: boolean;
    {$endif}
    FIFDList: TFPList;
    FReverseEndian: Boolean;
    fStartPos: int64;
    s: TStream;
    function GetImages(Index: integer): TTiffIFD;
    procedure TiffError(Msg: string);
    procedure SetStreamPos(p: LongWord);
    function ReadTiffHeader(QuickTest: boolean; out IFDStart: LongWord): boolean; // returns IFD: offset to first IFD
    function ReadIFD(Start: LongWord; IFD: TTiffIFD): LongWord;// Image File Directory
    procedure ReadDirectoryEntry(var EntryTag: Word; IFD: TTiffIFD);
    function ReadEntryUnsigned: LongWord;
    function ReadEntrySigned: Cint32;
    function ReadEntryRational: TTiffRational;
    function ReadEntryString: string;
    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadDWord: LongWord;
    procedure ReadValues(StreamPos: LongWord;
                         out EntryType: word; out EntryCount: LongWord;
                         out Buffer: Pointer; out ByteCount: PtrUInt);
    procedure ReadShortOrLongValues(StreamPos: LongWord;
                                    out Buffer: PLongWord; out Count: LongWord);
    procedure ReadShortValues(StreamPos: LongWord;
                              out Buffer: PWord; out Count: LongWord);
    procedure ReadImageSampleProperties(IFD: TTiffIFD; out AlphaChannel: integer; out PremultipliedAlpha: boolean;
      out SampleCnt: LongWord; out SampleBits: PWord; out SampleBitsPerPixel: LongWord;
      out PaletteCnt: LongWord; out PaletteValues: PWord);
    procedure ReadImgValue(BitCount: Word;
      var Run: Pointer; var BitPos: Byte; FillOrder: LongWord;
      Predictor: word; var LastValue: word; out Value: Word);
    function FixEndian(w: Word): Word; inline;
    function FixEndian(d: LongWord): LongWord; inline;
    procedure SetFPImgExtras(CurImg: TFPCustomImage; IFD: TTiffIFD);
    procedure DecodePackBits(var Buffer: Pointer; var Count: PtrInt);
    procedure DecodeLZW(var Buffer: Pointer; var Count: PtrInt);
    procedure DecodeDeflate(var Buffer: Pointer; var Count: PtrInt; ExpectedCount: PtrInt);
  protected
    procedure InternalRead(Str: TStream; AnImage: TFPCustomImage); override;
    function InternalCheck(Str: TStream): boolean; override;
    procedure DoCreateImage(ImgFileDir: TTiffIFD); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;

    procedure LoadFromStream(aStream: TStream; AutoClear: boolean = true); //load all images (you need to handle OnCreateImage event and assign ImgFileDir.Img)
    {$ifdef FPC_Debug_Image}
    property Debug: boolean read FDebug write FDebug;
    {$endif}
    property OnCreateImage: TTiffCreateCompatibleImgEvent read FOnCreateImage
                                                          write FOnCreateImage;
    property CheckIFDOrder: TTiffCheckIFDOrder read FCheckIFDOrder write FCheckIFDOrder; //check order of IFD entries or not
    function FirstImg: TTiffIFD;
    function GetBiggestImage: TTiffIFD;
    function ImageCount: integer;
    property Images[Index: integer]: TTiffIFD read GetImages; default;

  public //advanced
    ImageList: TFPList; // list of TTiffIFD
    procedure LoadHeaderFromStream(aStream: TStream);
    procedure LoadIFDsFromStream;                  // call LoadHeaderFromStream before
    procedure LoadImageFromStream(Index: integer); // call LoadIFDsFromStream before
    procedure LoadImageFromStream(IFD: TTiffIFD);  // call LoadIFDsFromStream before
    procedure ReleaseStream;
    property StartPos: int64 read fStartPos;
    property TheStream: TStream read s;
    property FirstIFDStart: LongWord read FFirstIFDStart;
  end;

procedure DecompressPackBits(Buffer: Pointer; Count: PtrInt;
  out NewBuffer: Pointer; out NewCount: PtrInt);
procedure DecompressLZW(Buffer: Pointer; Count: PtrInt;
  out NewBuffer: PByte; out NewCount: PtrInt);
function DecompressDeflate(Compressed: PByte; CompressedCount: LongWord;
  out Decompressed: PByte; var DecompressedCount: LongWord;
  ErrorMsg: PAnsiString = nil): boolean;

Implementation
End.

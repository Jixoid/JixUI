// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
    The original file is part of the Free Pascal run time library.
    Copyright (c) 2012 by the Free Pascal development team

    Tiff writer for fpImage modified by circular.

 **********************************************************************

 Working:
   Grayscale 8,16bit (optional alpha),
   RGB 8,16bit (optional alpha),
   Orientation,
   multiple images, pages
   thumbnail
   Compression: deflate

 ToDo:
   Compression: LZW, packbits, jpeg, ...
   Planar
   ColorMap
   separate mask
   fillorder - not needed by baseline tiff reader
   bigtiff 64bit offsets
   endian - currently using system endianess
   orientation with rotation
}
unit BGRAWriteTiff;

{$mode objfpc}{$H+}

interface

uses
  Math, BGRAClasses, SysUtils, zbase, zdeflate, FPimage, FPTiffCmn,
  BGRABitmapTypes;

type

  { TTiffWriterEntry }

  TTiffWriterEntry = class
  public
    Tag: Word;
    EntryType: Word;
    Count: LongWord;
    Data: Pointer;
    DataPos: LongWord;
    Bytes: LongWord;
    destructor Destroy; override;
  end;

  TTiffWriterChunk = record
    Data: Pointer;
    Bytes: LongWord;
  end;
  PTiffWriterChunk = ^TTiffWriterChunk;

  { TTiffWriterChunkOffsets }

  TTiffWriterChunkOffsets = class(TTiffWriterEntry)
  public
    Chunks: PTiffWriterChunk;
    ChunkByteCounts: TTiffWriterEntry;
    constructor Create(ChunkType: TTiffChunkType);
    destructor Destroy; override;
    procedure SetCount(NewCount: LongWord);
  end;

  { TBGRAWriterTiff }

  TBGRAWriterTiff = class(TFPCustomImageWriter)
  private
    FPremultiplyRGB: boolean;
    FSaveCMYKAsRGB: boolean;
    fStartPos: Int64;
    FEntries: TFPList; // list of TFPList of TTiffWriterEntry
    fStream: TStream;
    fPosition: LongWord;
    procedure ClearEntries;
    procedure WriteTiff;
    procedure WriteHeader;
    procedure WriteIFDs;
    procedure WriteEntry(Entry: TTiffWriterEntry);
    procedure WriteData;
    procedure WriteEntryData(Entry: TTiffWriterEntry);
    procedure WriteBuf(var Buf; Count: LongWord);
    procedure WriteWord(w: Word);
    procedure WriteDWord(d: LongWord);
  protected
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;
    procedure AddEntryString(Tag: word; const s: string);
    procedure AddEntryShort(Tag: word; Value: Word);
    procedure AddEntryLong(Tag: word; Value: LongWord);
    procedure AddEntryShortOrLong(Tag: word; Value: LongWord);
    procedure AddEntryRational(Tag: word; const Value: TTiffRational);
    procedure AddEntry(Tag: Word; EntryType: Word; EntryCount: LongWord;
                       Data: Pointer; Bytes: LongWord;
                       CopyData: boolean = true);
    procedure AddEntry(Entry: TTiffWriterEntry);
    procedure TiffError(Msg: string);
    procedure EncodeDeflate(var Buffer: Pointer; var Count: LongWord);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    procedure AddImage(Img: TFPCustomImage);
    procedure SaveToStream(Stream: TStream);
    property SaveCMYKAsRGB: boolean read FSaveCMYKAsRGB write FSaveCMYKAsRGB;
    property PremultiplyRGB: boolean read FPremultiplyRGB write FPremultiplyRGB;
  end;

function CompareTiffWriteEntries(Entry1, Entry2: Pointer): integer;

function CompressDeflate(InputData: PByte; InputCount: LongWord;
  out Compressed: PByte; var CompressedCount: LongWord;
  ErrorMsg: PAnsiString = nil): boolean;

Implementation
End.

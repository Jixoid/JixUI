// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAUTF8;

{$mode objfpc}{$H+}
{$i bgrabitmap.map.inc}

interface

uses
  BGRAClasses, SysUtils, math, BGRAUnicode{$IFDEF BGRABITMAP_USE_LCL}, lazutf8classes{$ENDIF};

const
  UTF8_ARABIC_ALEPH = 'ا';
  UTF8_ARABIC_ALEPH_HAMZA_BELOW = 'إ';
  UTF8_ARABIC_ALEPH_HAMZA_ABOVE = 'أ';
  UTF8_ARABIC_ALEPH_MADDA_ABOVE = 'آ';
  UTF8_ARABIC_LAM = 'ل';
  UTF8_NO_BREAK_SPACE = ' ';
  UTF8_ZERO_WIDTH_NON_JOINER = '‌';
  UTF8_ZERO_WIDTH_JOINER = '‍';
  UTF8_LINE_SEPARATOR = #$E2#$80#$A8;       //equivalent of <br>
  UTF8_PARAGRAPH_SEPARATOR = #$E2#$80#$A9;  //equivalent of </p>
  UTF8_NEXT_LINE = #$C2#$85;                //equivalent of CRLF

{$IFDEF BGRABITMAP_USE_LCL}
type
  TFileStreamUTF8 = lazutf8classes.TFileStreamUTF8;
  TStringListUTF8 = lazutf8classes.TStringListUTF8;
{$ELSE}
type
  TFileStreamUTF8 = class(THandleStream)
  private
    FFileName: utf8string;
  public
    constructor Create(const AFileName: utf8string; Mode: Word); overload;
    constructor Create(const AFileName: utf8string; Mode: Word; Rights: LongWord); overload;
    destructor Destroy; override;
    property FileName: utf8string Read FFilename;
  end;

  TStringListUTF8 = class(TStringList)
  protected
    function DoCompareText(const s1,s2 : string) : PtrInt; override;
  public
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
  end;
{$ENDIF}

procedure LoadStringsFromFileUTF8(List: TStrings; const FileName: string);
procedure SaveStringsToFileUTF8(List: TStrings; const FileName: string);

function UTF8ToSys(const s: string): string;
function SysToUTF8(const s: string): string;

function UTF8LowerCase(const s: string): string;
function UTF8UpperCase(const s: string): string;

function UTF8CompareStr(const S1, S2: string): Integer;
function UTF8CompareText(const S1, S2: string): Integer;

function UTF8CharStart(UTF8Str: PChar; Len, CharIndex: PtrInt): PChar;

function FileOpenUTF8(Const FileName : string; Mode : Integer) : THandle;
function FileCreateUTF8(Const FileName : string) : THandle; overload;
function FileCreateUTF8(Const FileName : string; Rights: LongWord) : THandle; overload;
function FileExistsUTF8(Const FileName : string): boolean;
function DeleteFileUTF8(Const FileName : string): boolean;
function FindFirstUTF8(const Path: string; Attr: Longint; out Rslt: TSearchRec): Longint;
function FindNextUTF8(var Rslt: TSearchRec): Longint;
procedure FindCloseUTF8(var F: TSearchrec);

type
  string4 = string[4];
  TUnicodeArray = packed array of LongWord;
  TIntegerArray = array of integer;

function UTF8CharacterLength(p: PChar): integer;
function UTF8Length(const s: string): PtrInt; overload;
function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt; overload;
function UnicodeCharToUTF8(u: LongWord): string4;
function UTF8ReverseString(const s: string): string;
function UTF8CodepointToUnicode(p: PChar; ACodePointLen: integer): LongWord;
function UTF8ToUTF16(const S: AnsiString): UnicodeString;
function UTF16ToUTF8(const S: UnicodeString): AnsiString;
procedure UTF8ToUnicodeArray(const sUTF8: string; out u: TUnicodeArray; out ofs: TIntegerArray);

type
  TBidiUTF8Info = packed record
    Offset: Integer;
    BidiInfo: TUnicodeBidiInfo;
  end;
  TBidiUTF8Array = packed array of TBidiUTF8Info;
  TUnicodeDisplayOrder = BGRAUnicode.TUnicodeDisplayOrder;
  TUnicodeBidiInfo = BGRAUnicode.TUnicodeBidiInfo;

function GetBidiClassUTF8(P: PChar): TUnicodeBidiClass;
function GetFirstStrongBidiClassUTF8(const sUTF8: string): TUnicodeBidiClass;
function GetLastStrongBidiClassUTF8(const sUTF8: string): TUnicodeBidiClass;
function IsRightToLeftUTF8(const sUTF8: string): boolean;
function IsZeroWidthUTF8(const sUTF8: string): boolean;
function AddParagraphBidiUTF8(s: string; ARightToLeft: boolean): string;
function AnalyzeBidiUTF8(const sUTF8: string): TBidiUTF8Array; overload;
function AnalyzeBidiUTF8(const sUTF8: string; ABidiMode: TFontBidiMode): TBidiUTF8Array; overload;
function AnalyzeBidiUTF8(const sUTF8: string; ARightToLeft: boolean): TBidiUTF8Array; overload;
function GetUTF8DisplayOrder(const ABidi: TBidiUTF8Array): TUnicodeDisplayOrder;
function ContainsBidiIsolateOrFormattingUTF8(const sUTF8: string): boolean;

function UTF8OverrideDirection(const sUTF8: string; ARightToLeft: boolean): string;
function UTF8EmbedDirection(const sUTF8: string; ARightToLeft: boolean): string;
function UTF8Ligature(const sUTF8: string; ARightToLeft: boolean; ALigatureLeft, ALigatureRight: boolean): string;

type

  { TGlyphUtf8 }

  TGlyphUtf8 = record
  private
    function GetEmpty: boolean;
  public
    GlyphUtf8, MirroredGlyphUtf8: string;
    RightToLeft, Mirrored, Merged: boolean;
    ByteOffset, ByteSize: integer;
    property Empty: boolean read GetEmpty;
  end;

  { TGlyphCursorUtf8 }

  TGlyphCursorUtf8 = record
  private
    sUTF8: string;
    currentChar: string;
    currentOffset: integer;
    currentBidiInfo: TUnicodeBidiInfo;
    bidiArray: TBidiUTF8Array;
    displayOrder: TUnicodeDisplayOrder;
    displayIndex: Integer;
    procedure NextMultichar;
    procedure PeekMultichar;
  public
    class function New(const textUTF8: string; ABidiMode: TFontBidiMode): TGlyphCursorUtf8; static;
    function GetNextGlyph: TGlyphUtf8;
    procedure Rewind;
    function EndOfString: boolean;
  end;

//little endian stream functions
function LEReadInt64(Stream: TStream): int64;
procedure LEWriteInt64(Stream: TStream; AValue: int64);
function LEReadLongint(Stream: TStream): longint;
procedure LEWriteLongint(Stream: TStream; AValue: LongInt);
function LEReadByte(Stream: TStream): byte;
procedure LEWriteByte(Stream: TStream; AValue: Byte);
function LEReadSingle(Stream: TStream): single;
procedure LEWriteSingle(Stream: TStream; AValue: single);

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAMemDirectory;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRAMultiFileType, fgl;

const
  MemDirectoryFileHeader = 'TMemDirectory'#26#0#0;
  MemDirectoryEntry_FlagDirectory = 1;   //entry is a directory
  MemDirectoryEntry_FlagCompressed = 2;  //the stream is compressed
  MemDirectoryEntry_FlagSmallEntryPacked = $8000; //name and size <= 255

type
  TMemDirectory = class;
  TEntryFilename = BGRAMultiFileType.TEntryFilename;

type
  TMemDirectoryPath = specialize TFPGList<TEntryFilename>;

  { TMemDirectoryEntry }

  TMemDirectoryEntry = class(TMultiFileEntry)
  private
    FStream: TStream;
    function GetIsCompressed: boolean;
    function GetCompressedSize: int64;
    function GetIsDirectory: boolean;
    procedure SetIsCompressed(AValue: boolean);
    procedure LoadExtraFromEmbeddedStream(ADataStream: TStream; AStartPos: int64);
    procedure SaveToEmbeddedStream(AEntryStream, ADataStream: TStream; AStartPos: int64; out uncompressedSize: int64);
  protected
    FFlags: Word;
    FName,FExtension: utf8String;
    FUncompressedSize: int64;
    FEmbeddedStreamPos: int64;
    FMemDirectory: TMemDirectory;
    function GetName: utf8string; override;
    procedure SetName(AValue: utf8string); override;
    function GetFileSize: int64; override;
    function GetExtension: utf8string; override;
    function InternalCopyTo({%H-}ADestination: TStream): int64;
  public
    function CopyTo({%H-}ADestination: TStream): int64; override;
    function GetStream: TStream; override;
    constructor Create(AContainer: TMultiFileContainer; AFilename: TEntryFilename; AUncompressedStream: TStream; AOwnStream: boolean); overload;
    constructor CreateDirectory(AContainer: TMultiFileContainer; AFilename: TEntryFilename);
    constructor CreateFromData(AContainer: TMultiFileContainer; AFilename: TEntryFilename; AStream: TStream; AOwnStream: boolean; AUncompressedSize: int64; AFlags: Word);
    destructor Destroy; override;
    property EmbeddedStreamPos: int64 read FEmbeddedStreamPos write FEmbeddedStreamPos;
    property IsCompressed: boolean read GetIsCompressed write SetIsCompressed;
    property IsDirectory: boolean read GetIsDirectory;
    property CompressedSize: int64 read GetCompressedSize;
    property Flags: Word read FFlags;
    property MemDirectory: TMemDirectory read FMemDirectory;
  end;

  TMemDirectory = class(TMultiFileContainer)
  private
    FParentDirectory: TMemDirectory;
    function GetEntryCompressed(AIndex: integer): boolean;
    function GetIsDirectory(AIndex: integer): boolean;
    function GetDirectory(AIndex: integer): TMemDirectory;
    procedure SetEntryCompressed(AIndex: integer; AValue: boolean);
  protected
    function CreateEntry(AName: utf8string; AExtension: utf8string; AContent: TStream): TMultiFileEntry; override;
    function SplitPath(APath: utf8string): TMemDirectoryPath;
  public
    constructor Create(AParentDirectory: TMemDirectory = nil);
    function Equals(Obj: TObject): boolean; override;
    procedure LoadFromStream(AStream: TStream); override;
    class function CheckHeader(AStream: TStream): boolean; static;
    procedure LoadFromEmbeddedStream(ARootStream, ADataStream: TStream; AStartPos: int64);
    procedure SaveToStream(ADestination: TStream); override;
    procedure SaveToEmbeddedStream(ARootDest, ADataDest: TStream; AStartPos: int64);
    function AddDirectory(AName: utf8string; AExtension: utf8string= ''; ACaseSensitive: boolean= true): integer;
    function Rename(AName: utf8string; AExtension: utf8string; ANewName: utf8string; ACaseSensitive: boolean= true): boolean;
    function FindPath(APath: utf8String; ACaseSensitive: boolean = true): TMemDirectory;
    function FindEntry(APath: utf8String; ACaseSensitive: boolean = true): TMemDirectoryEntry;
    procedure CopyTo(ADest: TMemDirectory; ARecursive: boolean);
    property IsEntryCompressed[AIndex: integer]: boolean read GetEntryCompressed write SetEntryCompressed;
    property Directory[AIndex: integer]: TMemDirectory read GetDirectory;
    property IsDirectory[AIndex: integer]: boolean read GetIsDirectory;
    property ParentDirectory: TMemDirectory read FParentDirectory;
  end;

Implementation
End.

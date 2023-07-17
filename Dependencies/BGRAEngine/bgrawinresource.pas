// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAWinResource;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRAMultiFileType, BGRABitmapTypes, BGRAReadBMP;

const
  RT_CURSOR = 1;
  RT_BITMAP = 2;
  RT_ICON = 3;

  RT_MENU = 4;
  RT_DIALOG = 5;
  RT_STRING = 6;
  RT_FONTDIR = 7;
  RT_FONT = 8;
  RT_ACCELERATOR = 9;
  RT_RCDATA = 10;
  RT_MESSAGETABLE = 11;

  RT_GROUP = 11;
  RT_GROUP_CURSOR = RT_GROUP + RT_CURSOR;
  RT_GROUP_ICON = RT_GROUP + RT_ICON;

  RT_VERSION = 16;
  RT_ANICURSOR = 21;
  RT_ANIICON = 22;
  RT_HTML = 23;
  RT_MANIFEST = 24;

  ICON_OR_CURSOR_FILE_ICON_TYPE = 1;
  ICON_OR_CURSOR_FILE_CURSOR_TYPE = 2;

type
  TNameOrId = record
    Id: integer;
    Name: utf8string;
  end;

  { TResourceInfo }

  TResourceInfo = object
    DataVersion: LongWord;
    MemoryFlags: Word;
    LanguageId: Word;
    Version: LongWord;
    Characteristics: LongWord;
    procedure SwapIfNecessary;
  end;

  TWinResourceContainer = class;

  { TCustomResourceEntry }

  TCustomResourceEntry = class(TMultiFileEntry)
  private
    class function GetNextEntry(AContainer: TMultiFileContainer; AStream: TStream): TCustomResourceEntry; static;
    procedure Serialize(ADestination: TStream);
  protected
    FTypeNameOrId: TNameOrId;
    FEntryNameOrId: TNameOrId;
    FResourceInfo: TResourceInfo;
    FReferenceCount: integer;
    function GetName: utf8string; override;
    procedure SetName(AValue: utf8string); override;
    function GetId: integer;
    procedure SetId(AValue: integer);
    function GetTypeId: integer;
    function GetTypeName: utf8string;
    procedure IncrementReferences; virtual;
    procedure DecrementReferences; virtual;
    procedure SerializeHeader(ADestination: TStream); virtual;
    procedure SerializeData(ADestination: TStream); virtual; abstract;
    function GetDataSize: integer; virtual; abstract;
    function GetLanguageId: integer;
    procedure SetLanguageId(AValue: integer);
  public
    constructor Create(AContainer: TMultiFileContainer; ATypeNameOrId: TNameOrId; AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo);
    function GetStream: TStream; override;
    property Id: integer read GetId write SetId;
    property TypeName: utf8string read GetTypeName;
    property TypeId: integer read GetTypeId;
    property LanguageId: integer read GetLanguageId write SetLanguageId;
  end;

  { TUnformattedResourceEntry }

  TUnformattedResourceEntry = class(TCustomResourceEntry)
  protected
    FDataStream: TStream;
    function GetFileSize: int64; override;
    function GetDataSize: integer; override;
    procedure SerializeData(ADestination: TStream); override;
    function GetExtension: utf8string; override;
  public
    constructor Create(AContainer: TMultiFileContainer; ATypeNameOrId: TNameOrId; AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo; ADataStream: TStream);
    destructor Destroy; override;
    function CopyTo(ADestination: TStream): int64; override;
    function GetStream: TStream; override;
  end;

  { TBitmapResourceEntry }

  TBitmapResourceEntry = class(TUnformattedResourceEntry)
  protected
    function GetFileSize: int64; override;
    function GetExtension: utf8string; override;
  public
    constructor Create(AContainer: TMultiFileContainer; AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo; ADataStream: TStream);
    function CopyTo(ADestination: TStream): int64; override;
    procedure CopyFrom(ASource: TStream);
  end;

  { TGroupIconHeader }

  TGroupIconHeader = object
    Reserved, ResourceType, ImageCount: Word;
    procedure SwapIfNecessary;
  end;
  TGroupIconDirEntry = packed record
    Width, Height, Colors, Reserved: byte;
    //stored in little endian
    case byte of
    0: (Variable: LongWord; ImageSize: LongWord; ImageId: Word);
    1: (Planes, BitsPerPixel: Word);
    2: (HotSpotX, HotSpotY: Word);
  end;
  TIconFileDirEntry = packed record
    Width, Height, Colors, Reserved: byte;
    //stored in little endian
    case byte of
    0: (Variable: LongWord; ImageSize: LongWord; ImageOffset: LongWord);
    1: (Planes, BitsPerPixel: Word);
    2: (HotSpotX, HotSpotY: Word);
  end;

  { TGroupIconOrCursorEntry }

  TGroupIconOrCursorEntry = class(TCustomResourceEntry)
  private
    function GetNbIcons: integer;
  protected
    FGroupIconHeader: TGroupIconHeader;
    FDirectory: packed array of TGroupIconDirEntry;
    function GetFileSize: int64; override;
    function GetDataSize: integer; override;
    procedure SerializeData(ADestination: TStream); override;
    procedure IncrementReferences; override;
    procedure DecrementReferences; override;
    function ExpectedResourceType: word; virtual; abstract;
  public
    constructor Create(AContainer: TMultiFileContainer; ATypeNameOrId: TNameOrId; AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo; ADataStream: TStream);
    constructor Create(AContainer: TMultiFileContainer; ATypeNameOrId: TNameOrId; AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo);
    procedure Clear;
    function CopyTo(ADestination: TStream): int64; override;
    procedure CopyFrom(ASource: TStream);
    property NbIcons: integer read GetNbIcons;
  end;

  { TGroupIconEntry }

  TGroupIconEntry = class(TGroupIconOrCursorEntry)
  protected
    function GetExtension: utf8string; override;
    function ExpectedResourceType: word; override;
  public
    constructor Create(AContainer: TMultiFileContainer; AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo; ADataStream: TStream);
    constructor Create(AContainer: TMultiFileContainer; AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo);
  end;

  { TGroupCursorEntry }

  TGroupCursorEntry = class(TGroupIconOrCursorEntry)
  protected
    function GetExtension: utf8string; override;
    function ExpectedResourceType: word; override;
  public
    constructor Create(AContainer: TMultiFileContainer; AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo; ADataStream: TStream);
    constructor Create(AContainer: TMultiFileContainer; AEntryNameOrId: TNameOrId; const AResourceInfo: TResourceInfo);
  end;

  { TWinResourceContainer }

  TWinResourceContainer = class(TMultiFileContainer)
  private
    function InternalFind(const AEntry: TNameOrId; const AType: TNameOrId; ALanguageId: integer = 0): TCustomResourceEntry;
    procedure AddHidden(AEntry: TCustomResourceEntry);
    function GetMaxId(AType: TNameOrId): integer;
    procedure IncrementReferenceOf(ANameId, ATypeId: integer);
    procedure DecrementReferenceOf(ANameId, ATypeId: integer);
  protected
    FHiddenEntries: TMultiFileEntryList;
    procedure Init; override;
    procedure ClearHiddenEntries;
    procedure RemoveHidden(AEntry: TCustomResourceEntry);
    function CreateEntry(AName: utf8string; AExtension: utf8string; AContent: TStream; ALanguageId: integer): TMultiFileEntry; overload;
    function CreateEntry(AName: utf8string; AExtension: utf8string; AContent: TStream): TMultiFileEntry; override;
  public
    procedure Clear; override;
    destructor Destroy; override;
    procedure Delete(AIndex: integer); override;
    procedure LoadFromStream(AStream: TStream); override;
    function IndexOf(AName: utf8string; AExtenstion: utf8string; ACaseSensitive: boolean = True): integer; override;
    function IndexOf(AName: utf8string; AExtenstion: utf8string; ALanguageId: integer; ACaseSensitive: boolean = True): integer; overload;
    procedure SaveToStream(ADestination: TStream); override;
  end;

Implementation
End.

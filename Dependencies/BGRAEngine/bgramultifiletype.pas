// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAMultiFileType;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  BGRAClasses, SysUtils, fgl;

type

  { TEntryFilename }

  TEntryFilename = record
  private
    FExtension: utf8string;
    FName: utf8string;
    function GetFilename: utf8string;
    function GetIsEmpty: boolean;
    procedure SetExtension(AValue: utf8string);
    procedure SetFilename(AValue: utf8string);
    procedure SetName(AValue: utf8string);
  public
    class operator =(const AValue1,AValue2: TEntryFilename): boolean;
    property Filename: utf8string read GetFilename write SetFilename;
    property Name: utf8string read FName write SetName;
    property Extension: utf8string read FExtension write SetExtension;
    property IsEmpty: boolean read GetIsEmpty;
  end;

function EntryFilename(AName,AExtension: string): TEntryFilename; overload;
function EntryFilename(AFilename: string): TEntryFilename; overload;

type
  TMultiFileContainer = class;

  { TMultiFileEntry }

  TMultiFileEntry = class
  protected
    FContainer: TMultiFileContainer;
    function GetName: utf8string; virtual; abstract;
    procedure SetName(AValue: utf8string); virtual; abstract;
    function GetFileSize: int64; virtual;
    function GetExtension: utf8string; virtual;
  public
    constructor Create(AContainer: TMultiFileContainer);
    function CopyTo({%H-}ADestination: TStream): int64; virtual;
    function GetStream: TStream; virtual;
    function CompareNameAndExtension(AName: utf8string; AExtension: utf8string; ACaseSensitive: boolean = true): integer;
    property Name: utf8string read GetName write SetName;
    property Extension: utf8string read GetExtension;
    property FileSize: int64 read GetFileSize;
    property Container: TMultiFileContainer read FContainer;
  end;

  TMultiFileEntryList = specialize TFPGList<TMultiFileEntry>;

  { TMultiFileContainer }

  TMultiFileContainer = class(TPersistent)
  private
    FEntries: TMultiFileEntryList;
  protected
    procedure Init; virtual;
    function AddEntry(AEntry: TMultiFileEntry; AIndex: integer = -1): integer;
    function GetCount: integer;
    function GetEntry(AIndex: integer): TMultiFileEntry;
    function CreateEntry(AName: utf8string; AExtension: utf8string; AContent: TStream): TMultiFileEntry; virtual; abstract;
    function GetRawString(AIndex: integer): RawByteString;
    function GetRawStringByFilename(AFilename: string): RawByteString;
    procedure SetRawString(AIndex: integer; AValue: RawByteString);
    procedure SetRawStringByFilename(AFilename: string; AValue: RawByteString);
  public
    constructor Create; overload;
    constructor Create(AFilename: utf8string); overload;
    constructor Create(AStream: TStream); overload;
    constructor Create(AStream: TStream; AStartPos: Int64); overload;
    procedure Assign(Source: TPersistent); override;
    function Add(AName: utf8string; AExtension: utf8string; AContent: TStream; AOverwrite: boolean = false; AOwnStream: boolean = true): integer; overload;
    function Add(AName: utf8string; AExtension: utf8string; AContent: RawByteString; AOverwrite: boolean = false): integer; overload;
    function Add(AFilename: TEntryFilename; AContent: TStream; AOverwrite: boolean = false; AOwnStream: boolean = true): integer; overload;
    function Add(AFilename: TEntryFilename; AContent: RawByteString; AOverwrite: boolean = false): integer; overload;
    procedure Clear; virtual;
    destructor Destroy; override;
    procedure LoadFromFile(AFilename: utf8string);
    procedure LoadFromStream(AStream: TStream); virtual; abstract;
    procedure LoadFromResource(AFilename: string); virtual;
    procedure SaveToFile(AFilename: utf8string);
    procedure SaveToStream(ADestination: TStream); virtual; abstract;
    procedure Remove(AEntry: TMultiFileEntry); virtual;
    procedure Delete(AIndex: integer); overload; virtual;
    function Delete(AName: utf8string; AExtension: utf8string; ACaseSensitive: boolean = True): boolean; overload;
    function Delete(AFilename: TEntryFilename; ACaseSensitive: boolean = True): boolean; overload;
    function IndexOf(AEntry: TMultiFileEntry): integer; overload;
    function IndexOf(AName: utf8string; AExtenstion: utf8string; ACaseSensitive: boolean = True): integer; overload; virtual;
    function IndexOf(AFilename: TEntryFilename; ACaseSensitive: boolean = True): integer; overload;
    property Count: integer read GetCount;
    property Entry[AIndex: integer]: TMultiFileEntry read GetEntry;
    property RawString[AIndex: integer]: RawByteString read GetRawString write SetRawString;
    property RawStringByFilename[AFilename: string]: RawByteString read GetRawStringByFilename write SetRawStringByFilename;
  end;

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRADNetDeserial;

{$mode objfpc}{$H+}

interface

{ This unit allow to read .Net serialized classes with BinaryFormatter of
  namespace System.Runtime.Serialization.Formatters.Binary.

  Serialization is a process by which objects in memory are saved according
  to their structure.

  This unit is used by BGRAPaintNet to read Paint.NET images. }

uses
  BGRAClasses, SysUtils;

type
  arrayOfLongword = array of LongWord;

  TTypeCategory = (ftPrimitiveType = 0, ftString = 1, ftObjectType =
    2, ftRuntimeType = 3,
    ftGenericType = 4, ftArrayOfObject = 5, ftArrayOfString = 6,
    ftArrayOfPrimitiveType = 7);

  TPrimitiveType = (ptNone = 0, ptBoolean = 1, ptByte = 2, ptChar = 3, ptDecimal = 5,
    ptDouble = 6, ptInt16 = 7, ptInt32 = 8, ptInt64 = 9, ptSByte = 10, ptSingle = 11,
    ptDateTime = 13, ptUInt16 = 14, ptUInt32 = 15, ptUInt64 = 16, ptString = 18);

  TGenericArrayType = (gatSingleDimension, gatJagged, gatMultidimensional);

  TDotNetDeserialization = class;

  ArrayOfNameValue = array of record
    Name: string;
    Value, valueType: string;
  end;

  TFieldType = record
    category: TTypeCategory;
    primitiveType: TPrimitiveType;
    refAssembly: LongWord;
    Name: string;
  end;

  TSerializedType = record
    ClassName:   string;
    nbFields:    integer;
    fieldNames:  array of string;
    fieldTypes:  array of TFieldType;
    refAssembly: LongWord;
  end;

  TAssemblyReference = record
    idAssembly: LongWord;
    Name: string;
  end;

  { TCustomSerializedObject }

  TCustomSerializedObject = class
  protected
    FContainer: TDotNetDeserialization;
    function GetTypeAsString: string; virtual; abstract;
    function GetFieldAsString(Index: LongWord): string; overload; virtual; abstract;
    function GetFieldAsString(Name: string): string; overload;
    function GetFieldCount: LongWord; virtual; abstract;
    function GetFieldName(Index: LongWord): string; virtual; abstract;
    function GetFieldTypeAsString(Index: LongWord): string; virtual; abstract;
    function IsReferenceType(index: LongWord): boolean; virtual; abstract;
  public
    idObject:   LongWord;
    refCount:   integer;
    inToString: boolean;
    constructor Create(container: TDotNetDeserialization); virtual;
    property FieldCount: LongWord read GetFieldCount;
    property FieldName[Index: LongWord]:string read GetFieldName;
    property FieldAsString[Index: LongWord]: string read GetFieldAsString;
    property FieldByNameAsString[Name: string]: string read GetFieldAsString;
    property FieldTypeAsString[Index: LongWord]: string read GetFieldTypeAsString;
    property TypeAsString: string read GetTypeAsString;
    function GetFieldIndex(Name: string): integer;
  end;

  { TSerializedClass }

  TSerializedClass = class(TCustomSerializedObject)
  protected
    function GetFieldAsString(Index: LongWord): string; override;
    function GetFieldCount: LongWord; override;
    function GetFieldName(Index: LongWord): string; override;
    function GetFieldTypeAsString(Index: LongWord): string; override;
    function IsReferenceType(index: LongWord): boolean; override;
    function GetTypeAsString: string; override;
  public
    numType: integer;
    fields:  ArrayOfNameValue;
  end;

  { TSerializedArray }

  TSerializedArray = class(TCustomSerializedObject)
  private
    data:       pointer;
    FItemSize:   LongWord;
    function GetItemPtr(Index: LongWord): pointer;
    procedure InitData;
  protected
    FArrayType: TGenericArrayType;
    function GetFieldAsString(Index: LongWord): string; override;
    function GetFieldCount: LongWord; override;
    function GetFieldName(Index: LongWord): string; override;
    function GetFieldTypeAsString(Index: LongWord): string; override;
    function IsReferenceType(index: LongWord): boolean; override;
    function GetTypeAsString: string; override;
  public
    dimensions: array of LongWord;
    itemType:   TFieldType;
    nbItems:    LongWord;
    constructor Create(AContainer: TDotNetDeserialization; AItemType: TFieldType; ALength: LongWord); overload;
    constructor Create(AContainer: TDotNetDeserialization; AArrayType: TGenericArrayType; AItemType: TFieldType; ADimensions: arrayOfLongword); overload;
    destructor Destroy; override;
    property ItemPtr[Index:LongWord]: pointer read GetItemPtr;
    property ItemSize: LongWord read FItemSize;
  end;

  { TSerializedValue }

  TSerializedValue = class(TSerializedArray)
  protected
    function GetIsReferenceType: boolean;
    function GetValueAsString: string;
    function GetTypeAsString: string; override;
  public
    constructor Create(AContainer: TDotNetDeserialization; AItemType: TFieldType); overload;
    property ValueAsString: string read GetValueAsString;
    property IsReferenceType: boolean read GetIsReferenceType;
  end;

  { TDotNetDeserialization }
  TDotNetDeserialization = class
    objectTypes: array of TSerializedType;
    assemblies:  array of TAssemblyReference;
    objects:     array of TCustomSerializedObject;

    function FindClass(typeName: string): TSerializedClass;
    function FindObject(typeName: string): TCustomSerializedObject;
    function GetSimpleField(obj: TCustomSerializedObject; Name: string): string;
    function GetObjectField(obj: TCustomSerializedObject; Name: string): TCustomSerializedObject; overload;
    function GetObjectField(obj: TCustomSerializedObject; index: integer): TCustomSerializedObject; overload;
    function GetObject(id: string): TCustomSerializedObject; overload;
    function GetObject(id: LongWord): TCustomSerializedObject; overload;
    function IsBoxedValue(obj: TCustomSerializedObject; index: integer): boolean;
    function GetBoxedValue(obj: TCustomSerializedObject; index: integer): string;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(filename: string);
    procedure LoadFromFileUTF8(filenameUTF8: string);
    function ToString: string; override;
    constructor Create;
    destructor Destroy; override;
    function GetTypeOfClassObject(idObject: LongWord): integer;
  private
    EndOfStream:      boolean;
    ArrayFillerCount: Longword;
    currentAutoObjectValue: LongWord;
    function nextAutoObjectId: LongWord;
    function LoadNextFromStream(Stream: TStream): LongWord;
    function LoadStringFromStream(Stream: TStream): string;
    function LoadDotNetCharFromStream(Stream: TStream): string;
    function LoadTypeFromStream(Stream: TStream; IsRuntimeType: boolean): integer;
    function LoadValuesFromStream(Stream: TStream; numType: integer): ArrayOfNameValue;
    function LoadValueFromStream(Stream: TStream; const fieldType: TFieldType): string;
    function LoadFieldType(Stream: TStream; category: TTypeCategory): TFieldType;
  end;

function WinReadByte(stream: TStream): byte;
function WinReadWord(Stream: TStream): word;
function WinReadSmallInt(Stream: TStream): smallint;
function WinReadLongint(Stream: TStream): longint;
function WinReadLongword(Stream: TStream): LongWord;
function WinReadInt64(Stream: TStream): int64;
function WinReadQWord(Stream: TStream): QWord;

Implementation
End.

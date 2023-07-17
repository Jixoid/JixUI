// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAIconCursor;

{$mode objfpc}{$H+}
{$i bgrabitmap.map.inc}

interface

uses
  BGRAClasses, SysUtils, BGRAMultiFileType, BGRABitmapTypes;

type
  { TBGRAIconCursorEntry }

  TBGRAIconCursorEntry = class(TMultiFileEntry)
  protected
    FWidth,FHeight,FBitDepth: integer;
    FExtension: string;
    FContent: TStream;
    FHotSpot: TPoint;
    function GetName: utf8string; override;
    procedure SetName({%H-}AValue: utf8string); override;
    function GetExtension: utf8string; override;
    function GetFileSize: int64; override;
  public
    constructor Create(AContainer: TMultiFileContainer; AExtension: string; AInfo: TQuickImageInfo; AContent: TStream);
    class function TryCreate(AContainer: TMultiFileContainer; AContent: TStream): TBGRAIconCursorEntry; static;
    destructor Destroy; override;
    function CopyTo(ADestination: TStream): int64; override;
    function GetStream: TStream; override;
    function GetBitmap: TBGRACustomBitmap;
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property BitDepth: integer read FBitDepth;
    property HotSpot: TPoint read FHotSpot write FHotSpot;
  end;

  { TBGRAIconCursor }

  TBGRAIconCursor = class(TMultiFileContainer)
  private
    function GetBitDepthAt(AIndex: integer): integer;
    function GetHeightAt(AIndex: integer): integer;
    function GetHotSpotAtAt(AIndex: integer): TPoint;
    function GetWidthAt(AIndex: integer): integer;
    procedure SetFileType(AValue: TBGRAImageFormat);
    procedure SetHotSpotAt(AIndex: integer; AValue: TPoint);
  protected
    FFileType : TBGRAImageFormat;
    FLoading : boolean;
    function CreateEntry(AName: utf8string; AExtension: utf8string;
      AContent: TStream): TMultiFileEntry; override;
    function ExpectedMagic: Word;
    procedure Init; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AFileType: TBGRAImageFormat); overload;
    procedure Assign(Source: TPersistent); override;
    function Add(ABitmap: TBGRACustomBitmap; ABitDepth: integer; AOverwrite: boolean = false): integer; overload;
    function Add(AContent: TStream; AOverwrite: boolean = false; AOwnStream: boolean = true): integer; overload;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(ADestination: TStream); override;
    function GetBitmap(AIndex: integer): TBGRACustomBitmap;
    function GetBestFitBitmap(AWidth,AHeight: integer): TBGRACustomBitmap;
    function IndexOf(AWidth,AHeight,ABitDepth: integer): integer; overload;
    property FileType: TBGRAImageFormat read FFileType write SetFileType;
    property Width[AIndex: integer]: integer read GetWidthAt;
    property Height[AIndex: integer]: integer read GetHeightAt;
    property BitDepth[AIndex: integer]: integer read GetBitDepthAt;
    property HotSpot[AIndex: integer]: TPoint read GetHotSpotAtAt write SetHotSpotAt;
  end;

function BGRADitherIconCursor(ABitmap: TBGRACustomBitmap; ABitDepth: integer; ADithering: TDitheringAlgorithm): TBGRACustomBitmap;
function BGRABitDepthIconCursor(ABitmap: TBGRACustomBitmap): integer;

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRALazResource;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRAMultiFileType;

type
  { TLazResourceEntry }

  TLazResourceEntry = class(TMultiFileEntry)
  private
    procedure Serialize(ADestination: TStream);
  protected
    FName: utf8string;
    FValueType: utf8string;
    FContent: TStream;
    function GetName: utf8string; override;
    procedure SetName(AValue: utf8string); override;
    function GetExtension: utf8string; override;
    function GetFileSize: int64; override;
  public
    constructor Create(AContainer: TMultiFileContainer; AName: utf8string; AValueType: utf8string; AContent: TStream);
    destructor Destroy; override;
    function CopyTo(ADestination: TStream): int64; override;
    function GetStream: TStream; override;
  end;

  { TFormDataEntry }

  TFormDataEntry = class(TLazResourceEntry)
  protected
    FTextContent: TStream;
    procedure RequireTextContent;
    function GetExtension: utf8string; override;
    function GetFileSize: int64; override;
  public
    constructor Create(AContainer: TMultiFileContainer; AName: utf8string; ABinaryContent: TStream);
    destructor Destroy; override;
    function CopyTo(ADestination: TStream): int64; override;
  end;

  { TLazResourceContainer }

  TLazResourceContainer = class(TMultiFileContainer)
  protected
    function CreateEntry(AName: utf8string; AExtension: utf8string; AContent: TStream): TMultiFileEntry; override;
  public
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(ADestination: TStream); override;
  end;

Implementation
End.

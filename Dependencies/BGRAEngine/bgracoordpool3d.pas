// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRACoordPool3D;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, BGRABitmapTypes, BGRASSE, BGRAMatrix3D;

type
  PBGRACoordData3D = ^TBGRACoordData3D;
  TBGRACoordData3D = packed record
    {0} sceneCoord: TPoint3D_128;
    {16} viewCoord: TPoint3D_128;
    {32} projectedCoord: TPointF;
    {40} InvZ: single;
    {44} used: wordbool; customNormalUsed: wordbool;
    {48} viewNormal: TPoint3D_128;
    {64} customNormal: TPoint3D_128;
  end; {80}

  PBGRANormalData3D = ^TBGRANormalData3D;
  TBGRANormalData3D = packed record
    {0} customNormal: TPoint3D_128;
    {16} viewNormal: TPoint3D_128;
    {32} used: longbool;
    {36} filler1,filler2,filler3: LongWord;
  end; {48}

  { TBGRAGenericPool }

  TBGRAGenericPool = class
  private
    FFirstFree: integer;
    FNbElements,FCapacity: integer;
    FElementSize: PtrInt;
    FUsedCapacity : integer;
    function GetElement(AIndex: integer): Pointer;
    procedure SetCapacity(ACapacity: integer);
  protected
    FPoolData: TMemoryBlockAlign128;
    function GetUsed({%H-}AElement: integer): boolean; virtual;
    procedure SetUsed({%H-}AElement: integer; {%H-}AUsed: boolean); virtual;
    procedure Remove(AIndex: integer); //does not work if GetUsed/SetUsed are not implemented
  public
    constructor Create(ACapacity: integer; AElementSize: integer);
    destructor Destroy; override;
    function Add: integer;
    property Element[AIndex: integer]: Pointer read GetElement;
    property Capacity: integer read FCapacity;
    property UsedCapacity: integer read FUsedCapacity;
  end;

  { TBGRACoordPool3D }

  TBGRACoordPool3D = class(TBGRAGenericPool)
  private
    function GetCoordData(AIndex: integer): PBGRACoordData3D;
  protected
    function GetUsed(AElement: integer): boolean; override;
    procedure SetUsed(AElement: integer; AUsed: boolean); override;
  public
    procedure Remove(AIndex: integer);
    constructor Create(ACapacity: integer);
    procedure ComputeWithMatrix(const AMatrix: TMatrix3D; const AProjection: TProjection3D);
    property CoordData[AIndex: integer]: PBGRACoordData3D read GetCoordData;
  end;

  { TBGRANormalPool3D }

  TBGRANormalPool3D = class(TBGRAGenericPool)
  private
    function GetNormalData(AIndex: integer): PBGRANormalData3D;
  protected
    function GetUsed(AElement: integer): boolean; override;
    procedure SetUsed(AElement: integer; AUsed: boolean); override;
  public
    procedure Remove(AIndex: integer);
    constructor Create(ACapacity: integer);
    procedure ComputeWithMatrix(const AMatrix: TMatrix3D);
    property NormalData[AIndex: integer]: PBGRANormalData3D read GetNormalData;
  end;

Implementation
End.

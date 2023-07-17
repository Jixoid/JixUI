// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRASSE;

{$mode objfpc}{$H+}

{$i bgrasse.map.inc}
{$modeswitch advancedrecords}

interface

{begin  //param: eax, edx, ecx  //float: eax ecx edx
  //flds $d9
  //fadds $d8
  //fstps $d9 +$18
  //fmuls $d8 +$08
  //fsubrs $d8 +$28
  //offset +$40 $..}
uses
  BGRABitmapTypes {$ifdef CPUI386}, cpu, mmx{$endif};

const FLAG_ENABLED_SSE = true;

var UseSSE, UseSSE2, UseSSE3 : boolean;

{$ifdef CPUI386}
  {$asmmode intel}
{$ENDIF}
{$ifdef cpux86_64}
  {$asmmode intel}
{$ENDIF}

{$ifdef BGRASSE_AVAILABLE}
  //SSE rotate singles
  const Shift231 = 1 + 8;
        Shift312 = 2 + 16;
{$endif}

type

  { TPoint3D_128 }

  TPoint3D_128 = packed record
                   x,y,z,t: single;
                   procedure Offset(const point3D_128: TPoint3D_128);
                   procedure Scale(AScale: single);
                 end;
  PPoint3D_128 = ^TPoint3D_128;

  function Point3D(const point3D_128: TPoint3D_128): TPoint3D; inline; overload;
  function Point3D_128(const point3D: TPoint3D): TPoint3D_128; inline; overload;
  function Point3D_128(const pointF: TPointF): TPoint3D_128; inline; overload;
  function Point3D_128(x,y,z: single): TPoint3D_128; inline; overload;
  function Point3D_128(x,y,z,t: single): TPoint3D_128; inline; overload;
  procedure Normalize3D_128_SqLen(var v: TPoint3D_128; out SqLen: single);
  operator * (const v1: TPoint3D_128; const factor: single): TPoint3D_128;
  operator + (constref v1,v2: TPoint3D_128): TPoint3D_128;
  operator - (const v1,v2: TPoint3D_128): TPoint3D_128;
  operator - (const v: TPoint3D_128): TPoint3D_128; inline;
  operator = (const v1,v2: TPoint3D_128): boolean; inline;
  procedure ClearPoint3D_128(out v: TPoint3D_128);
  {$IFDEF BGRASSE_AVAILABLE}
  procedure ClearPoint3D_128_AlignedSSE(out v: TPoint3D_128);
  {$ENDIF}
  function IsPoint3D_128_Zero(const v: TPoint3D_128): boolean; inline;

var
  Add3D_Aligned : procedure (var dest: TPoint3D_128; constref src: TPoint3D_128);
  Normalize3D_128 : procedure (var v: TPoint3D_128);
  VectProduct3D_128 : procedure (const u,v: TPoint3D_128; out w: TPoint3D_128);
  DotProduct3D_128 : function (constref v1,v2: TPoint3D_128): single;

const
  Point3D_128_Zero : TPoint3D_128 = (x:0; y:0; z:0; t:0);

type

  { TMemoryBlockAlign128 }

  TMemoryBlockAlign128 = class
  private
    FContainer: Pointer;
    FData: Pointer;
  public
    constructor Create(size: integer);
    destructor Destroy; override;
    property Data: pointer read FData;
  end;

  PBasicLightingContext = ^TBasicLightingContext;
  TBasicLightingContext = packed record
    {0} Position, {16} Normal: TPoint3D_128;
    {32} PositionInvZ, {48} NormalInvZ: TPoint3D_128;
    {64} PositionStepInvZ, {80} NormalStepInvZ: TPoint3D_128;
    {96} dummy4: single;
    {100} dummy3: LongBool;
    {104} dummy1: LongWord;
    {108} dummy2: LongWord;
    {112} dummy: packed array[0..15]of byte;
  end; {128}

const ExtendedLightingContextSize = 128;

Implementation
End.

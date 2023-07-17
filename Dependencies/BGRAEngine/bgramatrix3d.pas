// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAMatrix3D;

{$mode objfpc}{$H+}

{$i bgrasse.map.inc}

{$ifdef CPUI386}
  {$asmmode intel}
{$ENDIF}
{$ifdef cpux86_64}
  {$asmmode intel}
{$ENDIF}

interface

uses
  BGRABitmapTypes, BGRASSE,
  BGRATransform;

type
  TMatrix3D = packed array[1..3,1..4] of single;
  TMatrix4D = packed array[1..4,1..4] of single;
  TProjection3D = packed record
    Zoom, Center: TPointF;
  end;
  TComputeProjectionFunc = function(AViewCoord: TPoint3D_128): TPointF of object;

operator*(const A: TMatrix3D; const M: TPoint3D): TPoint3D;
operator*(constref A: TMatrix3D; var M: TPoint3D_128): TPoint3D_128;
function MultiplyVect3DWithoutTranslation(constref A: TMatrix3D; constref M: TPoint3D_128): TPoint3D_128;
operator*(A,B: TMatrix3D): TMatrix3D;

function Matrix3D(m11,m12,m13,m14, m21,m22,m23,m24, m31,m32,m33,m34: single): TMatrix3D; overload;
function Matrix3D(vx,vy,vz,ofs: TPoint3D): TMatrix3D; overload;
function Matrix3D(vx,vy,vz,ofs: TPoint3D_128): TMatrix3D; overload;
function MatrixIdentity3D: TMatrix3D;
function MatrixInverse3D(A: TMatrix3D): TMatrix3D;
function MatrixTranslation3D(ofs: TPoint3D): TMatrix3D;
function MatrixScale3D(size: TPoint3D): TMatrix3D;
function MatrixRotateX(angle: single): TMatrix3D;
function MatrixRotateY(angle: single): TMatrix3D;
function MatrixRotateZ(angle: single): TMatrix3D;

operator *(const A, B: TMatrix4D): TMatrix4D;
function MatrixIdentity4D: TMatrix4D;
function AffineMatrixToMatrix4D(AValue: TAffineMatrix): TMatrix4D;

{$IFDEF BGRASSE_AVAILABLE}
procedure Matrix3D_SSE_Load(const A: TMatrix3D);
procedure MatrixMultiplyVect3D_SSE_Aligned(var M: TPoint3D_128; out N: TPoint3D_128);
procedure MatrixMultiplyVect3D_SSE3_Aligned(var M: TPoint3D_128; out N: TPoint3D_128);
procedure MatrixMultiplyVect3DWithoutTranslation_SSE_Aligned(var M: TPoint3D_128; out N: TPoint3D_128);
procedure MatrixMultiplyVect3DWithoutTranslation_SSE3_Aligned(var M: TPoint3D_128; out N: TPoint3D_128);
{$ENDIF}

Implementation
End.

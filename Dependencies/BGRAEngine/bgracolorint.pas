// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAColorInt;

{$mode objfpc}{$H+}
{$ifdef CPUI386}
  {$define BGRACOLORINT_USEASM}
{$endif}
{$ifdef DARWIN}
  {$undef BGRACOLORINT_USEASM}
{$endif}

interface

uses
  BGRABitmapTypes;

type
  TColorInt65536 = packed record
    r,g,b,a: integer;
  end;

function ColorInt65536(r,g,b,a: integer): TColorInt65536; inline; overload;
function ColorInt65536(r,g,b: integer): TColorInt65536; inline; overload;
function ColorFToColorInt65536(colorF: TColorF): TColorInt65536; inline;
function ColorInt65536ToColorF(color: TColorInt65536): TColorF;
operator +(const color1,color2: TColorInt65536): TColorInt65536; inline;
operator *(const color1,color2: TColorInt65536): TColorInt65536;
operator *(const color1: TColorInt65536; factor65536: integer): TColorInt65536;
function ColorIntToBGRA(const AColor: TColorInt65536; AGammaCompression: boolean = false): TBGRAPixel;
function BGRAToColorInt(const AColor: TBGRAPixel; AGammaExpansion: boolean = false): TColorInt65536;
function BGRAToColorIntMultiply(const color1: TBGRAPixel; const color2: TColorInt65536): TColorInt65536;

Implementation
End.

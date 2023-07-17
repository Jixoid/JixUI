// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAResample;

{$mode objfpc}{$H+}
{$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
interface

{ This unit provides resampling functions, i.e. resizing of bitmaps with or
  without interpolation filters.

  SimpleStretch does a boxed resample with limited antialiasing.

  FineResample uses floating point coordinates to get an antialiased resample.
  It can use minimal interpolation (4 pixels when upsizing) for simple interpolation
  filters (linear and cosine-like) or wide kernel resample for complex interpolation.
  In this cas, it calls WideKernelResample.

  WideKernelResample can be called by custom filter kernel, derived
  from TWideKernelFilter. It is slower of course than simple interpolation. }

uses
  SysUtils, BGRABitmapTypes;

{------------------------------- Simple stretch ------------------------------------}

function SimpleStretch(bmp: TBGRACustomBitmap;
  NewWidth, NewHeight: integer): TBGRACustomBitmap;
procedure StretchPutImage(bmp: TBGRACustomBitmap;
  NewWidth, NewHeight: integer; dest: TBGRACustomBitmap; OffsetX,OffsetY: Integer; ADrawMode: TDrawMode; AOpacity: byte; ANoTransition: boolean = false);
procedure DownSamplePutImage(source: TBGRACustomBitmap; factorX,factorY: integer; dest: TBGRACustomBitmap; OffsetX,OffsetY: Integer; ADrawMode: TDrawMode);
function DownSample(source: TBGRACustomBitmap; factorX,factorY: integer): TBGRACustomBitmap;

{---------------------------- Interpolation filters --------------------------------}

function FineInterpolation(t: single; ResampleFilter: TResampleFilter): single;
function FineInterpolation256(t256: integer; ResampleFilter: TResampleFilter): integer;

type
  TWideKernelFilter = class
    function Interpolation(t: single): single; virtual; abstract;
    function ShouldCheckRange: boolean; virtual; abstract;
    function KernelWidth: single; virtual; abstract;
  end;

  TMitchellKernel = class(TWideKernelFilter)
    function Interpolation(t: single): single; override;
    function ShouldCheckRange: boolean; override;
    function KernelWidth: single; override;
  end;

  { TSplineKernel }

  TSplineKernel = class(TWideKernelFilter)
  public
    Coeff: single;
    constructor Create; overload;
    constructor Create(ACoeff: single); overload;
    function Interpolation(t: single): single; override;
    function ShouldCheckRange: boolean; override;
    function KernelWidth: single; override;
  end;

  { TCubicKernel }

  TCubicKernel = class(TWideKernelFilter)
    function pow3(x: single): single; inline;
    function Interpolation(t: single): single; override;
    function ShouldCheckRange: boolean; override;
    function KernelWidth: single; override;
  end;

  { TLanczosKernel }

  TLanczosKernel = class(TWideKernelFilter)
  private
    FNumberOfLobes: integer;
    FFactor: ValReal;
    procedure SetNumberOfLobes(AValue: integer);
  public
    constructor Create(ANumberOfLobes: integer);
    function Interpolation(t: single): single; override;
    function ShouldCheckRange: boolean; override;
    function KernelWidth: single; override;

    property NumberOfLobes : integer read FNumberOfLobes write SetNumberOfLobes;
  end;

function CreateInterpolator(style: TSplineStyle): TWideKernelFilter;

{-------------------------------- Fine resample ------------------------------------}

function FineResample(bmp: TBGRACustomBitmap;
  NewWidth, NewHeight: integer; ResampleFilter: TResampleFilter): TBGRACustomBitmap;

function WideKernelResample(bmp: TBGRACustomBitmap;
  NewWidth, NewHeight: integer; ResampleFilterSmaller, ResampleFilterLarger: TWideKernelFilter): TBGRACustomBitmap;

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAWriteAvif;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPimage, avifbgra;

type
  { TBGRAWriterAvif }

  TBGRAWriterAvif = class(TFPCustomImageWriter)
  protected
    FLossless: boolean;
    FQualityPercent: Single;
    FSpeed: integer;
    FPixelFormat:avifPixelFormat;
    FIgnoreAlpha:boolean;
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
    property QualityPercent: single read FQualityPercent write FQualityPercent;
    { If Lossless is set to True, the QualityPercent property is ignored }
    property Lossless: boolean read FLossless write FLossless;
    property Speed: integer read FSpeed write FSpeed;
    property PixelFormat: avifPixelFormat read FPixelFormat write FPixelFormat;
    property IgnoreAlpha: boolean read FIgnoreAlpha write FIgnoreAlpha;
  end;

Implementation
End.

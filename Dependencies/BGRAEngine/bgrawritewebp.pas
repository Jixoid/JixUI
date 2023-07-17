// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAWriteWebP;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPimage;

type
  { TBGRAWriterWebP }

  TBGRAWriterWebP = class(TFPCustomImageWriter)
  protected
    FLossless: boolean;
    FQualityPercent: Single;
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
    property QualityPercent: single read FQualityPercent write FQualityPercent;
    { If Lossless is set to True, the QualityPercent property is ignored }
    property Lossless: boolean read FLossless write FLossless;

  end;

Implementation
End.

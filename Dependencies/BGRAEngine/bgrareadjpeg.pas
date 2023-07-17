// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAReadJpeg;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPReadJPEG;

type
  TJPEGScale = FPReadJPEG.TJPEGScale;
  TJPEGReadPerformance = FPReadJPEG.TJPEGReadPerformance;

const
  jsFullSize = FPReadJPEG.jsFullSize;
  jsHalf = FPReadJPEG.jsHalf;
  jsQuarter = FPReadJPEG.jsQuarter;
  jsEighth = FPReadJPEG.jsEighth;

  jpBestQuality = FPReadJPEG.jpBestQuality;
  jpBestSpeed = FPReadJPEG.jpBestSpeed;

type
  { TBGRAReaderJpeg }

  TBGRAReaderJpeg = class(TFPReaderJPEG)
    constructor Create; override;
  protected
    function InternalCheck(Str: TStream): boolean; override;
  end;

Implementation
End.

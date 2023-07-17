// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAReadAvif;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPImage;

type

  { TBGRAReaderAvif }

  TBGRAReaderAvif = class(TFPCustomImageReader)
  protected
    procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Str: TStream): boolean; override;
  end;

Implementation
End.

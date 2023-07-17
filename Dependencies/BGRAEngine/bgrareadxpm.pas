// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAReadXPM;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPReadXPM, FPimage;

type

  { TBGRAReaderXPM }

  TBGRAReaderXPM = class(TFPReaderXPM)
    protected
      procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
      function InternalCheck(Str: TStream): boolean; override;
    public
      class procedure ConvertToXPM3(ASource: TStream; ADestination: TStream); static;
  end;

Implementation
End.

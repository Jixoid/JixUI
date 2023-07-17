// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAReadWebP;

{$mode objfpc}{$H+}
{$WARN 5060 off : Function result variable does not seem to be initialized}
interface

uses
  BGRAClasses, SysUtils, FPImage;

type
  TWebPHeader = record
    RIFFCode: array[1..4] of char;
    FileSize: LongWord;
    WebPCode: array[1..4] of char;
  end;

  { TBGRAReaderWebP }

  TBGRAReaderWebP = class(TFPCustomImageReader)
  protected
    function ReadHeader(Str: TStream): TWebPHeader;
    procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Str: TStream): boolean; override;
  end;

Implementation
End.

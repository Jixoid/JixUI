// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAReadIco;

{$mode objfpc}{$H+}
{$i bgrabitmap.map.inc}

interface

uses
  BGRAClasses, SysUtils, FPimage{$IFDEF BGRABITMAP_USE_LCL}, Graphics{$ENDIF};

type
  {$IFDEF BGRABITMAP_USE_LCL}TCustomIconClass = class of TCustomIcon;{$ENDIF}
  TByteSet = set of byte;

  { TBGRAReaderIcoOrCur }

  TBGRAReaderIcoOrCur = class(TFPCustomImageReader)
  protected
    procedure InternalRead({%H-}Str: TStream; {%H-}Img: TFPCustomImage); override;
    function InternalCheck(Str: TStream): boolean; override;
    function ExpectedMagic: TByteSet; virtual; abstract;
    {$IFDEF BGRABITMAP_USE_LCL}function LazClass: TCustomIconClass; virtual; abstract;{$ENDIF}
  public
    WantedWidth, WantedHeight : integer;
  end;

  TBGRAReaderIco = class(TBGRAReaderIcoOrCur)
  protected
    function ExpectedMagic: TByteSet; override;
    {$IFDEF BGRABITMAP_USE_LCL}function LazClass: TCustomIconClass; override;{$ENDIF}
  end;

  { TBGRAReaderCur }

  TBGRAReaderCur = class(TBGRAReaderIcoOrCur)
  protected
    function ExpectedMagic: TByteSet; override;
    {$IFDEF BGRABITMAP_USE_LCL}function LazClass: TCustomIconClass; override;{$ENDIF}
  end;

Implementation
End.

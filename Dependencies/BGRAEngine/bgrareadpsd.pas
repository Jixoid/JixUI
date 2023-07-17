// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
    The original file was part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team

    Psd reader for fpImage modified by circular.

 **********************************************************************

    03/2014 changes by circular :
    - added MinifyHeight,WantedHeight and OutputHeight (useful for thumbnails)
}
unit BGRAReadPSD;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPimage, FPReadPSD;

type
  { TBGRAReaderPSD }

  TBGRAReaderPSD = class(TFPReaderPSD)
  private
    FCompressed: boolean;
  protected
    FScanLines      : array of PByte;
    FInputLine     : array of record
        StreamOffset: Int64;
        Size: PtrInt;
      end;
    FOutputHeight: integer;
    function ReadPalette(Stream: TStream): boolean;
    procedure AnalyzeHeader;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function ReadScanLine(Stream: TStream; AInputSize: PtrInt; AChannel: integer): boolean; overload;
    procedure WriteScanLine(Img: TFPCustomImage; Row: integer); overload;
    function  InternalCheck(Stream: TStream) : boolean; override;
  public
    MinifyHeight,WantedHeight: integer;
    constructor Create; override;
    property Compressed: Boolean read FCompressed;
    property OutputHeight: integer read FOutputHeight;
  end;

Implementation
End.

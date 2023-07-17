// SPDX-License-Identifier: LGPL-3.0-linking-exception
{*****************************************************************************}
{
    The original file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2003 by Mazen NEIFER of the Free Pascal development team

    Targa reader implementation modified by circular.
}
{*****************************************************************************}

{ - 22/11/2007 Modified by Laurent Jacques for support all format }

{$mode objfpc}
{$h+}

unit BGRAReadTGA;

interface

uses FPReadTGA, FPimage, BGRAClasses;

type
  { TBGRAReaderTarga }

  TBGRAReaderTarga = class (TFPReaderTarga)
  protected
    FBuffer: packed array of byte;
    FBufferPos, FBufferSize: integer;
    FBufferStream: TStream;
    procedure ReadScanLine({%H-}Row: Integer; Stream: TStream); override;
    procedure WriteScanLine(Row : Integer; Img : TFPCustomImage); override;
    procedure InitReadBuffer(AStream: TStream; ASize: integer);
    procedure CloseReadBuffer;
    function GetNextBufferByte: byte;
  end;

Implementation
End.

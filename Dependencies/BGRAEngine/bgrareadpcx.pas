// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ This unit provides some optimisations of TFPReaderPCX: decompression using a read buffer.
  It also fixes the progress message and the InternalCheck. }

unit BGRAReadPCX;

{$mode objfpc}{$H+}

interface

uses FPImage, BGRAClasses, SysUtils, FPReadPCX;

type

  { TBGRAReaderPCX }

  TBGRAReaderPCX = class(TFPReaderPCX)
  protected
    FBuffer: packed array of byte;
    FBufferPos, FBufferSize: integer;
    FBufferStream: TStream;
    function InternalCheck(Stream: TStream): boolean; override;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    procedure ReadScanLine({%H-}Row: integer; Stream: TStream); override;
    procedure WriteScanLine(Row: integer; Img: TFPCustomImage); override;
    procedure InitReadBuffer(AStream: TStream; ASize: integer);
    procedure CloseReadBuffer;
    function GetNextBufferByte: byte;
  end;

Implementation
End.

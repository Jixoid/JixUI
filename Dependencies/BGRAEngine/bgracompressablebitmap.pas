// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRACompressableBitmap;

{$mode objfpc}{$H+}
{$WARN 6018 off : unreachable code}
interface

{ This unit contains the TBGRACompressableBitmap class, which
  can be used to temporarily compress bitmaps in memory.
  To use it, create an instance with the bitmap you want
  to compress. You can then free the original bitmap because
  TBGRACompressableBitmap contains all information necessary
  to build it again. To construct again your bitmap, call
  the GetBitmap function.

  When you have your bitmap in TBGRACompressableBitmap,
  you can call Compress function as many times as necessary
  until all data is compressed. It does only a part of the
  work at each call, so you can put it in a loop or in
  a timer. When it's done, Compress returns false to
  notify that it did nothing, which means you can
  stop calling Compress.

  In this implementation, the memory usage grows during
  the compression process and is lower only after it is
  finished. So it is recommended to compress one bitmap
  at a time. }

uses
  BGRAClasses, SysUtils, BGRABitmapTypes, BGRABitmap, zstream;

type

   { TBGRACompressableBitmap }

   TBGRACompressableBitmap = class
   private
     FWidth,FHeight: integer;
     FCaption: String;
     FBounds: TRect;
     FCompressedDataArray: array of TMemoryStream;
     FUncompressedData: TMemoryStream;
     FLineOrder: TRawImageLineOrder;
     FCompressionProgress: Int64;
     procedure Decompress;
     procedure FreeData;
     procedure Init;
   public
     CompressionLevel: Tcompressionlevel;
     constructor Create; overload;
     constructor Create(Source: TBGRABitmap); overload;
     function GetBitmap: TBGRABitmap;

     //call Compress as many times as necessary
     //when it returns false, it means that
     //the image compression is finished
     function Compress: boolean;
     procedure WriteToStream(AStream: TStream);
     procedure ReadFromStream(AStream: TStream);
     
     function UsedMemory: Int64;
     procedure Assign(Source: TBGRABitmap);
     destructor Destroy; override;
     property Width : Integer read FWidth;
     property Height: Integer read FHeight;
     property Caption : string read FCaption write FCaption;

   end;

Implementation
End.

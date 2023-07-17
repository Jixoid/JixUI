// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAGifFormat;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRAGraphics, BGRABitmap, BGRABitmapTypes,
  BGRAPalette;

type
  //what to do when finishing a frame and starting the next one
  TDisposeMode = (dmNone,        //undefined value
                  dmKeep,        //keep the changes done by the frame
                  dmErase,       //clear everything after the frame
                  dmRestore);    //restore to how it was before the frame

  //one image in the array
  TGifSubImage = record
    Image:    TBGRABitmap;       //image to draw at the beggining of the frame
    Position: TPoint;            //relative position of the image in the frame
    DelayMs:    integer;         //time in milliseconds to wait before going to next frame
    DisposeMode: TDisposeMode;   //what do do when going to next frame
    HasLocalPalette: boolean;    //the image has its own palette
  end;
  TGifSubImageArray = array of TGifSubImage;

  TGIFSignature = packed array[1..6] of char; //'GIF87a' or 'GIF89a'

  TGIFScreenDescriptor = packed record
    Width, Height: word;
    flags,                    //screen bit depth  = ((flags shr 4) and 7) + 1
                              //palette bit depth = (flags and 7) + 1
    BackgroundColorIndex,     //index of background color in global palette
    AspectRatio64 : byte;     //0 if not specified, otherwise aspect ratio is (AspectRatio64 + 15) / 64
  end;

  TGIFImageDescriptor = packed record
    x, y, Width, Height: word;
    flags: byte;
  end;

  TGIFImageDescriptorWithHeader = packed record
    ImageIntroducer: byte;
    Image: TGIFImageDescriptor;
  end;

  TGIFExtensionBlock = packed record
    FunctionCode: byte;
  end;

  TGIFGraphicControlExtension = packed record
    flags: byte;
    DelayHundredthSec: word;
    TransparentColorIndex: byte;
  end;

  TGIFGraphicControlExtensionWithHeader = packed record
    ExtensionIntroducer: byte;
    FunctionCode: byte;
    BlockSize: byte;
    GraphicControl: TGIFGraphicControlExtension;
    BlockTerminator: byte;
  end;

  TPackedRGBTriple = packed record
    r, g, b: byte;
  end;

  TGIFData = record
    Width, Height: integer;
    AspectRatio: single;
    BackgroundColor: TColor;
    LoopCount: Word;
    Images: array of TGifSubImage;
  end;

  { EColorQuantizerMissing }

  EColorQuantizerMissing = class(Exception)
    constructor Create;
    constructor Create(AMessage: string);
  end;

const
  GIFScreenDescriptor_GlobalColorTableFlag = $80;    //global palette is present
  GIFScreenDescriptor_GlobalColorSortFlag  = $08;    //global palette colors are sorted by importance

  GIFImageIntroducer     = $2c;
  GIFExtensionIntroducer = $21;
  GIFBlockTerminator     = $00;
  GIFFileTerminator      = $3B;

  GIFGraphicControlExtension_TransparentFlag = $01;  //transparent color index is provided
  GIFGraphicControlExtension_UserInputFlag = $02;    //wait for user input at this frame (ignored)
  GIFGraphicControlExtension_FunctionCode = $f9;
  GIFGraphicControlExtension_DisposeModeShift = 2;

  GIFImageDescriptor_LocalColorTableFlag = $80;      //local palette is present
  GIFImageDescriptor_InterlacedFlag = $40;           //image data is interlaced
  GIFImageDescriptor_LocalColorSortFlag = $20;       //local palette colors are sorted by importance

  GIFInterlacedStart: array[1..4] of longint = (0, 4, 2, 1);
  GIFInterlacedStep: array[1..4] of longint = (8, 8, 4, 2);

  GIFCodeTableSize = 4096;

  NetscapeApplicationIdentifier = 'NETSCAPE2.0';
  NetscapeSubBlockIdLoopCount = 1;
  NetscapeSubBlockIdBuffering = 2;

function CeilLn2(AValue: Integer): integer;
function BGRAToPackedRgbTriple(color: TBGRAPixel): TPackedRGBTriple;
function PackedRgbTribleToBGRA(rgb: TPackedRGBTriple): TBGRAPixel;
function GIFLoadFromStream(stream: TStream; MaxImageCount: integer = maxLongint): TGIFData;
procedure GIFSaveToStream(AData: TGifData; Stream: TStream; AQuantizerFactory: TBGRAColorQuantizerAny;
          ADitheringAlgorithm: TDitheringAlgorithm);
procedure GIFDecodeLZW(AStream: TStream; AImage: TBGRACustomBitmap;
          const APalette: ArrayOfTBGRAPixel; transcolorIndex: integer;
          interlaced: boolean);

//Encode an image supplied as an sequence of bytes, from left to right and top to bottom.
//Adapted from the work of Udo Schmal, http://www.gocher.me/FPWriteGIF
procedure GIFEncodeLZW(AStream: TStream; AImageData: PByte;
          AImageWidth, AImageHeight: integer; ABitDepth: byte);

Implementation
End.

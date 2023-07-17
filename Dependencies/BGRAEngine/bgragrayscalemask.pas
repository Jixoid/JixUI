// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAGrayscaleMask;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
  BGRAClasses, BGRAGraphics, SysUtils, BGRABitmapTypes, BGRAResample, {%H-}UniversalDrawer;

type
  { TGrayscaleMask }

  TGrayscaleMask = class(specialize TGenericUniversalBitmap<TByteMask,TByteMaskColorspace>)
  private
     function GetScanLine(Y: Integer): PByte; inline;
  protected
     function InternalNew: TCustomUniversalBitmap; override;
     procedure AssignTransparentPixel(out ADest); override;
     function InternalGetPixelCycle256(ix,iy: int32or64; iFactX,iFactY: int32or64): TByteMask;
     function InternalGetPixel256(ix,iy: int32or64; iFactX,iFactY: int32or64; smoothBorder: boolean): TByteMask;
     procedure Init; override;
  public
     ScanInterpolationFilter: TResampleFilter;

     constructor Create(AWidth,AHeight: Integer; AValue: byte); overload;
     constructor Create(ABitmap: TBGRACustomBitmap; AChannel: TChannel); overload;
     constructor CreateDownSample(ABitmap: TBGRACustomBitmap; AWidth,AHeight: integer);
     constructor CreateDownSample(ABitmap: TGrayscaleMask; AWidth,AHeight: integer);
     constructor CreateDownSample(ABitmap: TBGRACustomBitmap; AWidth,AHeight: integer; ASourceRect: TRect);
     constructor CreateDownSample(ABitmap: TGrayscaleMask; AWidth,AHeight: integer; ASourceRect: TRect);
     procedure CopyFrom(ABitmap: TGrayscaleMask); overload;
     procedure CopyFrom(ABitmap: TBGRACustomBitmap; AChannel: TChannel); overload;
     procedure CopyPropertiesTo(ABitmap: TCustomUniversalBitmap); override;
     function GetImageBounds: TRect; overload; override;
     function GetImageBoundsWithin(const ARect: TRect; Channel: TChannel = cAlpha; ANothingValue: Byte = 0): TRect; overload; override;
     function GetImageBoundsWithin(const ARect: TRect; Channels: TChannels; ANothingValue: Byte = 0): TRect; overload; override;

     class procedure SolidBrush(out ABrush: TUniversalBrush; const AColor: TByteMask; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
     class procedure ScannerBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner; ADrawMode: TDrawMode = dmDrawWithTransparency;
                                  AOffsetX: integer = 0; AOffsetY: integer = 0); override;
     class procedure MaskBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner;
                               AOffsetX: integer = 0; AOffsetY: integer = 0); override;
     class procedure EraseBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;
     class procedure AlphaBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;

     procedure Draw(ABitmap: TBGRACustomBitmap; X,Y: Integer; AGammaCorrection: boolean = false);
     procedure DrawAsAlpha(ABitmap: TBGRACustomBitmap; X,Y: Integer; const c: TBGRAPixel); overload;
     procedure DrawAsAlpha(ABitmap: TBGRACustomBitmap; X,Y: Integer; texture: IBGRAScanner); overload;
     function GetPixel(X,Y: integer): byte; overload;
     procedure SetPixel(X,Y: integer; AValue: byte);
     property ScanLine[Y: Integer]: PByte read GetScanLine;
     property Data: PByte read FDataByte;

     function GetPixel(x, y: single; AResampleFilter: TResampleFilter = rfLinear; smoothBorder: boolean = true): TByteMask; overload;
     function GetPixel256(x, y, fracX256,fracY256: int32or64; AResampleFilter: TResampleFilter = rfLinear; smoothBorder: boolean = true): TByteMask;

     procedure ScanNextMaskChunk(var ACount: integer; out AMask: PByteMask; out AStride: integer); override;
     function ScanAtIntegerMask(X,Y: integer): TByteMask; override;
     function ScanAtMask(X,Y: Single): TByteMask; override;
     function ScanAtInteger(X, Y: integer): TBGRAPixel; override;
     function ScanAt(X, Y: Single): TBGRAPixel; override;

     {inplace filters}
     procedure Negative;
     procedure NegativeRect(ABounds: TRect);
     procedure InplaceNormalize; overload;
     procedure InplaceNormalize(ABounds: TRect); overload;

     //return type helpers
     function NewBitmap: TGrayscaleMask; overload; override;
     function NewBitmap(AWidth, AHeight: integer): TGrayscaleMask; overload; override;
     function NewBitmap(AWidth, AHeight: integer; const Color: TByteMask): TGrayscaleMask; overload; override;
     function NewBitmap(AWidth, AHeight: integer; AColor: Pointer): TGrayscaleMask; overload; override;
     function NewReference: TGrayscaleMask; override;
     function GetUnique: TGrayscaleMask; override;
     function Duplicate(DuplicateProperties: Boolean = False): TGrayscaleMask; overload; override;
     function GetPart(const ARect: TRect): TGrayscaleMask; override;
     function CreateBrushTexture(ABrushStyle: TBrushStyle; APatternColor, ABackgroundColor: TByteMask;
                 AWidth: integer = 8; AHeight: integer = 8; APenWidth: single = 1): TGrayscaleMask; override;
     function RotateCW: TGrayscaleMask; override;
     function RotateCCW: TGrayscaleMask; override;
     function RotateUD: TGrayscaleMask; override;
     function FilterContour(ABorderValue: byte = 0): TGrayscaleMask;
     function FilterBlurRadial(radius: single; blurType: TRadialBlurType): TGrayscaleMask; overload; override;
     function FilterBlurRadial(const ABounds: TRect; radius: single; blurType: TRadialBlurType): TGrayscaleMask; overload; override;
     function FilterBlurRadial(radiusX, radiusY: single; blurType: TRadialBlurType): TGrayscaleMask; overload; override;
     function FilterBlurRadial(const ABounds: TRect; radiusX, radiusY: single; blurType: TRadialBlurType): TGrayscaleMask; overload; override;
     function FilterBlurMotion(distance: single; angle: single; oriented: boolean): TGrayscaleMask; overload; override;
     function FilterBlurMotion(const ABounds: TRect; distance: single; angle: single; oriented: boolean): TGrayscaleMask; overload; override;
     function FilterCustomBlur(mask: TCustomUniversalBitmap): TGrayscaleMask; overload; override;
     function FilterCustomBlur(const ABounds: TRect; mask: TCustomUniversalBitmap): TGrayscaleMask; overload; override;
     function FilterSphere: TGrayscaleMask;
     function FilterCylinder: TGrayscaleMask;
  end;

procedure DownSamplePutImageGrayscale(sourceData: PByte; sourcePixelSize: Int32or64; sourceRowDelta: Int32or64; sourceWidth, sourceHeight: Int32or64; dest: TGrayscaleMask; ADestRect: TRect); overload;
procedure DownSamplePutImageGrayscale(source: TBGRACustomBitmap; dest: TGrayscaleMask; ADestRect: TRect); overload;
procedure DownSamplePutImageGrayscale(source: TGrayscaleMask; dest: TGrayscaleMask; ADestRect: TRect); overload;
procedure DownSamplePutImageGrayscale(source: TBGRACustomBitmap; dest: TGrayscaleMask; ADestRect: TRect; ASourceRect: TRect); overload;
procedure DownSamplePutImageGrayscale(source: TGrayscaleMask; dest: TGrayscaleMask; ADestRect: TRect; ASourceRect: TRect); overload;

procedure BGRAFillClearTypeGrayscaleMask(dest: TBGRACustomBitmap; x,
  y: integer; xThird: integer; mask: TGrayscaleMask; color: TBGRAPixel;
  texture: IBGRAScanner; RGBOrder: boolean);

const
  ByteMaskBlack : TByteMask = (gray:0);
  ByteMaskWhite : TByteMask = (gray:255);

operator = (const c1, c2: TByteMask): boolean; inline;

Implementation
End.

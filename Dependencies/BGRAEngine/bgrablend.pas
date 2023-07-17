// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRABlend;

{ This unit contains pixel blending functions. They take a destination adress as parameter,
  and draw pixels at this address with different blending modes. These functions are used
  by many functions in BGRABitmap library to do the low level drawing. }

{$mode objfpc}{$H+}

interface

uses
  BGRABitmapTypes;

{ Brush providers }

procedure BGRASolidBrushIndirect(out ABrush: TUniversalBrush; AColor: Pointer; ADrawMode: TDrawMode = dmDrawWithTransparency);
procedure BGRAScannerBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner; ADrawMode: TDrawMode = dmDrawWithTransparency;
                           AOffsetX: integer = 0; AOffsetY: integer = 0);
procedure BGRAMaskBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner; AOffsetX: integer = 0; AOffsetY: integer = 0);
procedure BGRAEraseBrush(out ABrush: TUniversalBrush; AAlpha: Word);
procedure BGRAAlphaBrush(out ABrush: TUniversalBrush; AAlpha: Word);

{ Draw one pixel with alpha blending }
procedure DrawPixelInlineWithAlphaCheck(dest: PBGRAPixel; const c: TBGRAPixel); inline; overload;
procedure DrawPixelInlineWithAlphaCheck(dest: PBGRAPixel; c: TBGRAPixel; appliedOpacity: byte); inline; overload;
procedure DrawExpandedPixelInlineWithAlphaCheck(dest: PBGRAPixel; const ec: TExpandedPixel); inline; overload;
procedure DrawPixelInlineExpandedOrNotWithAlphaCheck(dest: PBGRAPixel; const ec: TExpandedPixel; c: TBGRAPixel); inline; overload;  //alpha in 'c' parameter
procedure DrawPixelInlineNoAlphaCheck(dest: PBGRAPixel; const c: TBGRAPixel); inline; overload;
procedure DrawExpandedPixelInlineNoAlphaCheck(dest: PBGRAPixel; const ec: TExpandedPixel; calpha: byte); inline; overload;
procedure ClearTypeDrawPixel(pdest: PBGRAPixel; Cr, Cg, Cb: byte; Color: TBGRAPixel); inline;
procedure InterpolateBilinear(pUpLeft,pUpRight,pDownLeft,pDownRight: PBGRAPixel;
                iFactX,iFactY: Integer; ADest: PBGRAPixel);
procedure InterpolateBilinearMask(pUpLeft,pUpRight,pDownLeft,pDownRight: PByteMask;
                iFactX,iFactY: Integer; ADest: PByteMask);

procedure CopyPixelsWithOpacity(dest,src: PBGRAPixel; opacity: byte; Count: integer); inline;
function ApplyOpacity(opacity1,opacity2: byte): byte; inline;
function FastRoundDiv255(value: LongWord): LongWord; inline;

{ Draw a series of pixels with alpha blending }
procedure PutPixels(pdest: PBGRAPixel; psource: PBGRAPixel; copycount: integer; mode: TDrawMode; AOpacity:byte);
procedure DrawPixelsInline(dest: PBGRAPixel; c: TBGRAPixel; Count: integer); inline; overload;
procedure DrawExpandedPixelsInline(dest: PBGRAPixel; ec: TExpandedPixel; Count: integer); inline; overload;
procedure DrawPixelsInlineExpandedOrNot(dest: PBGRAPixel; ec: TExpandedPixel; c: TBGRAPixel; Count: integer); inline; overload;  //alpha in 'c' parameter

{ Draw one pixel with linear alpha blending }
procedure FastBlendPixelInline(dest: PBGRAPixel; const c: TBGRAPixel); inline; overload;
procedure FastBlendPixelInline(dest: PBGRAPixel; c: TBGRAPixel; appliedOpacity: byte); inline; overload;

{ Draw a series of pixels with linear alpha blending }
procedure FastBlendPixelsInline(dest: PBGRAPixel; c: TBGRAPixel; Count: integer); inline;

{ Replace a series of pixels }
procedure FillInline(dest: PBGRAPixel; c: TBGRAPixel; Count: integer); inline;

{ Xor a series of pixels }
procedure XorInline(dest: PBGRAPixel; c: TBGRAPixel; Count: integer); inline;
procedure XorPixels(pdest, psrc: PBGRAPixel; count: integer);

{ Set alpha value for a series of pixels }
procedure AlphaFillInline(dest: PBGRAPixel; alpha: byte; Count: integer); inline;

{ Erase a series of pixels, i.e. decrease alpha value }
procedure ErasePixelInline(dest: PBGRAPixel; alpha: byte); inline;

{ Draw a pixel to the extent the current pixel is close enough to compare value.
  It should not be called on pixels that have not been checked to be close enough }
procedure DrawPixelInlineDiff(dest: PBGRAPixel; c, compare: TBGRAPixel;
  maxDiff: byte); inline;
{ Draw a series of pixel to the extent the current pixel is close enough to compare value }
procedure DrawPixelsInlineDiff(dest: PBGRAPixel; c: TBGRAPixel;
  Count: integer; compare: TBGRAPixel; maxDiff: byte); inline;

{ Blend pixels with scanner content }
procedure ScannerPutPixels(scan: IBGRAScanner; pdest: PBGRAPixel; count: integer; mode: TDrawMode);

{ Perform advanced blending operation }
procedure BlendPixels(pdest: PBGRAPixel; psrc: PBGRAPixel;
  blendOp: TBlendOperation; Count: integer; excludeChannels: TChannels = []);

{ Perform blending operation and merge over destination }
procedure BlendPixelsOver(pdest: PBGRAPixel; psrc: PBGRAPixel;
  blendOp: TBlendOperation; Count: integer; opacity: byte; linearBlend: boolean = false;
  excludeChannels: TChannels = []);

//layer blend modes
//- http://www.pegtop.net/delphi/articles/blendmodes/
//- http://www.w3.org/TR/2009/WD-SVGCompositing-20090430/#comp-op
//- http://docs.gimp.org/en/gimp-concepts-layer-modes.html
procedure LinearMultiplyPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure AddPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearAddPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure ColorBurnPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure ColorDodgePixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure DividePixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure ReflectPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure NonLinearReflectPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure GlowPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure NiceGlowPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure OverlayPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearOverlayPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure DifferencePixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearDifferencePixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure ExclusionPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearExclusionPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearSubtractPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearSubtractInversePixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure SubtractPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure SubtractInversePixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure NegationPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearNegationPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LightenPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure DarkenPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure ScreenPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure SoftLightPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure SvgSoftLightPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure HardLightPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure BlendXorPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure BlendMaskPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearMultiplySaturationInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearHueInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearColorInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearLightnessInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearSaturationInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure CorrectedHueInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure CorrectedColorInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure CorrectedLightnessInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure CorrectedSaturationInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure BGRAFillClearTypeMask(dest: TBGRACustomBitmap; x,y: integer; xThird: integer; mask: TBGRACustomBitmap; color: TBGRAPixel; texture: IBGRAScanner; RGBOrder: boolean);
procedure BGRAFillClearTypeRGBMask(dest: TBGRACustomBitmap; x, y: integer;
  mask: TBGRACustomBitmap; color: TBGRAPixel; texture: IBGRAScanner;
  KeepRGBOrder: boolean);
procedure BGRAFillClearTypeMaskPtr(dest: TBGRACustomBitmap; x,y: integer; xThird: integer; maskData: PByte; maskPixelSize: Int32or64; maskRowSize: Int32or64; maskWidth,maskHeight: integer; color: TBGRAPixel; texture: IBGRAScanner; RGBOrder: boolean);

Implementation
End.

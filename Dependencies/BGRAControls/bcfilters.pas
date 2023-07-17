{
  Created by BGRA Controls Team
  Dibo, Circular, lainz (007) and contributors.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit bcfilters;

{
// all pixels //
var
  i: integer;
  p: PBGRAPixel;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels-1 downto 0 do
  begin
    p^.red := ;
    p^.green := ;
    p^.blue := ;
    p^.alpha := ;
    Inc(p);
  end;

// scan line //
var
  x, y: integer;
  p: PBGRAPixel;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      p^.red := ;
      p^.green := ;
      p^.blue := ;
      p^.alpha := ;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
}

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LCLProc, LazUTF8,{$ELSE}Types, BGRAGraphics, GraphType, FPImage, {$ENDIF} Math, BGRABitmap, BGRABitmapTypes;

type
  TBCSimpleFilter = (bcsNone, bcsGameBoyDithering, bcsBlackAndWhiteDithering, bcsInvert,
    bcsGrayScale, bcsGrayScaleA,
    bcsGrayScaleBGRA, bcsGameBoy, bcsNoise,
    bcsNoiseA, bcsNoiseBW, bcsNoiseBWA, bcsTVScanLinesH, bcsTVScanLinesV,
    bcsCheckeredL, bcsCheckeredR, bcsBlackAndWhite, bcsInstagram1,
    bcsInstagram2, bcsInstagram3, bcsInstagram4, bcsInstagram5, bcsInstagram6,
    bcsPhotoNoise, bcsPolaroid, bcsMovement, bcsRBG, bcsGRB, bcsGBR,
    bcsBRG, bcsBGR, bcsRRG, bcsRGR, bcsGRR, bcsRRB, bcsRBR, bcsBRR,
    bcsGGR, bcsGRG, bcsRGG, bcsGGB, bcsGBG, bcsBGG, bcsBBR, bcsBRB,
    bcsRBB, bcsBBG, bcsBGB, bcsGBB, bcsRRR, bcsGGG, bcsBBB);

const
  BCSimpleFilterStr: array [TBCSimpleFilter] of string =
    ('None', 'GameBoyDithering', 'BlackAndWhiteDithering', 'Invert', 'GrayScale',
    'GrayScaleA', 'GrayScaleBGRA', 'GameBoy',
    'Noise', 'NoiseA', 'NoiseBW', 'NoiseBWA', 'TVScanLinesH', 'TVScanLinesV',
    'CheckeredL', 'CheckeredR', 'BlackAndWhite', 'Instagram1', 'Instagram2',
    'Instagram3', 'Instagram4', 'Instagram5', 'Instagram6', 'PhotoNoise',
    'Polaroid', 'Movement', 'RBG', 'GRB', 'GBR', 'BRG', 'BGR', 'RRG',
    'RGR', 'GRR', 'RRB', 'RBR', 'BRR', 'GGR', 'GRG', 'RGG', 'GGB', 'GBG',
    'BGG', 'BBR', 'BRB', 'RBB', 'BBG', 'BGB', 'GBB', 'RRR', 'GGG', 'BBB');

function StrToTBCSimpleFilter(const s: ansistring): TBCSimpleFilter;
procedure BCSimpleFilterStrList(s: TStrings);

procedure FilterRGB(Bitmap: TBGRABitmap; R, G, B: byte);

procedure RBG(Bitmap: TBGRABitmap);
procedure GRB(Bitmap: TBGRABitmap);
procedure GBR(Bitmap: TBGRABitmap);
procedure BRG(Bitmap: TBGRABitmap);
procedure BGR(Bitmap: TBGRABitmap);
procedure RRG(Bitmap: TBGRABitmap);
procedure RGR(Bitmap: TBGRABitmap);
procedure GRR(Bitmap: TBGRABitmap);
procedure RRB(Bitmap: TBGRABitmap);
procedure RBR(Bitmap: TBGRABitmap);
procedure BRR(Bitmap: TBGRABitmap);
procedure GGR(Bitmap: TBGRABitmap);
procedure GRG(Bitmap: TBGRABitmap);
procedure RGG(Bitmap: TBGRABitmap);
procedure GGB(Bitmap: TBGRABitmap);
procedure GBG(Bitmap: TBGRABitmap);
procedure BGG(Bitmap: TBGRABitmap);
procedure BBR(Bitmap: TBGRABitmap);
procedure BRB(Bitmap: TBGRABitmap);
procedure RBB(Bitmap: TBGRABitmap);
procedure BBG(Bitmap: TBGRABitmap);
procedure BGB(Bitmap: TBGRABitmap);
procedure GBB(Bitmap: TBGRABitmap);
procedure RRR(Bitmap: TBGRABitmap);
procedure GGG(Bitmap: TBGRABitmap);
procedure BBB(Bitmap: TBGRABitmap);

{ Invert colors, keep alpha }
procedure Invert(Bitmap: TBGRABitmap); overload;
{ Invert colors, advanced options }
procedure Invert(Bitmap: TBGRABitmap; touchR, touchG, touchB, touchA: boolean); overload;

{ GrayScale, keep alpha }
procedure GrayScale(Bitmap: TBGRABitmap); overload;
{ GrayScale, keep alpha, pallete }
procedure GrayScale(Bitmap: TBGRABitmap; pallete: byte); overload;
{ GrayScale, alpha 255}
procedure GrayScaleA(Bitmap: TBGRABitmap);
{ GrayScale, using BGRAToGrayScale }
procedure GrayScaleBGRA(Bitmap: TBGRABitmap);

{ like GameBoy}
procedure GameBoy(Bitmap: TBGRABitmap);

{ Dithering }
procedure GameBoyDithering(Bitmap: TBGRABitmap);
procedure BlackAndWhiteDithering(Bitmap: TBGRABitmap);

{ Noise random color, keep alpha }
procedure Noise(Bitmap: TBGRABitmap); overload;
{ Noise random color, advanced options }
procedure Noise(Bitmap: TBGRABitmap; touchR, touchG, touchB, touchA: boolean); overload;
{ Noise random color, random alpha }
procedure NoiseA(Bitmap: TBGRABitmap);

{ Noise random color, set max posible values }
procedure NoiseMax(Bitmap: TBGRABitmap; maxR, maxG, maxB, maxA: byte); overload;
{ Noise random color, set max posible values, advanced options }
procedure NoiseMax(Bitmap: TBGRABitmap; maxR, maxG, maxB, maxA: byte;
  touchR, touchG, touchB, touchA: boolean); overload;

{ Noise black and white, keep alpha }
procedure NoiseBW(Bitmap: TBGRABitmap);
{ Noise black and white, random alpha }
procedure NoiseBWA(Bitmap: TBGRABitmap);

{ TV Lines Horizontal }
procedure TVScanLinesH(Bitmap: TBGRABitmap);
{ TV Lines Vertical }
procedure TVScanLinesV(Bitmap: TBGRABitmap);
{ Checkered Left aligned }
procedure CheckeredL(Bitmap: TBGRABitmap);
{ Checkered Right aligned }
procedure CheckeredR(Bitmap: TBGRABitmap);

{ Black and White, middle 128 }
procedure BlackAndWhite(Bitmap: TBGRABitmap); overload;
{ Black and White, custom middle }
procedure BlackAndWhite(Bitmap: TBGRABitmap; middle: byte); overload;

{ Instagram Filters }
// sepia
procedure Instagram1(Bitmap: TBGRABitmap);
// blue-green
procedure Instagram2(Bitmap: TBGRABitmap);
// purple
procedure Instagram3(Bitmap: TBGRABitmap);
// blue 3 channels
procedure Instagram4(Bitmap: TBGRABitmap);
// green 3 channels
procedure Instagram5(Bitmap: TBGRABitmap);
// red 3 channels
procedure Instagram6(Bitmap: TBGRABitmap);
// white rounded border
procedure Polaroid(Bitmap: TBGRABitmap);
// blured bw noise
procedure PhotoNoise(Bitmap: TBGRABitmap);

{ Pixel movement }
procedure Movement(Bitmap: TBGRABitmap; randXmin: NativeInt = -5;
  randXmax: NativeInt = 5; randYmin: NativeInt = -5; randYmax: NativeInt = 5);

procedure Zoomy(Bitmap: TBGRABitmap; xMy, yMy: extended);

{ Filters that only need Bitmap as parameter }
procedure SimpleFilter(Bitmap: TBGRABitmap; Filter: TBCSimpleFilter);

Implementation
End.

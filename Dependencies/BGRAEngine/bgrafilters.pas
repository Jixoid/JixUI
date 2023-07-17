// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAFilters;

{$mode objfpc}{$H+}

interface

{ Here are some filters that can be applied to a bitmap. The filters
  take a source image as a parameter and gives a filtered image as
  a result. }

uses
  BGRAClasses, BGRABitmapTypes, BGRAFilterType, BGRAFilterBlur;

type
  TFilterTask = BGRAFilterType.TFilterTask;

/////////////////////// PIXELWISE FILTERS ////////////////////////////////
type
  { TGrayscaleTask }
  { Grayscale converts colored pixel into grayscale with same luminosity }
  TGrayscaleTask = class(TFilterTask)
  private
    FBounds: TRect;
  public
    constructor Create(bmp: TBGRACustomBitmap; ABounds: TRect);
  protected
    procedure DoExecute; override;
  end;

{ Grayscale converts colored pixel into grayscale with same luminosity }
function FilterGrayscale(bmp: TBGRACustomBitmap): TBGRACustomBitmap; overload;
function FilterGrayscale(bmp: TBGRACustomBitmap; ABounds: TRect): TBGRACustomBitmap; overload;
function CreateGrayscaleTask(bmp: TBGRACustomBitmap; ABounds: TRect): TFilterTask;

{ Normalize use the whole available range of values, making dark colors darkest possible
  and light colors lightest possible }
function FilterNormalize(bmp: TBGRACustomBitmap;
  eachChannel: boolean = True): TBGRACustomBitmap; overload;
function FilterNormalize(bmp: TBGRACustomBitmap; ABounds: TRect;
  eachChannel: boolean = True): TBGRACustomBitmap; overload;

////////////////////// 3X3 FILTERS ////////////////////////////////////////////

{ Sharpen filter add more contrast between pixels }
function FilterSharpen(bmp: TBGRACustomBitmap; AAmount: integer = 256): TBGRACustomBitmap; overload;
function FilterSharpen(bmp: TBGRACustomBitmap; ABounds: TRect; AAmount: integer = 256): TBGRACustomBitmap; overload;

{ Compute a contour, as if the image was drawn with a 2 pixels-wide black pencil }
function FilterContour(bmp: TBGRACustomBitmap; AGammaCorrection: boolean = false): TBGRACustomBitmap;

{ Emboss filter compute a color difference in the angle direction }
function FilterEmboss(bmp: TBGRACustomBitmap; angle: single; AStrength: integer= 64; AOptions: TEmbossOptions = []): TBGRACustomBitmap; overload;
function FilterEmboss(bmp: TBGRACustomBitmap; angle: single; ABounds: TRect; AStrength: integer= 64; AOptions: TEmbossOptions = []): TBGRACustomBitmap; overload;

{ Emboss highlight computes a sort of emboss with 45 degrees angle and
  with standard selection color (white/black and filled with blue) }
function FilterEmbossHighlight(bmp: TBGRACustomBitmap;
  FillSelection: boolean; DefineBorderColor: TBGRAPixel): TBGRACustomBitmap;
function FilterEmbossHighlightOffset(bmp: TBGRACustomBitmap;
  FillSelection: boolean; DefineBorderColor: TBGRAPixel; var Offset: TPoint): TBGRACustomBitmap;

{ The median filter consist in calculating the median value of pixels. Here
  a square of 9x9 pixel is considered. The median allow to select the most
  representative colors. The option parameter allow to choose to smooth the
  result or not. }
function FilterMedian(bmp: TBGRACustomBitmap; Option: TMedianOption): TBGRACustomBitmap;

//////////////////////// DEFORMATION FILTERS /////////////////////////////////

{ Distort the image as if it were on a sphere }
function FilterSphere(bmp: TBGRACustomBitmap): TBGRACustomBitmap;

{ Twirl distortion, i.e. a progressive rotation }
function FilterTwirl(bmp: TBGRACustomBitmap; ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRACustomBitmap; overload;
function FilterTwirl(bmp: TBGRACustomBitmap; ABounds: TRect; ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRACustomBitmap; overload;

{ Distort the image as if it were on a vertical cylinder }
function FilterCylinder(bmp: TBGRACustomBitmap): TBGRACustomBitmap;

{ Compute a plane projection towards infinity (SLOW) }
function FilterPlane(bmp: TBGRACustomBitmap): TBGRACustomBitmap;

{ Rotate filter rotate the image and clip it in the bounding rectangle }
function FilterRotate(bmp: TBGRACustomBitmap; origin: TPointF;
  angle: single; correctBlur: boolean = false): TBGRACustomBitmap;

///////////////////////// BLUR FILTERS //////////////////////////////////////

{ A radial blur applies a blur with a circular influence, i.e, each pixel
  is merged with pixels within the specified radius. There is an exception
  with rbFast blur, the optimization entails a hyperbolic shape. }
type TRadialBlurTask = BGRAFilterBlur.TRadialBlurTask;
function FilterBlurRadial(bmp: TBGRACustomBitmap; radius: single; blurType: TRadialBlurType): TBGRACustomBitmap; overload;
function FilterBlurRadial(bmp: TBGRACustomBitmap; radiusX: single; radiusY: single; blurType: TRadialBlurType): TBGRACustomBitmap; overload;
function CreateRadialBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect; ARadius: single; ABlurType: TRadialBlurType): TRadialBlurTask; overload;
function CreateRadialBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect; ARadiusX,ARadiusY: single; ABlurType: TRadialBlurType): TRadialBlurTask; overload;

{ The precise blur allow to specify the blur radius with subpixel accuracy }
function FilterBlurRadialPrecise(bmp: TBGRACustomBitmap; radius: single): TBGRACustomBitmap; deprecated 'Use FilterBlurRadial with blurType:=rbPrecise and radius multiplied by 10';
function CreateRadialPreciseBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect; ARadius: single): TRadialBlurTask; deprecated 'Use CreateRadialBlurTask with blurType:=rbPrecise and radius multiplied by 10';

{ Motion blur merge pixels in a direction. The oriented parameter specifies
  if the weights of the pixels are the same along the line or not. }
type TMotionBlurTask = BGRAFilterBlur.TMotionBlurTask;
function FilterBlurMotion(bmp: TBGRACustomBitmap; distance: single; angle: single; oriented: boolean): TBGRACustomBitmap;
function CreateMotionBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect; ADistance,AAngle: single; AOriented: boolean): TMotionBlurTask;

{ General purpose blur filter, with a blur mask as parameter to describe
  how pixels influence each other }
function FilterBlur(bmp: TBGRACustomBitmap; AMask: TCustomUniversalBitmap; AMaskIsThreadSafe: boolean = false): TBGRACustomBitmap;
function CreateBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect; AMask: TCustomUniversalBitmap; AMaskIsThreadSafe: boolean = false): TFilterTask;

////////////////////////////// OTHER FILTERS /////////////////////////////////

{ SmartZoom x3 is a filter that upsizes 3 times the picture and add
  pixels that could be logically expected (horizontal, vertical, diagonal lines) }
function FilterSmartZoom3(bmp: TBGRACustomBitmap;
  Option: TMedianOption): TBGRACustomBitmap;

function FilterPixelate(bmp: TBGRACustomBitmap; pixelSize: integer; useResample: boolean; filter: TResampleFilter = rfLinear): TBGRACustomBitmap;

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
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
unit BGRAPascalScript;
// Note: overloaded procedures not supported, use unique identifiers
{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes;

type
  TBGRAColor = longword;

var
  BitmapArray: array of TBGRABitmap;

{Internal use only}
procedure bgra_Initialization;
procedure bgra_Finalization;

procedure bgra_AddBitmap(id: integer);
function bgra_GetHighestID: integer;

function BGRAColorToBGRAPixel(AColor: TBGRAColor): TBGRAPixel;

function rgb(red, green, blue: byte): TBGRAColor;
function rgba(red, green, blue, alpha: byte): TBGRAColor;
function getBlue(AColor: TBGRAColor): byte;
function getGreen(AColor: TBGRAColor): byte;
function getRed(AColor: TBGRAColor): byte;
function getAlpha(AColor: TBGRAColor): byte;
function setBlue(AColor: TBGRAColor; AValue: byte): TBGRAColor;
function setGreen(AColor: TBGRAColor; AValue: byte): TBGRAColor;
function setRed(AColor: TBGRAColor; AValue: byte): TBGRAColor;
function setAlpha(AColor: TBGRAColor; AValue: byte): TBGRAColor;

{Constructors}
procedure bgra_Create(id: integer);
procedure bgra_CreateWithSize(id: integer; AWidth, AHeight: integer);
procedure bgra_CreateFromFile(id: integer; AFilename: string);
procedure bgra_Destroy(id: integer);
procedure bgra_DestroyAll;

procedure bgra_Fill(id: integer; AColor: TBGRAColor);
procedure bgra_SetPixel(id: integer; x, y: integer; AColor: TBGRAColor);
function bgra_GetPixel(id: integer; x, y: integer): TBGRAColor;

{Loading functions}
procedure bgra_SaveToFile(id: integer; const filename: string);

{Filters - direct apply}
procedure bgra_FilterSmartZoom3(id: integer; Option: TMedianOption);
procedure bgra_FilterMedian(id: integer; Option: TMedianOption);
procedure bgra_FilterSmooth(id: integer);
procedure bgra_FilterSharpen(id: integer; Amount: single);
procedure bgra_FilterSharpenRect(id: integer; ABounds: TRect; Amount: single);
procedure bgra_FilterContour(id: integer);
procedure bgra_FilterPixelate(id: integer; pixelSize: integer;
  useResample: boolean; filter: TResampleFilter);
procedure bgra_FilterBlurRadial(id: integer; radius: integer; blurType: TRadialBlurType);
procedure bgra_FilterBlurRadialRect(id: integer; ABounds: TRect;
  radius: integer; blurType: TRadialBlurType);
procedure bgra_FilterBlurMotion(id: integer; distance: integer;
  angle: single; oriented: boolean);
procedure bgra_FilterBlurMotionRect(id: integer; ABounds: TRect;
  distance: integer; angle: single; oriented: boolean);
procedure bgra_FilterCustomBlur(id: integer; mask: integer);
procedure bgra_FilterCustomBlurRect(id: integer; ABounds: TRect; mask: integer);
procedure bgra_FilterEmboss(id: integer; angle: single);
procedure bgra_FilterEmbossRect(id: integer; angle: single; ABounds: TRect);
procedure bgra_FilterEmbossHighlight(id: integer; FillSelection: boolean);
procedure bgra_FilterEmbossHighlightBorder(id: integer; FillSelection: boolean;
  BorderColor: TBGRAColor);
procedure bgra_FilterEmbossHighlightBorderAndOffset(id: integer;
  FillSelection: boolean; BorderColor: TBGRAColor; Offset: TPoint);
procedure bgra_FilterGrayscale(id: integer);
procedure bgra_FilterGrayscaleRect(id: integer; ABounds: TRect);
procedure bgra_FilterNormalize(id: integer; eachChannel: boolean);
procedure bgra_FilterNormalizeRect(id: integer; ABounds: TRect; eachChannel: boolean);
procedure bgra_FilterRotate(id: integer; origin: TPointF; angle: single;
  correctBlur: boolean);
procedure bgra_FilterSphere(id: integer);
procedure bgra_FilterTwirl(id: integer; ACenter: TPoint; ARadius: single;
  ATurn: single; AExponent: single);
procedure bgra_FilterTwirlRect(id: integer; ABounds: TRect; ACenter: TPoint;
  ARadius: single; ATurn: single; AExponent: single);
procedure bgra_FilterCylinder(id: integer);
procedure bgra_FilterPlane(id: integer);

Implementation
End.

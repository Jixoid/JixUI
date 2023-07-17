// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAGradients;

{$mode objfpc}{$H+}
{$i bgrabitmap.map.inc}
{$i bgrasse.map.inc}

interface

{ Here are various functions that draw gradients, shadow and lighting }

uses
  BGRAClasses, BGRAGraphics, BGRABitmapTypes, BGRABitmap, BGRABlend, BGRAPhongTypes,
  BGRASSE, BGRAGrayscaleMask;

{$IFDEF BGRABITMAP_USE_LCL}{ Creates a bitmap with the specified text horizontally centered and with a shadow }
function TextShadow(AWidth,AHeight: Integer; AText: String; AFontHeight: Integer; ATextColor,AShadowColor: TBGRAPixel;
  AOffSetX,AOffSetY: Integer; ARadius: Integer = 0; AFontStyle: TFontStyles = []; AFontName: String = 'Default'; AShowText: Boolean = True): TBGRABitmap;
{$ENDIF}

{----------------------------------------------------------------------}
{ Functions to draw multiple gradients.
  See : http://wiki.lazarus.freepascal.org/Double_Gradient#nGradient }
type
  TnGradientInfo = record
    StartColor,StopColor: TBGRAPixel;
    Direction: TGradientDirection;
    EndPercent : single; // Position from 0 to 1
  end;

function nGradientInfo(StartColor, StopColor: TBGRAPixel; Direction: TGradientDirection; EndPercent: Single): TnGradientInfo;

function nGradientAlphaFill(ARect: TRect; ADir: TGradientDirection; const AGradient: array of TnGradientInfo): TBGRABitmap; overload;
function nGradientAlphaFill(AWidth, AHeight: Integer; ADir: TGradientDirection; const AGradient: array of TnGradientInfo): TBGRABitmap; overload;
procedure nGradientAlphaFill(ACanvas: TCanvas; ARect: TRect; ADir: TGradientDirection; const AGradient: array of TnGradientInfo); overload;
procedure nGradientAlphaFill(ABitmap: TBGRABitmap; ARect: TRect; ADir: TGradientDirection; const AGradient: array of TnGradientInfo); overload;

function DoubleGradientAlphaFill(ARect: TRect; AStart1,AStop1,AStart2,AStop2: TBGRAPixel;
                                 ADirection1,ADirection2,ADir: TGradientDirection; AValue: Single): TBGRABitmap; overload;
function DoubleGradientAlphaFill(AWidth,AHeight: Integer; AStart1,AStop1,AStart2,AStop2: TBGRAPixel;
                                 ADirection1,ADirection2,ADir: TGradientDirection; AValue: Single): TBGRABitmap; overload;
procedure DoubleGradientAlphaFill(ACanvas: TCanvas; ARect: TRect; AStart1,AStop1,AStart2,AStop2: TBGRAPixel;
                                 ADirection1,ADirection2,ADir: TGradientDirection; AValue: Single); overload;
procedure DoubleGradientAlphaFill(ABitmap: TBGRABitmap; ARect: TRect; AStart1,AStop1,AStart2,AStop2: TBGRAPixel;
                                 ADirection1,ADirection2,ADir: TGradientDirection; AValue: Single); overload;

{----------------------------------------------------------------------}
{ Phong shading functions. Use a height map (grayscale image or a precise map filled with MapHeightToBGRA)
  to determine orientation and position of the surface.

  Phong shading consist in adding an ambiant light, a diffuse light (angle between light and object),
  and a specular light (angle between light, object and observer, i.e. reflected light) }

type
  TRectangleMapOption = (rmoNoLeftBorder,rmoNoTopBorder,rmoNoRightBorder,rmoNoBottomBorder,rmoLinearBorder);
  TRectangleMapOptions = set of TRectangleMapOption;

  { TPhongShading }

  TPhongShading = class(TCustomPhongShading)
  public
    LightSourceIntensity : Single; //global intensity of the light

    LightSourceDistanceTerm,       //minimum distance always added (positive value)
    LightSourceDistanceFactor,     //how much actual distance is taken into account (usually 0 or 1)
    LightDestFactor : Single;      //how much the location of the lightened pixel is taken into account (usually 0 or 1)

    LightColor: TBGRAPixel;        //color of the light reflection

    SpecularFactor,                //how much light is reflected (0..1)
    SpecularIndex : Single;        //how concentrated reflected light is (positive value)

    AmbientFactor,                 //ambiant lighting whereever the point is (0..1)
    DiffusionFactor,               //diffusion, i.e. how much pixels are lightened by light source (0..1)
    NegativeDiffusionFactor : Single; //how much hidden surface are darkened (0..1)
    DiffuseSaturation: Boolean;    //when diffusion saturates, use light color to show it

    constructor Create;

    { Render the specified map on the destination bitmap with one solid color. Map altitude
      indicate the global height of the map. }
    procedure Draw(dest: TBGRACustomBitmap; map: TBGRACustomBitmap; mapAltitude: single; ofsX,ofsY: integer;
                   Color : TBGRAPixel); override;

    { Render with a color map of the same size as the height map. Map altitude
      indicate the global height of the map. }
    procedure Draw(dest: TBGRACustomBitmap; map: TBGRACustomBitmap; mapAltitude: single; ofsX,ofsY: integer;
                   ColorMap : TBGRACustomBitmap); override;

    { Render with a color scanner. Map altitude
      indicate the global height of the map. }
    procedure DrawScan(dest: TBGRACustomBitmap; map: TBGRACustomBitmap; mapAltitude: single; ofsX,ofsY: integer;
                   ColorScan : IBGRAScanner); override;

    { Draw a cone of the specified color }
    procedure DrawCone(dest: TBGRACustomBitmap; X,Y,Size: Integer; Altitude: Single; Color: TBGRAPixel); overload;
    procedure DrawCone(dest: TBGRACustomBitmap; bounds: TRect; Altitude: Single; Color: TBGRAPixel); overload;

    { Draw a vertical cone of the specified color }
    procedure DrawVerticalCone(dest: TBGRACustomBitmap; bounds: TRect; Altitude: Single; Color: TBGRAPixel);

    { Draw an horizontal cylinder of the specified color }
    procedure DrawHorizontalCylinder(dest: TBGRACustomBitmap; bounds: TRect; Altitude: Single; Color: TBGRAPixel);

    { Draw a vertical cylinder of the specified color }
    procedure DrawVerticalCylinder(dest: TBGRACustomBitmap; bounds: TRect; Altitude: Single; Color: TBGRAPixel);

    { Draw a hemisphere of the specified color }
    procedure DrawSphere(dest: TBGRACustomBitmap; bounds: TRect; Altitude: Single; Color: TBGRAPixel);

    { Draw a rectangle of the specified color }
    procedure DrawRectangle(dest: TBGRACustomBitmap; bounds: TRect; Border: Integer; Altitude: Single; Color: TBGRAPixel; RoundCorners: Boolean; Options: TRectangleMapOptions);

  protected

    procedure DrawMapNormal(dest: TBGRACustomBitmap; map: TBGRACustomBitmap; mapAltitude: single; ofsX,ofsY: integer;
                   ColorMap : TBGRACustomBitmap);
    procedure DrawScannerNormal(dest: TBGRACustomBitmap; map: TBGRACustomBitmap; mapAltitude: single; ofsX,ofsY: integer;
                   ColorScan : IBGRAScanner);
    procedure DrawColorNormal(dest: TBGRACustomBitmap; map: TBGRACustomBitmap; mapAltitude: single; ofsX,ofsY: integer;
                   Color : TBGRAPixel);

    {$ifdef BGRASSE_AVAILABLE}
    procedure DrawMapSSE(dest: TBGRACustomBitmap; map: TBGRACustomBitmap; mapAltitude: single; ofsX,ofsY: integer;
                   ColorMap : TBGRACustomBitmap);
    procedure DrawScannerSSE(dest: TBGRACustomBitmap; map: TBGRACustomBitmap; mapAltitude: single; ofsX,ofsY: integer;
                   ColorScan : IBGRAScanner);
    procedure DrawColorSSE(dest: TBGRACustomBitmap; map: TBGRACustomBitmap; mapAltitude: single; ofsX,ofsY: integer;
                   Color : TBGRAPixel);
    {$endif}
  end;

{ Create a grayscale height map for a cone (may not be precise enough) }
function CreateConeMap(size: integer): TBGRABitmap;

{ Create a precise height map for a cone (not grayscale anymore but more precise) }
function CreateConePreciseMap(width,height: integer): TBGRABitmap;

{ Create a precise height map for a vertical cone (not grayscale anymore but more precise) }
function CreateVerticalConePreciseMap(width,height: integer): TBGRABitmap;

{ Create a precise height map for a vertical cylinder (not grayscale anymore but more precise) }
function CreateVerticalCylinderPreciseMap(width,height: integer): TBGRABitmap;

{ Create a precise height map for an horizontal cylinder (not grayscale anymore but more precise) }
function CreateHorizontalCylinderPreciseMap(width,height: integer): TBGRABitmap;

{ Create a grayscale height map for a sphere (may not be precise enough) }
function CreateSphereMap(width,height: integer): TBGRABitmap;

{ Create a precise height map for a sphere (not grayscale anymore but more precise) }
function CreateSpherePreciseMap(width,height: integer): TBGRABitmap;

{ Create a rectangle height map with a border }
function CreateRectangleMap(width,height,border: integer; options: TRectangleMapOptions = []): TBGRABitmap;

{ Create a precise height map for a rectangle height map with a border (not grayscale anymore but more precise) }
function CreateRectanglePreciseMap(width,height,border: integer; options: TRectangleMapOptions = []): TBGRABitmap;
function CreateRectanglePreciseMap(width, height, borderWidth, borderHeight: integer; options: TRectangleMapOptions): TBGRABitmap;

{ Create a round rectangle height map with a border }
function CreateRoundRectangleMap(width,height,border: integer; options: TRectangleMapOptions = []): TBGRABitmap;

{ Create a precise height map for a round rectangle height map with a border (not grayscale anymore but more precise) }
function CreateRoundRectanglePreciseMap(width,height,border: integer; options: TRectangleMapOptions = []): TBGRABitmap;
function CreateRoundRectanglePreciseMap(width,height,borderWidth,borderHeight: integer; options: TRectangleMapOptions = []): TBGRABitmap;

{---------- Perlin Noise -------------}
{ Random image using a superposition of interpolated random values.
  See : http://wiki.lazarus.freepascal.org/Perlin_Noise
        http://freespace.virgin.net/hugo.elias/models/m_perlin.htm }

{ Creates a non-tilable random grayscale image }
function CreatePerlinNoiseMap(AWidth, AHeight: integer; HorizontalPeriod: Single = 1;
  VerticalPeriod: Single = 1; Exponent: Double = 1; ResampleFilter: TResampleFilter = rfCosine): TBGRABitmap;

{ Creates a tilable random grayscale image }
function CreateCyclicPerlinNoiseMap(AWidth, AHeight: integer; HorizontalPeriod: Single = 1;
  VerticalPeriod: Single = 1; Exponent: Double = 1; ResampleFilter: TResampleFilter = rfCosine): TBGRABitmap;

Implementation
End.

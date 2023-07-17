// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAFreeType;

{$mode objfpc}{$H+}

{
  Font rendering units : BGRAText, BGRATextFX, BGRAVectorize, BGRAFreeType

  This units provide a font renderer with FreeType fonts, using the integrated FreeType font engine in Lazarus.
  The simplest way to render effects is to use TBGRAFreeTypeFontRenderer class.
  To do this, create an instance of this class and assign it to a TBGRABitmap.FontRenderer property. Now functions
  to draw text like TBGRABitmap.TextOut will use the chosen renderer.

  >> Note that you need to define the default FreeType font collection
  >> using EasyLazFreeType unit.

  To set the effects, keep a variable containing
  the TBGRAFreeTypeFontRenderer class and modify ShadowVisible and other effects parameters. The FontHinted property
  allows you to choose if the font is snapped to pixels to make it more readable.

  TBGRAFreeTypeDrawer class is the class that provides basic FreeType drawing
  by deriving the TFreeTypeDrawer type. You can use it directly, but it is not
  recommended, because there are less text layout parameters. However, it is
  necessary if you want to create TBGRATextEffect objects using FreeType fonts.
}

interface

{$i bgrabitmap.map.inc}

uses
  BGRAClasses, SysUtils, BGRAGraphics, BGRABitmapTypes, EasyLazFreeType, FPimage,
  BGRACustomTextFX, BGRAPhongTypes, BGRATypewriter, LazVersion;

{$IF laz_fullversion >= 2001000}
  {$DEFINE LAZFREETYPE_GLYPH_BOX_FIXED}
{$ENDIF}

{$IF laz_fullversion >= 2010000}
  {$DEFINE LAZFREETYPE_PROVIDE_KERNING}
{$ENDIF}

type
  TBGRAFreeTypeDrawer = class;

  //this is the class to assign to FontRenderer property of TBGRABitmap
  { TBGRAFreeTypeFontRenderer }

  TBGRAFreeTypeFontRenderer = class(TBGRACustomFontRenderer)
  private
    FDrawer: TBGRAFreeTypeDrawer;
    FFont: TFreeTypeFont;
    FLastFontSize: single;
    function GetCollection: TCustomFreeTypeFontCollection;
    function GetDrawer(ASurface: TBGRACustomBitmap): TBGRAFreeTypeDrawer;
    function GetShaderLightPosition: TPoint;
    function GetShaderLightPositionF: TPointF;
    procedure SetShaderLightPosition(const AValue: TPoint);
    procedure SetShaderLightPositionF(const AValue: TPointF);
  protected
    FShaderOwner: boolean;
    FShader: TCustomPhongShading;
    FTypeWriter: TBGRACustomTypeWriter;
    function GetTypeWriter: TBGRACustomTypeWriter;
    procedure UpdateFont(ADisableClearType: boolean = false);
    procedure Init;
    procedure TextOutAnglePatch(ADest: TBGRACustomBitmap; x, y: single; orientation: integer; s: string;
              c: TBGRAPixel; tex: IBGRAScanner; align: TAlignment);
    procedure InternalTextOut(ADest: TBGRACustomBitmap; x, y: single; s: string; c: TBGRAPixel; align: TAlignment);
    property TypeWriter: TBGRACustomTypeWriter read GetTypeWriter;
  public
    FontHinted: boolean;

    ShaderActive: boolean;

    ShadowVisible: boolean;
    ShadowColor: TBGRAPixel;
    ShadowRadius: integer;
    ShadowOffset: TPoint;
    ShadowQuality: TRadialBlurType;

    OutlineColor: TBGRAPixel;
    OutlineVisible,OuterOutlineOnly: boolean;
    OutlineTexture: IBGRAScanner;

    constructor Create; overload;
    constructor Create(AShader: TCustomPhongShading; AShaderOwner: boolean); overload;
    function FontExists(AName: string): boolean; override;
    function TextVisible(const AColor: TBGRAPixel): boolean; override;
    function GetFontPixelMetric: TFontPixelMetric; override;
    function GetFontPixelMetricF: TFontPixelMetricF; override;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientation: integer; s: string; c: TBGRAPixel; align: TAlignment); overload; override;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientation: integer; s: string; texture: IBGRAScanner; align: TAlignment); overload; override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; s: string; texture: IBGRAScanner; align: TAlignment); overload; override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; s: string; c: TBGRAPixel; align: TAlignment); overload; override;
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; s: string; style: TTextStyle; c: TBGRAPixel); overload; override;
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; s: string; style: TTextStyle; texture: IBGRAScanner); overload; override;
    function TextSize(sUTF8: string): TSize; overload; override;
    function TextSizeF(sUTF8: string): TPointF; overload; override;
    function TextSize(sUTF8: string; AMaxWidth: integer; {%H-}ARightToLeft: boolean): TSize; overload; override;
    function TextSizeF(sUTF8: string; AMaxWidthF: single; {%H-}ARightToLeft: boolean): TPointF; overload; override;
    function TextFitInfo(sUTF8: string; AMaxWidth: integer): integer; override;
    function TextFitInfoF(sUTF8: string; AMaxWidthF: single): integer; override;
    destructor Destroy; override;
    property Collection: TCustomFreeTypeFontCollection read GetCollection;
    property ShaderLightPosition: TPoint read GetShaderLightPosition write SetShaderLightPosition;
    property ShaderLightPositionF: TPointF read GetShaderLightPositionF write SetShaderLightPositionF;
  end;

  { TBGRAFreeTypeDrawer }

  TBGRAFreeTypeDrawer = class(TFreeTypeDrawer)
  private
    FMask: TBGRACustomBitmap;
    FColor: TBGRAPixel;
    FInCreateTextEffect: boolean;
    procedure RenderDirectly(x, y, tx: integer; data: pointer);
    procedure RenderDirectlyClearType(x, y, tx: integer; data: pointer);
    function ShadowActuallyVisible :boolean;
    function OutlineActuallyVisible: boolean;
    function ShaderActuallyActive : boolean;
  public
    Destination: TBGRACustomBitmap;
    ClearTypeRGBOrder: boolean;
    Texture: IBGRAScanner;

    Shader: TCustomPhongShading;
    ShaderActive: boolean;

    ShadowVisible: boolean;
    ShadowColor: TBGRAPixel;
    ShadowRadius: integer;
    ShadowOffset: TPoint;
    ShadowQuality: TRadialBlurType;

    OutlineColor: TBGRAPixel;
    OutlineVisible,OuterOutlineOnly: boolean;
    OutlineTexture: IBGRAScanner;

    constructor Create(ADestination: TBGRACustomBitmap);
    procedure DrawText(AText: string; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TFPColor); overload; override;
    procedure DrawText(AText: string; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TBGRAPixel); overload;
    procedure DrawText(AText: string; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TBGRAPixel; AAlign: TFreeTypeAlignments); overload;
    { If this code does not compile, you probably have an older version of Lazarus. To fix the problem,
      go into "bgrabitmap.inc" and comment the compiler directives }
    {$IFDEF BGRABITMAP_USE_LCL12}
    procedure DrawTextWordBreak(AText: string; AFont: TFreeTypeRenderableFont; x, y, AMaxWidth: Single; AColor: TBGRAPixel; AAlign: TFreeTypeAlignments); overload;
    procedure DrawTextRect(AText: string; AFont: TFreeTypeRenderableFont; X1,Y1,X2,Y2: Single; AColor: TBGRAPixel; AAlign: TFreeTypeAlignments); overload;
    {$ENDIF}
    {$IFDEF BGRABITMAP_USE_LCL15}
    procedure DrawGlyph(AGlyph: integer; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TFPColor); overload; override;
    procedure DrawGlyph(AGlyph: integer; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TBGRAPixel); overload;
    procedure DrawGlyph(AGlyph: integer; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TBGRAPixel; AAlign: TFreeTypeAlignments); overload;
    {$ENDIF}
    function CreateTextEffect(AText: string; AFont: TFreeTypeRenderableFont): TBGRACustomTextEffect;
    destructor Destroy; override;
  end;


Implementation
End.

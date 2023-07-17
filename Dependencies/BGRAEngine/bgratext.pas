// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAText;

{$mode objfpc}{$H+}
{$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
{$i bgrabitmap.map.inc}

interface

{$IFDEF LINUX}
  {$DEFINE SYSTEM_RENDERER_IS_FINE}
  {$DEFINE SYSTEM_CLEARTYPE_RENDERER_IS_FINE}
  {$DEFINE RENDER_TEXT_ON_TBITMAP}
{$ENDIF}
{$IFDEF FREEBSD}
  {$DEFINE SYSTEM_RENDERER_IS_FINE}
  {$DEFINE SYSTEM_CLEARTYPE_RENDERER_IS_FINE}
{$ENDIF}
{$IFDEF DARWIN}
  {$DEFINE SYSTEM_RENDERER_IS_FINE}
  {$DEFINE RENDER_TEXT_ON_TBITMAP}
{$ENDIF}
{$IFDEF WINDOWS}
  {$IFNDEF LEGACY_FONT_VERTICAL_OFFSET}
    {$DEFINE FIX_FONT_VERTICAL_OFFSET}
  {$ENDIF}
{$ENDIF}
{$IFDEF BGRABITMAP_USE_MSEGUI}
  {$DEFINE RENDER_TEXT_ON_TBITMAP}
{$ENDIF}

{
  Font rendering units : BGRAText, BGRATextFX, BGRAVectorize, BGRAFreeType

  This unit provides basic text rendering functions using system renderer 
  (LCL or MSEgui).

  Text functions use a temporary bitmap where the operating system text drawing 
  is used. Then it is scaled down (if antialiasing is activated) and colored.

  These routines are rather slow, so you may use other font renderers
  like TBGRATextEffectFontRenderer in BGRATextFX if you want to use LCL fonts,
  or, if you have TrueType fonts files, you may use TBGRAFreeTypeFontRenderer
  in BGRAFreeType. }

uses
  BGRAClasses, SysUtils, BGRAGraphics, BGRABitmapTypes, BGRAPen, BGRAGrayscaleMask
  {$IFDEF LCL},InterfaceBase, LCLVersion{$ENDIF};

const
  RenderTextOnBitmap = {$IFDEF RENDER_TEXT_ON_TBITMAP}true{$ELSE}false{$ENDIF};

type
  TWordBreakHandler = BGRABitmapTypes.TWordBreakHandler;

  { TBGRASystemFontRenderer }

  TBGRASystemFontRenderer = class(TBGRACustomFontRenderer)
  protected
    FFont: TFont;             //font parameters
    FWordBreakHandler: TWordBreakHandler;
    FOwnUnderline: boolean;
    procedure UpdateFont; virtual;
    function InternalTextSize(sUTF8: string; AShowPrefix: boolean): TSize;
    function InternalTextSizeAngle(sUTF8: string; AShowPrefix: boolean; AOrientation: integer): TSize; virtual;
    procedure InternalTextWordBreak(ADest: TBGRACustomBitmap; ATextUTF8: string;
                                    x, y, AMaxWidth: integer; AColor: TBGRAPixel; ATexture: IBGRAScanner;
                                    AHorizAlign: TAlignment; AVertAlign: TTextLayout; ARightToLeft: boolean);
    procedure InternalTextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; c: TBGRAPixel; ATexture: IBGRAScanner);
    procedure InternalTextOut(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; c: TBGRAPixel; texture: IBGRAScanner;
                              align: TAlignment; AShowPrefix: boolean = false; ARightToLeft: boolean = false); 
    procedure InternalTextOutAngle(ADest: TBGRACustomBitmap; x, y: single; AOrientation: integer; sUTF8: string; c: TBGRAPixel; texture: IBGRAScanner;
                              align: TAlignment; AShowPrefix: boolean = false; ARightToLeft: boolean = false); virtual;
    procedure InternalTextOutEllipse(ADest: TBGRACustomBitmap; x, y, availableWidth: single; sUTF8: string; c: TBGRAPixel; texture: IBGRAScanner;
                              align: TAlignment; AShowPrefix: boolean = false; ARightToLeft: boolean = false);
    procedure InternalSplitText(var ATextUTF8: string; AMaxWidth: integer; out ARemainsUTF8: string; out ALineEndingBreak: boolean;
                                AWordBreak: TWordBreakHandler); overload;
    procedure InternalSplitText(var ATextUTF8: string; AMaxWidth: integer; out ARemainsUTF8: string;
                                AWordBreak: TWordBreakHandler); overload;
    function InternalGetFontPixelMetric: TFontPixelMetric;
    procedure DefaultWorkBreakHandler(var ABeforeUTF8, AAfterUTF8: string);
  public
    OverrideUnderlineDecoration: boolean; // draw unerline according to computed font pixel metric instead of using system rendering of underline
    procedure SplitText(var ATextUTF8: string; AMaxWidth: integer; out ARemainsUTF8: string);
    function GetFontPixelMetric: TFontPixelMetric; override;
    function FontExists(AName: string): boolean; override;
    class function PatchSystemFontName(AName: string): string;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientationTenthDegCCW: integer; sUTF8: string; c: TBGRAPixel; align: TAlignment); overload; override;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientationTenthDegCCW: integer; sUTF8: string; c: TBGRAPixel; align: TAlignment; ARightToLeft: boolean); overload; override;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientationTenthDegCCW: integer; sUTF8: string; texture: IBGRAScanner; align: TAlignment); overload; override;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientationTenthDegCCW: integer; sUTF8: string; texture: IBGRAScanner; align: TAlignment; ARightToLeft: boolean); overload; override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; texture: IBGRAScanner; align: TAlignment); overload; override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; c: TBGRAPixel; align: TAlignment); overload; override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; texture: IBGRAScanner; align: TAlignment; ARightToLeft: boolean); overload; override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; c: TBGRAPixel; align: TAlignment; ARightToLeft: boolean); overload; override;
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; c: TBGRAPixel); overload; override;
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; texture: IBGRAScanner); overload; override;
    procedure TextWordBreak(ADest: TBGRACustomBitmap; AText: string; x, y, AMaxWidth: integer; AColor: TBGRAPixel; AHorizAlign: TAlignment; AVertAlign: TTextLayout; ARightToLeft: boolean = false); overload;
    procedure TextWordBreak(ADest: TBGRACustomBitmap; AText: string; x, y, AMaxWidth: integer; ATexture: IBGRAScanner; AHorizAlign: TAlignment; AVertAlign: TTextLayout; ARightToLeft: boolean = false); overload;
    function TextSize(sUTF8: string): TSize; overload; override;
    function TextSizeAngle(sUTF8: string; orientationTenthDegCCW: integer): TSize; override;
    function TextSize(sUTF8: string; AMaxWidth: integer; {%H-}ARightToLeft: boolean): TSize; overload; override;
    function TextFitInfo(sUTF8: string; AMaxWidth: integer): integer; override;
    constructor Create;
    destructor Destroy; override;
    property OnWordBreak: TWordBreakHandler read FWordBreakHandler write FWordBreakHandler;
  end;

{$IFDEF BGRABITMAP_USE_MSEGUI}
  TMSEFontRenderer = class(TBGRASystemFontRenderer);
{$ELSE}
  TLCLFontRenderer = class(TBGRASystemFontRenderer);
{$ENDIF}

function CleanTextOutString(s: string): string; //this works with UTF8 strings as well
function RemoveLineEnding(var s: string; indexByte: integer): boolean; //this works with UTF8 strings however the index is the byte index
function RemoveLineEndingUTF8(var sUTF8: string; indexUTF8: integer): boolean;

procedure BGRATextOut(bmp: TBGRACustomBitmap; Font: TFont; Quality: TBGRAFontQuality; xf, yf: single; sUTF8: string;
  c: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; CustomAntialiasingLevel: Integer = 0;
  ShowPrefix: boolean = false; RightToLeft: boolean = false);

procedure BGRATextOutAngle(bmp: TBGRACustomBitmap; Font: TFont; Quality: TBGRAFontQuality; xf, yf: single; orientationTenthDegCCW: integer;
  sUTF8: string; c: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; CustomAntialiasingLevel: Integer = 0);

procedure BGRATextRect(bmp: TBGRACustomBitmap; Font: TFont; Quality: TBGRAFontQuality; ARect: TRect; xf, yf: single;
  sUTF8: string; style: TTextStyle; c: TBGRAPixel; tex: IBGRAScanner; CustomAntialiasingLevel: Integer = 0);

function BGRATextSize(Font: TFont; Quality: TBGRAFontQuality; sUTF8: string; CustomAntialiasingLevel: Integer): TSize;
function BGRATextSizeAngle(Font: TFont; AOrientation: integer; Quality: TBGRAFontQuality; sUTF8: string; CustomAntialiasingLevel: Integer): TSize;
function BGRATextFitInfo(Font: TFont; Quality: TBGRAFontQuality; sUTF8: string; CustomAntialiasingLevel: Integer; AMaxWidth: integer): integer;
function BGRATextFitInfoAngle(Font: TFont; AOrientation: integer; Quality: TBGRAFontQuality; sUTF8: string; CustomAntialiasingLevel: Integer; AMaxWidth: integer): integer;
function BGRAOriginalTextSize(Font: TFont; Quality: TBGRAFontQuality; sUTF8: string; CustomAntialiasingLevel: integer): TSize;
function BGRAOriginalTextSizeAngle(Font: TFont; AOrientation: integer; Quality: TBGRAFontQuality; sUTF8: string; CustomAntialiasingLevel: Integer): TSize;
function BGRAOriginalTextSizeEx(Font: TFont; Quality: TBGRAFontQuality; sUTF8: string; CustomAntialiasingLevel: Integer;
                                out actualAntialiasingLevel: integer; out extraVerticalMarginDueToRotation: integer): TSize;
function BGRAOriginalTextSizeExAngle(Font: TFont; AOrientation: integer; Quality: TBGRAFontQuality; sUTF8: string; CustomAntialiasingLevel: Integer;
                                out actualAntialiasingLevel: integer; out extraVerticalMarginDueToRotation: integer): TSize;

function BGRATextUnderline(ATopLeft: TPointF; AWidth: Single; AMetrics: TFontPixelMetric): ArrayOfTPointF; overload;
function BGRATextUnderline(ATopLeft: TPointF; AWidth: Single; ABaseline, AEmHeight: single): ArrayOfTPointF; overload;
function BGRATextStrikeOut(ATopLeft: TPointF; AWidth: Single; AMetrics: TFontPixelMetric): ArrayOfTPointF; overload;
function BGRATextStrikeOut(ATopLeft: TPointF; AWidth: Single; ABaseline, AEmHeight, AXHeight: single): ArrayOfTPointF; overload;

function GetFontHeightSign: integer;
function FontEmHeightSign: integer;
function FontFullHeightSign: integer;
function SystemFontAvailable: boolean;
function GetFineClearTypeAuto: TBGRAFontQuality;
function FixSystemFontFullHeight({%H-}AFontName: string; AFontHeight: integer): integer;

{$IFDEF LCL}
function LCLFontAvailable: boolean;
function FixLCLFontFullHeight(AFontName: string; AFontHeight: integer): integer;
{$ENDIF}

procedure BGRAFillClearTypeGrayscaleMask(dest: TBGRACustomBitmap; x,y: integer; xThird: integer; mask: TGrayscaleMask; color: TBGRAPixel; texture: IBGRAScanner = nil; RGBOrder: boolean=true);
procedure BGRAFillClearTypeMask(dest: TBGRACustomBitmap; x,y: integer; xThird: integer; mask: TBGRACustomBitmap; color: TBGRAPixel; texture: IBGRAScanner = nil; RGBOrder: boolean=true);
procedure BGRAFillClearTypeRGBMask(dest: TBGRACustomBitmap; x,y: integer; mask: TBGRACustomBitmap; color: TBGRAPixel; texture: IBGRAScanner = nil; KeepRGBOrder: boolean=true);

const FontAntialiasingLevel = {$IFDEF SYSTEM_RENDERER_IS_FINE}3{$ELSE}6{$ENDIF};
const FontDefaultQuality = fqAntialiased;
const IsLclFontRendererFine = {$IFDEF SYSTEM_RENDERER_IS_FINE}true{$ELSE}false{$ENDIF};

function GetLCLFontPixelMetric(AFont: TFont): TFontPixelMetric;

var
  BGRATextOutImproveReadabilityProc : procedure (bmp: TBGRACustomBitmap; AFont: TFont; xf,yf: single; text: string; color: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; mode : TBGRATextOutImproveReadabilityMode);

procedure BitmapTextOut(ABitmap: TBitmap; ACoord: TPoint; AText: string);
procedure BitmapTextOutAngle(ABitmap: TBitmap; ACoord: TPoint; AText: string; AOrientation: integer);
procedure BitmapTextRect(ABitmap: TBitmap; ARect: TRect; ACoord: TPoint; 
  AText: string; const AStyle: TTextStyle);
function BitmapTextExtent(ABitmap: TBitmap; AText: string): TSize;
function BitmapTextExtentAngle(ABitmap: TBitmap; AText: string; AOrientation: integer): TSize;
function BitmapTextFitInfo(ABitmap: TBitmap; AText: string; AMaxWidth: integer): integer;
function BitmapTextFitInfoAngle(ABitmap: TBitmap; AText: string; AMaxWidth: integer; AOrientation: integer): integer;
procedure BitmapFillRect(ABitmap: TBitmap; ARect: TRect; AColor: TColor);

Implementation
End.

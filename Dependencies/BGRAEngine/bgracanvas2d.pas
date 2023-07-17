// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRACanvas2D;

{ To do :

  draw text with a different precision if the matrix is scaled
  drawImage(in image, in double sx, in double sy, in double sw, in double sh, in double dx, in double dy, in double dw, in double dh)
  -> using FillPoly with texture coordinates
  linear gradient any transformation
  clearPath clipping
  createRadialGradient
  globalCompositeOperation
  image data functions
}

{$mode objfpc}{$H+}
{$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
{$WARN 5093 off : function result variable of a managed type does not seem to be initialized}
interface

uses
  BGRAClasses, SysUtils, BGRAGraphics, BGRABitmapTypes, BGRATransform,
  BGRAGradientScanner, BGRAPath, BGRAPen, BGRAGrayscaleMask;

type
  ArrayOfString = array of string;

  IBGRACanvasTextureProvider2D = interface
    function getTexture: IBGRAScanner;
    property texture: IBGRAScanner read GetTexture;
  end;

  IBGRACanvasGradient2D = interface(IBGRACanvasTextureProvider2D)
    procedure addColorStop(APosition: single; AColor: TBGRAPixel);
    procedure addColorStop(APosition: single; AColor: TColor);
    procedure addColorStop(APosition: single; AColor: string);
    procedure setColors(ACustomGradient: TBGRACustomGradient);
    function GetGammaCorrection: boolean;
    procedure SetGammaCorrection(AValue: boolean);
    function GetRepetition: TBGRAGradientRepetition;
    procedure SetRepetition(AValue: TBGRAGradientRepetition);
    property gammaCorrection: boolean read GetGammaCorrection write SetGammaCorrection;
    property repetition: TBGRAGradientRepetition read GetRepetition write SetRepetition;
  end;

  { TBGRACanvasTextureProvider2D }

  TBGRACanvasTextureProvider2D = class(TInterfacedObject,IBGRACanvasTextureProvider2D)
    function getTexture: IBGRAScanner; virtual; abstract;
  end;

  { TBGRACanvasState2D }

  TBGRACanvasState2D = class
  private
    FClipMask: TGrayscaleMask;
    FClipMaskOwned: boolean;
    function GetClipMaskReadWrite: TGrayscaleMask;
  public
    strokeColor: TBGRAPixel;
    strokeTextureProvider: IBGRACanvasTextureProvider2D;
    fillColor: TBGRAPixel;
    fillMode: TFillMode;
    fillTextureProvider: IBGRACanvasTextureProvider2D;
    globalAlpha: byte;

    fontName: string;
    fontStyle: TFontStyles;
    fontEmHeight: single;
    textAlign: TAlignment;
    textBaseline: string;
    textDirection: TFontBidiMode;

    lineWidth: single;
    penStroker: TBGRAPenStroker;

    shadowOffsetX,shadowOffsetY,shadowBlur: single;
    shadowColor: TBGRAPixel;
    shadowFastest: boolean;

    matrix: TAffineMatrix;
    constructor Create(AMatrix: TAffineMatrix; AClipMask: TGrayscaleMask; AClipMaskOwned: boolean);
    function Duplicate: TBGRACanvasState2D;
    destructor Destroy; override;
    procedure transform(AMatrix: TAffineMatrix);
    procedure SetClipMask(AClipMask: TGrayscaleMask; AOwned: boolean);
    property clipMaskReadOnly: TGrayscaleMask read FClipMask;
    property clipMaskReadWrite: TGrayscaleMask read GetClipMaskReadWrite;
  end;

  TCanvas2dTextSize = record
    width,height: single;
  end;

  { TBGRACanvas2D }

  TBGRACanvas2D = class(IBGRAPath)
  private
    FSurface: TBGRACustomBitmap;
    StateStack: TList;
    currentState: TBGRACanvasState2D;
    FCanvasOffset: TPointF;
    FPixelCenteredCoordinates: boolean;
    FPathPoints: array of TPointF;
    FPathPointCount: integer;
    FTextPaths: array of record
        Text: string;
        FontName: string;
        FontMatrix: TAffineMatrix;
        FontAlign: TAlignment;
        FontAnchor: TFontVerticalAnchor;
        FontStyle: TFontStyles;
        TextDirection: TFontBidiMode;
      end;
    FFontRenderer: TBGRACustomFontRenderer;
    FLastCoord, FStartCoord: TPointF;
    function GetCurrentPathAsPoints: ArrayOfTPointF;
    function GetTextDirection: TFontBidiMode;
    function GetFontName: string;
    function GetFontRenderer: TBGRACustomFontRenderer;
    function GetFontEmHeight: single;
    function GetFontString: string;
    function GetFontStyle: TFontStyles;
    function GetGlobalAlpha: single;
    function GetHasShadow: boolean;
    function GetHeight: Integer;
    function GetLineCap: string;
    function GetLineCapLCL: TPenEndCap;
    function GetlineJoin: string;
    function GetlineJoinLCL: TPenJoinStyle;
    function GetLineWidth: single;
    function GetMatrix: TAffineMatrix;
    function GetMiterLimit: single;
    function GetPixelCenteredCoordinates: boolean;
    function GetShadowBlur: single;
    function GetShadowFastest: boolean;
    function GetShadowOffset: TPointF;
    function GetShadowOffsetX: single;
    function GetShadowOffsetY: single;
    function GetStrokeMatrix: TAffineMatrix;
    function GetTextAlign: string;
    function GetTextAlignLCL: TAlignment;
    function GetTextBaseline: string;
    function GetFillMode: TFillMode;
    function GetWidth: Integer;
    procedure SetTextDirection(AValue: TFontBidiMode);
    procedure SetFontName(AValue: string);
    procedure SetFontRenderer(AValue: TBGRACustomFontRenderer);
    procedure SetFontEmHeight(AValue: single);
    procedure SetFontString(AValue: string);
    procedure SetFontStyle(AValue: TFontStyles);
    procedure SetGlobalAlpha(const AValue: single);
    procedure SetLineCap(const AValue: string);
    procedure SetLineCapLCL(AValue: TPenEndCap);
    procedure SetLineJoin(const AValue: string);
    procedure FillPoly(const points: array of TPointF);
    procedure FillStrokePoly(const points: array of TPointF; fillOver: boolean);
    procedure FillTexts(AErase: boolean);
    procedure SetLineJoinLCL(AValue: TPenJoinStyle);
    procedure SetLineWidth(const AValue: single);
    procedure SetMatrix(AValue: TAffineMatrix);
    procedure SetMiterLimit(const AValue: single);
    procedure SetPixelCenteredCoordinates(const AValue: boolean);
    procedure SetShadowBlur(const AValue: single);
    procedure SetShadowFastest(AValue: boolean);
    procedure SetShadowOffset(const AValue: TPointF);
    procedure SetShadowOffsetX(const AValue: single);
    procedure SetShadowOffsetY(const AValue: single);
    procedure SetStrokeMatrix(AValue: TAffineMatrix);
    procedure SetTextAlign(AValue: string);
    procedure SetTextAlignLCL(AValue: TAlignment);
    procedure SetTextBaseline(AValue: string);
    procedure SetFillMode(mode: TFillMode);
    procedure StrokePoly(const points: array of TPointF);
    procedure DrawShadow(const points, points2: array of TPointF; AFillMode: TFillMode = fmWinding);
    procedure DrawShadowMask(X,Y: integer; AMask: TCustomUniversalBitmap; AMaskOwned: boolean);
    procedure ClearPoly(const points: array of TPointF);
    function ApplyTransform(const points: array of TPointF; matrix: TAffineMatrix): ArrayOfTPointF; overload;
    function ApplyTransform(const points: array of TPointF): ArrayOfTPointF; overload;
    function ApplyTransform(point: TPointF): TPointF; overload;
    function GetPenPos(defaultX, defaultY: single): TPointF;
    function GetPenPos(defaultPt: TPointF): TPointF;
    procedure AddPoint(point: TPointF);
    procedure AddPoints(const points: array of TPointF);
    procedure AddPointsRev(const points: array of TPointF);
    function ApplyGlobalAlpha(color: TBGRAPixel): TBGRAPixel;
    function GetDrawMode: TDrawMode;
    procedure copyTo({%H-}dest: IBGRAPath); //IBGRAPath
    function getPoints: ArrayOfTPointF; //IBGRAPath
    function getPoints(AMatrix: TAffineMatrix): ArrayOfTPointF; //IBGRAPath
    function getCursor: TBGRACustomPathCursor; //IBGRAPath
  public
    antialiasing, linearBlend, gradientGammaCorrection: boolean;
    constructor Create(ASurface: TBGRACustomBitmap);
    destructor Destroy; override;

    function toDataURL(mimeType: string = 'image/png'): string;

    procedure save;
    procedure restore;
    procedure scale(x,y: single); overload;
    procedure scale(factor: single); overload;
    procedure rotate(angleRadCW: single);
    procedure translate(x,y: single);
    procedure skewx(angleRadCW: single);
    procedure skewy(angleRadCW: single);
    procedure transform(m11,m21, m12,m22, m13,m23: single); overload;
    procedure transform(AMatrix: TAffineMatrix); overload;
    procedure setTransform(m11,m21, m12,m22, m13,m23: single);
    procedure resetTransform;

    procedure strokeScale(x,y: single);
    procedure strokeSkewx(angleRadCW: single);
    procedure strokeSkewy(angleRadCW: single);
    procedure strokeResetTransform;

    procedure strokeStyle(color: TBGRAPixel); overload;
    procedure strokeStyle(color: TColor); overload;
    procedure strokeStyle(color: string); overload;
    procedure strokeStyle(texture: IBGRAScanner); overload;
    procedure strokeStyle(provider: IBGRACanvasTextureProvider2D); overload;
    procedure fillStyle(color: TBGRAPixel); overload;
    procedure fillStyle(color: TColor); overload;
    procedure fillStyle(color: string); overload;
    procedure fillStyle(texture: IBGRAScanner); overload;
    procedure fillStyle(provider: IBGRACanvasTextureProvider2D); overload;
    procedure shadowColor(color: TBGRAPixel); overload;
    procedure shadowColor(color: TColor); overload;
    procedure shadowColor(color: string); overload;
    procedure shadowNone;
    function getShadowColor: TBGRAPixel;

    function createLinearGradient(x0,y0,x1,y1: single): IBGRACanvasGradient2D; overload;
    function createLinearGradient(p0,p1: TPointF): IBGRACanvasGradient2D; overload;
    function createLinearGradient(x0,y0,x1,y1: single; Colors: TBGRACustomGradient): IBGRACanvasGradient2D; overload;
    function createLinearGradient(p0,p1: TPointF; Colors: TBGRACustomGradient): IBGRACanvasGradient2D; overload;

    function createRadialGradient(x0,y0,r0,x1,y1,r1: single; flipGradient: boolean=false): IBGRACanvasGradient2D; overload;
    function createRadialGradient(p0: TPointF; r0: single; p1: TPointF; r1: single; flipGradient: boolean=false): IBGRACanvasGradient2D; overload;
    function createRadialGradient(x0,y0,r0,x1,y1,r1: single; Colors: TBGRACustomGradient; flipGradient: boolean=false): IBGRACanvasGradient2D; overload;
    function createRadialGradient(p0: TPointF; r0: single; p1: TPointF; r1: single; Colors: TBGRACustomGradient; flipGradient: boolean=false): IBGRACanvasGradient2D; overload;

    function createPattern(image: TBGRACustomBitmap; repetition: string): IBGRACanvasTextureProvider2D; overload;
    function createPattern(texture: IBGRAScanner): IBGRACanvasTextureProvider2D; overload;

    procedure fillRect(x,y,w,h: single);
    procedure strokeRect(x,y,w,h: single);
    procedure clearRect(x,y,w,h: single);

    procedure addPath(APath: IBGRAPath); overload;
    procedure addPath(ASvgPath: string); overload;
    procedure path(APath: IBGRAPath); overload;
    procedure path(ASvgPath: string); overload;
    procedure beginPath;
    procedure closePath;
    procedure toSpline(closed: boolean; style: TSplineStyle= ssOutside);
    procedure moveTo(x,y: single); overload;
    procedure lineTo(x,y: single); overload;
    procedure moveTo(constref pt: TPointF); overload;
    procedure lineTo(constref pt: TPointF); overload;
    procedure polylineTo(const pts: array of TPointF);
    procedure quadraticCurveTo(cpx,cpy,x,y: single); overload;
    procedure quadraticCurveTo(constref cp,pt: TPointF); overload;
    procedure bezierCurveTo(cp1x,cp1y,cp2x,cp2y,x,y: single); overload;
    procedure bezierCurveTo(constref cp1,cp2,pt: TPointF); overload;
    procedure rect(x,y,w,h: single);
    procedure roundRect(x,y,w,h,radius: single); overload;
    procedure roundRect(x,y,w,h,rx,ry: single); overload;
    procedure openedSpline(const pts: array of TPointF; style: TSplineStyle);
    procedure closedSpline(const pts: array of TPointF; style: TSplineStyle);
    procedure spline(const pts: array of TPointF; style: TSplineStyle= ssOutside);
    procedure splineTo(const pts: array of TPointF; style: TSplineStyle= ssOutside);
    procedure arc(x, y, radius, startAngleRadCW, endAngleRadCW: single; anticlockwise: boolean); overload;
    procedure arc(x, y, radius, startAngleRadCW, endAngleRadCW: single); overload;
    procedure arc(cx, cy, rx,ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single; anticlockwise: boolean); overload;
    procedure arc(cx, cy, rx,ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single); overload;
    procedure arc(constref arcDef: TArcDef); overload;
    procedure arcTo(x1, y1, x2, y2, radius: single); overload;
    procedure arcTo(p1,p2: TPointF; radius: single); overload;
    procedure arcTo(rx, ry, xAngleRadCW: single; largeArc,anticlockwise: boolean; x, y: single);
    procedure circle(x,y,r: single);
    procedure ellipse(x,y,rx,ry: single);
    procedure text(AText: string; x,y: single);
    procedure fillText(AText: string; x,y: single);
    procedure strokeText(AText: string; x,y: single);
    function measureText(AText: string): TCanvas2dTextSize;

    procedure fill; overload;
    procedure fill(AFillProc: TBGRAPathFillProc; AData: pointer); overload;
    procedure fill(AFillProc: TBGRAPathFillProc; const AMatrix: TAffineMatrix; AData: pointer); overload; //may not render curve nicely
    procedure stroke; overload;
    procedure stroke(ADrawProc: TBGRAPathDrawProc; AData: pointer); overload;
    procedure stroke(ADrawProc: TBGRAPathDrawProc; const AMatrix: TAffineMatrix; AData: pointer); overload; //may not render curve nicely
    procedure fillOverStroke;
    procedure strokeOverFill;
    procedure clearPath;
    procedure clip;
    procedure unclip;
    function isPointInPath(x,y: single): boolean; overload;
    function isPointInPath(pt: TPointF): boolean; overload;

    procedure drawImage(image: TBGRACustomBitmap; dx,dy: single; AFilter: TResampleFilter = rfLinear); overload;
    procedure drawImage(image: TBGRACustomBitmap; dx,dy,dw,dh: single; AFilter: TResampleFilter = rfLinear); overload;

    function getLineStyle: TBGRAPenStyle;
    procedure lineStyle(const AValue: array of single); overload;
    procedure lineStyle(AStyle: TPenStyle); overload;

    class function StrToFontNameList(AText: string): ArrayOfString;
    class function FontNameListToStr(AList: ArrayOfString): string;
    class function CSSFontNameToLCL(AName: string): string;

    property surface: TBGRACustomBitmap read FSurface;
    property width: Integer read GetWidth;
    property height: Integer read GetHeight;
    property pixelCenteredCoordinates: boolean read GetPixelCenteredCoordinates write SetPixelCenteredCoordinates;
    property globalAlpha: single read GetGlobalAlpha write SetGlobalAlpha;
    property matrix: TAffineMatrix read GetMatrix write SetMatrix;
    property strokeMatrix: TAffineMatrix read GetStrokeMatrix write SetStrokeMatrix;

    property lineWidth: single read GetLineWidth write SetLineWidth;
    property lineCap: string read GetLineCap write SetLineCap;
    property lineCapLCL: TPenEndCap read GetLineCapLCL write SetLineCapLCL;
    property lineJoin: string read GetlineJoin write SetLineJoin;
    property lineJoinLCL: TPenJoinStyle read GetlineJoinLCL write SetLineJoinLCL;
    property miterLimit: single read GetMiterLimit write SetMiterLimit;

    property shadowOffsetX: single read GetShadowOffsetX write SetShadowOffsetX;
    property shadowOffsetY: single read GetShadowOffsetY write SetShadowOffsetY;
    property shadowOffset: TPointF read GetShadowOffset write SetShadowOffset;
    property shadowBlur: single read GetShadowBlur write SetShadowBlur;
    property shadowFastest: boolean read GetShadowFastest write SetShadowFastest;
    property hasShadow: boolean read GetHasShadow;

    property fontName: string read GetFontName write SetFontName;
    property fontEmHeight: single read GetFontEmHeight write SetFontEmHeight;
    property fontStyle: TFontStyles read GetFontStyle write SetFontStyle;
    property font: string read GetFontString write SetFontString;
    property textAlignLCL: TAlignment read GetTextAlignLCL write SetTextAlignLCL;
    property textAlign: string read GetTextAlign write SetTextAlign;
    property textBaseline: string read GetTextBaseline write SetTextBaseline;
    property direction: TFontBidiMode read GetTextDirection write SetTextDirection;
    
    property fillMode: TFillMode read GetFillMode write SetFillMode;

    property currentPath: ArrayOfTPointF read GetCurrentPathAsPoints;
    property fontRenderer: TBGRACustomFontRenderer read GetFontRenderer write SetFontRenderer;

  protected
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
  end;

Implementation
End.

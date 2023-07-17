// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRACanvas;

{$mode objfpc}{$H+}
{$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
{$i bgrabitmap.map.inc}

interface

uses
  BGRAClasses, SysUtils, BGRAGraphics, FPImage, BGRABitmapTypes
  {$IFDEF BGRABITMAP_USE_FPCANVAS}, FPCanvas{$ENDIF};

type

  { TBGRAColoredObject }

  TBGRAColoredObject = class
  private
    function GetColor: TColor;
    function GetOpacity: Byte;
    procedure SetColor(const AValue: TColor);
    procedure SetOpacity(const AValue: Byte);
  public
    BGRAColor: TBGRAPixel;
    procedure Assign(Source: TObject); virtual;
    property Color: TColor read GetColor write SetColor;
    property Opacity: Byte read GetOpacity write SetOpacity;
  end;

  { TBGRAPen }

  TBGRAPen = class(TBGRAColoredObject)
  private
    FPenMode: TPenMode;
    function GetActualColor: TBGRAPixel;
    function GetActualDrawMode: TDrawMode;
    function GetActualWidth: integer;
    function GetCustomPenStyle: TBGRAPenStyle;
    function GetInvisible: boolean;
    function GetPenStyle: TPenStyle;
    procedure SetCustomPenStyle(const AValue: TBGRAPenStyle);
    procedure SetPenMode(AValue: TPenMode);
    procedure SetPenStyle(const AValue: TPenStyle);
  protected
    FCustomPenStyle:  TBGRAPenStyle;
    FPenStyle: TPenStyle;
  public
    Width: Integer;
    EndCap: TPenEndCap;
    JoinStyle: TPenJoinStyle;
    constructor Create;
    procedure Assign(Source: TObject); override;
    procedure GetUniversalBrush(out ABrush: TUniversalBrush);
    property Style: TPenStyle read GetPenStyle Write SetPenStyle;
    property Mode: TPenMode read FPenMode write SetPenMode;
    property CustomStyle: TBGRAPenStyle read GetCustomPenStyle write SetCustomPenStyle;
    property ActualWidth: integer read GetActualWidth;
    property ActualColor: TBGRAPixel read GetActualColor;
    property ActualDrawMode: TDrawMode read GetActualDrawMode;
    property Invisible: boolean read GetInvisible;
  end;

  { TBGRABrush }

  TBGRABrush = class(TBGRAColoredObject)
  private
    function GetActualColor: TBGRAPixel;
    function GetActualDrawMode: TDrawMode;
    function GetInvisible: boolean;
    procedure SetBackColor(const AValue: TBGRAPixel);
    procedure SetBrushStyle(const AValue: TBrushStyle);
    procedure SetTexture(AValue: IBGRAScanner);
  protected
    FStyle, FStyleBeforeTexture: TBrushStyle;
    FBackColor: TBGRAPixel;
    FTexture: IBGRAScanner;
    FInternalBitmap: TBGRACustomBitmap;
    FInternalBitmapColor: TBGRAPixel;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TObject); override;
    procedure GetUniversalBrush(ABitmapPrototype: TBGRACustomBitmap; out ABrush: TUniversalBrush);
    function BuildTexture(Prototype: TBGRACustomBitmap): IBGRAScanner;
    property Style: TBrushStyle read FStyle write SetBrushStyle;
    property BackColor: TBGRAPixel read FBackColor write SetBackColor;
    property ActualColor: TBGRAPixel read GetActualColor;
    property ActualDrawMode: TDrawMode read GetActualDrawMode;
    property Invisible: boolean read GetInvisible;
    property Texture: IBGRAScanner read FTexture write SetTexture;
  end;

  { TBGRAFont }

  TBGRAFont = class(TBGRAColoredObject)
  private
    function GetAntialiasing: Boolean;
    procedure SetAntialiasing(const AValue: Boolean);
  public
    Name:   string;
    Height: Integer;
    Style:  TFontStyles;
    Quality : TBGRAFontQuality;
    Orientation:  integer;
    Texture:      IBGRAScanner;
    constructor Create;
    procedure Assign(Source: TObject); override;
    property Antialiasing: Boolean read GetAntialiasing write SetAntialiasing;

  end;

  { TBGRACanvas }

  TBGRACanvas = class
    procedure SetBrush(const AValue: TBGRABrush);
    procedure SetPen(const AValue: TBGRAPen);
    function GetPixelColor(X, Y: Integer): TColor;
    procedure SetPixelColor(X, Y: Integer; const AValue: TColor);
  private
    function GetClipping: Boolean;
    function GetClipRect: TRect;
    function GetExpandedPixel(X, Y: Integer): TExpandedPixel;
    function GetFPPixelColor(X, Y: Integer): TFPColor;
    function GetHeight: integer;
    function GetWidth: integer;
    procedure SetClipping(const AValue: Boolean);
    procedure SetClipRect(const AValue: TRect);
    procedure SetExpandedPixel(X, Y: Integer; const AValue: TExpandedPixel);
    procedure SetFont(const AValue: TBGRAFont);
    procedure SetFPPixelColor(X, Y: Integer; const AValue: TFPColor);
    function ComputeEllipseC(x1, y1, x2, y2: integer; out cx,cy,rx,ry: single): boolean;
    function CheckRectangle(var x1, y1, x2, y2: integer; out tx,ty: integer): boolean;

  protected
    FBitmap: TBGRACustomBitmap;
    FBrush: TBGRABrush;
    FPen: TBGRAPen;
    FPenPos: TPoint;
    FFont : TBGRAFont;
    FInactiveClipRect: TRect;
    FClippingOn: Boolean;
    procedure ApplyPenStyle;
    procedure ApplyFont;
    function NoPen: boolean;
    function NoBrush: boolean;
  public
    AntialiasingMode: TAntialiasingMode;
    FillMode : TFillMode;
    TextStyle : TTextStyle;
    DrawFontBackground : boolean;
    constructor Create(ABitmap: TBGRACustomBitmap);
    destructor Destroy; override;
    procedure MoveTo(x,y: integer); overload;
    procedure MoveTo(p: TPoint); overload;
    procedure LineTo(x,y: integer); overload;
    procedure LineTo(p: TPoint); overload;
    procedure Line(x1,y1,x2,y2: integer); overload;
    procedure Line(p1,p2: TPoint); overload;
    procedure Arc(x1,y1,x2,y2,sx,sy,ex,ey: integer);
    procedure Arc(x1,y1,x2,y2,StartDeg16,LengthDeg16: integer);
    procedure Arc65536(x1,y1,x2,y2: integer; start65536,end65536: word; Options: TArcOptions);
    procedure Chord(x1,y1,x2,y2,sx,sy,ex,ey: integer);
    procedure Chord(x1,y1,x2,y2,StartDeg16,LengthDeg16: integer);
    procedure Pie(x1,y1,x2,y2,sx,sy,ex,ey: integer);
    procedure Pie(x1,y1,x2,y2,StartDeg16,LengthDeg16: integer);
    procedure RadialPie(x1,y1,x2,y2,StartDeg16,LengthDeg16: integer);
    procedure Ellipse(x1,y1,x2,y2: integer);
    procedure Ellipse(const bounds: TRect);
    procedure Rectangle(x1,y1,x2,y2: integer; Filled: Boolean = True);
    procedure Rectangle(const bounds: TRect; Filled: Boolean = True);
    procedure Frame(x1,y1,x2,y2: integer);
    procedure Frame(const bounds: TRect);
    procedure RoundRect(x1,y1,x2,y2: integer; dx,dy: integer);
    procedure RoundRect(const bounds: TRect; dx,dy: integer);
    procedure EllipseC(x,y,rx,ry: integer);
    procedure FillRect(x1,y1,x2,y2: integer);
    procedure FillRect(const bounds: TRect);
    procedure FrameRect(x1,y1,x2,y2: integer; width: integer = 1);
    procedure FrameRect(const bounds: TRect; width: integer = 1);
    procedure Frame3D(var bounds: TRect; width: integer; Style: TGraphicsBevelCut); overload;
    procedure Frame3D(var bounds: TRect; width: integer; Style: TGraphicsBevelCut; LightColor: TBGRAPixel; ShadowColor: TBGRAPixel); overload;
    procedure GradientFill(ARect: TRect; AStart, AStop: TColor;
      ADirection: TGradientDirection; GammaCorrection: Boolean = false);
    procedure FloodFill(X, Y: Integer; FillColor: TColor; FillStyle: TFillStyle);
    procedure FloodFill(X, Y: Integer; FillColor: TBGRAPixel; FillStyle: TFillStyle);
    procedure FloodFill(X, Y: Integer);
    procedure Polygon(const APoints: array of TPoint);
    procedure Polygon(const Points: array of TPoint;
                      Winding: Boolean;
                      StartIndex: Integer = 0;
                      NumPts: Integer = -1);
    procedure Polygon(Points: PPoint; NumPts: Integer;
                      Winding: boolean = False);
    procedure PolygonF(const APoints: array of TPointF);
    procedure PolygonF(const APoints: array of TPointF; Winding: Boolean; FillOnly: Boolean = False);
    procedure Polyline(const APoints: array of TPoint);
    procedure Polyline(const Points: array of TPoint;
                      StartIndex: Integer;
                      NumPts: Integer = -1);
    procedure Polyline(Points: PPoint; NumPts: Integer);
    procedure PolylineF(const APoints: array of TPointF);
    procedure PolyBezier(Points: PPoint; NumPts: Integer;
                         Filled: boolean = False;
                         Continuous: boolean = False);
    procedure PolyBezier(const Points: array of TPoint;
                         Filled: boolean = False;
                         Continuous: boolean = False);
    procedure Draw(X,Y: Integer; SrcBitmap: TBGRACustomBitmap); overload;
    procedure Draw(X,Y: Integer; SrcBitmap: TBitmap); overload;
    procedure CopyRect(X,Y: Integer; SrcBitmap: TBGRACustomBitmap; SrcRect: TRect);
    procedure StretchDraw(DestRect: TRect; SrcBitmap: TBGRACustomBitmap; HorizFlip: Boolean = false; VertFlip: Boolean = false);
    procedure DrawFocusRect(bounds: TRect);
    procedure CopyRect(Dest: TRect; SrcBmp: TBGRACustomBitmap;
                       Source: TRect); virtual;

    procedure TextOut(X,Y: Integer; const Text: String);
    procedure TextRect(const ARect: TRect; X, Y: integer; const Text: string);
    procedure TextRect(ARect: TRect; X, Y: integer; const Text: string;
                       const Style: TTextStyle);
    function TextExtent(const Text: string): TSize;
    function TextHeight(const Text: string): Integer;
    function TextWidth(const Text: string): Integer;

    property Pen: TBGRAPen read FPen write SetPen;
    property PenPos : TPoint read FPenPos write FPenPos;
    property Brush: TBGRABrush read FBrush write SetBrush;
    property Font: TBGRAFont read FFont write SetFont;
    property Pixels[X,Y: Integer]: TColor read GetPixelColor write SetPixelColor;
    property GammaExpandedPixels[X,Y: Integer]: TExpandedPixel read GetExpandedPixel write SetExpandedPixel;
    property Colors[X,Y: Integer]: TFPColor read GetFPPixelColor write SetFPPixelColor;
    property Height: integer read GetHeight;
    property Width : integer read GetWidth;
    property ClipRect: TRect read GetClipRect write SetClipRect;
    property Clipping: Boolean read GetClipping write SetClipping;
  end;

Implementation
End.

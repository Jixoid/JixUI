// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAVectorize;

{$mode objfpc}{$H+}
{$WARN 5093 off : function result variable of a managed type does not seem to be initialized}
{$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
interface

{
  Font rendering units : BGRAText, BGRATextFX, BGRAVectorize, BGRAFreeType

  This unit provides vectorizers :
  - VectorizeMonochrome function vectorizes a back'n'white image
  - TBGRAVectorizedFont allows to vectorize and to load vectorized font and draw them

  TBGRAVectorizedFontRenderer class works like other font renderers, i.e., it can
  be assigned to the FontRenderer property. You can use it in two different modes :
  - if you supply a directory, it will look for *.glyphs files in it to load fonts
  - if you don't supply a directory, fonts will be vectorized from LCL

  Note that unless you want to supply your own glyphs files, you don't need
  to use explicitely this renderer, because TBGRATextEffectFontRenderer will
  make use of it if necessary, according to effects parameters used.
}

uses
  BGRAClasses, SysUtils, BGRAGraphics, BGRABitmapTypes, BGRATypewriter,
  BGRATransform, BGRACanvas2D, BGRAText;

//vectorize a monochrome bitmap
function VectorizeMonochrome(ASource: TBGRACustomBitmap; AZoom: single; APixelCenteredCoordinates: boolean;
  AWhiteBackground: boolean = true; ADiagonalFillPercent: single = 66; AIntermediateDiagonals: boolean = true): ArrayOfTPointF;
function VectorizeMonochrome(ASource: TBGRACustomBitmap; ARect: TRect; AZoom: single; APixelCenteredCoordinates: boolean;
  AWhiteBackground: boolean = true; ADiagonalFillPercent: single = 66; AIntermediateDiagonals: boolean = true): ArrayOfTPointF;

type
  TBGRAVectorizedFont = class;

  //this is the class to assign to FontRenderer property of TBGRABitmap
  { TBGRAVectorizedFontRenderer }

  TBGRAVectorizedFontRenderer = class(TBGRACustomFontRenderer)
  protected
    FVectorizedFontArray: array of record
        FontName: string;
        FontStyle: TFontStyles;
        VectorizedFont: TBGRAVectorizedFont;
      end;
    FVectorizedFont: TBGRAVectorizedFont;
    FCanvas2D: TBGRACanvas2D;
    FDirectoryUTF8: string;
    function OutlineActuallyVisible: boolean;
    procedure UpdateFont;
    function GetCanvas2D(ASurface: TBGRACustomBitmap): TBGRACanvas2D;
    procedure InternalTextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; c: TBGRAPixel; texture: IBGRAScanner);
    procedure InternalCopyTextPathTo(ADest: IBGRAPath; x, y: single; s: string; align: TAlignment; ABidiMode: TFontBidiMode);
    procedure InternalTextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientation: integer; s: string; c: TBGRAPixel; texture: IBGRAScanner; align: TAlignment; ABidiMode: TFontBidiMode);
    procedure Init;
  public
    MinFontResolution, MaxFontResolution: integer;
    QuadraticCurves: boolean;

    OutlineVisible: boolean;
    OutlineWidth: single;
    OutlineColor: TBGRAPixel;
    OutlineTexture: IBGRAScanner;
    OuterOutlineOnly: boolean;
    OutlineJoin: TPenJoinStyle;

    ShadowVisible: boolean;
    ShadowColor: TBGRAPixel;
    ShadowRadius: integer;
    ShadowOffset: TPoint;

    constructor Create; overload;
    constructor Create(ADirectoryUTF8: string); overload;
    function GetFontPixelMetric: TFontPixelMetric; override;
    function GetFontPixelMetricF: TFontPixelMetricF; override;
    function FontExists(AName: string): boolean; override;
    function TextVisible(const AColor: TBGRAPixel): boolean; override;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientation: integer; s: string; c: TBGRAPixel; align: TAlignment); overload; override;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientation: integer; s: string; c: TBGRAPixel; align: TAlignment; ARightToLeft: boolean); overload; override;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientation: integer; s: string; texture: IBGRAScanner; align: TAlignment); overload; override;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientation: integer; s: string; texture: IBGRAScanner; align: TAlignment; ARightToLeft: boolean); overload; override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; s: string; texture: IBGRAScanner; align: TAlignment); overload; override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; s: string; texture: IBGRAScanner; align: TAlignment; ARightToLeft: boolean); overload; override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; s: string; c: TBGRAPixel; align: TAlignment); overload; override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; s: string; c: TBGRAPixel; align: TAlignment; ARightToLeft: boolean); overload; override;
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; s: string; style: TTextStyle; c: TBGRAPixel); overload; override;
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; s: string; style: TTextStyle; texture: IBGRAScanner); overload; override;
    procedure CopyTextPathTo(ADest: IBGRAPath; x, y: single; s: string; align: TAlignment); override;
    procedure CopyTextPathTo(ADest: IBGRAPath; x, y: single; s: string; align: TAlignment; ARightToLeft: boolean); override;
    function HandlesTextPath: boolean; override;
    function TextSize(sUTF8: string): TSize; overload; override;
    function TextSizeF(sUTF8: string): TPointF; overload; override;
    function TextSize(sUTF8: string; AMaxWidth: integer; {%H-}ARightToLeft: boolean): TSize; overload; override;
    function TextSizeF(sUTF8: string; AMaxWidthF: single; {%H-}ARightToLeft: boolean): TPointF; overload; override;
    function TextFitInfo(sUTF8: string; AMaxWidth: integer): integer; override;
    function TextFitInfoF(sUTF8: string; AMaxWidthF: single): integer; override;
    destructor Destroy; override;
  end;

  TGlyphSizes = array of record
    Text, Glyph: String;
    Width,Height: single;
  end;
  TGlyphSizesCallbackData = record
    Sizes: TGlyphSizes;
    Count: integer;
  end;

  TBGRAVectorizedFontHeader = record
    Name: string;
    Style: TFontStyles;
    EmHeightRatio: single;
    Resolution: integer;
    PixelMetric: TFontPixelMetric;
  end;
  TBGRAGlyphsInfo = record
    Name: string;
    Style: TFontStyles;
    NbGlyphs: integer;
  end;

  { TBGRAVectorizedFont }

  TBGRAVectorizedFont = class(TBGRACustomTypeWriter)
  private
    FName : string;
    FStyle: TFontStyles;
    FResolution: integer;
    FFont: TFont;
    FBuffer: TBGRACustomBitmap;
    FFullHeight: single;
    FFontMatrix: TAffineMatrix;
    FOrientation: single;
    FQuadraticCurves: boolean;
    FItalicSlope: single;
    FWordBreakHandler: TWordBreakHandler;
    FDirectory: string;
    FDirectoryContent: array of record
      Filename: string;
      FontName: string;
      FontStyle: TFontStyles;
    end;
    FFontEmHeightRatioComputed: boolean;
    FFontEmHeightRatio: single;
    FFontPixelMetric: TFontPixelMetric;
    FFontPixelMetricComputed: boolean;
    FFontFound: boolean;
    function GetEmHeight: single;
    function GetFontPixelMetric: TFontPixelMetric;
    function GetLCLHeight: single;
    function GetVectorizeLCL: boolean;
    procedure SetEmHeight(AValue: single);
    procedure SetItalicSlope(AValue: single);
    procedure SetLCLHeight(AValue: single);
    procedure SetOrientation(AValue: single);
    procedure SetQuadraticCurves(AValue: boolean);
    procedure SetResolution(AValue: integer);
    procedure SetFontMatrix(AValue: TAffineMatrix);
    procedure SetFullHeight(AValue: single);
    procedure SetName(AValue: string);
    procedure SetStyle(AValue: TFontStyles);
    function GetFontEmHeightRatio: single;
    procedure SetVectorizeLCL(AValue: boolean);
    procedure GlyphCallbackForGlyphSizes(ATextUTF8: string; AGlyph: TBGRAGlyph;
      {%H-}AFlags: TBrowseGlyphCallbackFlags; AData: Pointer; out AContinue: boolean);
    procedure UpdateQuadraticCallback({%H-}ATextUTF8: string; AGlyph: TBGRAGlyph;
      {%H-}AFlags: TBrowseGlyphCallbackFlags; {%H-}AData: Pointer; out AContinue: boolean);
  protected
    procedure UpdateFont;
    procedure UpdateMatrix;
    function GetGlyph(AIdentifier: string): TBGRAGlyph; override;
    procedure DefaultWordBreakHandler(var ABefore, AAfter: string);
    procedure Init(AVectorize: boolean);
    function CustomHeaderSize: integer; override;
    procedure WriteCustomHeader(AStream: TStream); override;
    procedure ReadAdditionalHeader(AStream: TStream); override;
    function ReadVectorizedFontHeader(AStream: TStream): TBGRAVectorizedFontHeader;
    function HeaderName: string; override;
    procedure SetDirectory(const AValue: string);
    function ComputeKerning(AIdLeft, AIdRight: string): single; override;
  public
    UnderlineDecoration,StrikeOutDecoration: boolean;
    constructor Create; overload;
    constructor Create(AVectorizeLCL: boolean); overload;
    destructor Destroy; override;
    function GetGlyphSize(AIdentifier:string): TPointF;
    function GetTextGlyphSizes(ATextUTF8:string): TGlyphSizes;
    function GetTextSize(ATextUTF8:string): TPointF;
    function TextFitInfo(ATextUTF8: string; AMaxWidth: single): integer;
    procedure SplitText(var ATextUTF8: string; AMaxWidth: single; out ARemainsUTF8: string);
    procedure DrawText(ADest: TBGRACanvas2D; ATextUTF8: string; X, Y: Single; AAlign: TBGRATypeWriterAlignment=twaTopLeft); override;
    procedure CopyTextPathTo(ADest: IBGRAPath; ATextUTF8: string; X, Y: Single;
      AAlign: TBGRATypeWriterAlignment=twaTopLeft); override;
    procedure DrawTextWordBreak(ADest: TBGRACanvas2D; ATextUTF8: string; X, Y, MaxWidth: Single; AAlign: TBGRATypeWriterAlignment=twaTopLeft);
    procedure DrawTextRect(ADest: TBGRACanvas2D; ATextUTF8: string; X1,Y1,X2,Y2: Single; AAlign: TBGRATypeWriterAlignment=twaTopLeft); overload;
    procedure DrawTextRect(ADest: TBGRACanvas2D; ATextUTF8: string; ATopLeft,ABottomRight: TPointF; AAlign: TBGRATypeWriterAlignment=twaTopLeft); overload;
    function GetTextWordBreakGlyphBoxes(ATextUTF8: string; X,Y, MaxWidth: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft): TGlyphBoxes;
    function GetTextRectGlyphBoxes(ATextUTF8: string; X1,Y1,X2,Y2: Single; AAlign: TBGRATypeWriterAlignment=twaTopLeft): TGlyphBoxes; overload;
    function GetTextRectGlyphBoxes(ATextUTF8: string; ATopLeft,ABottomRight: TPointF; AAlign: TBGRATypeWriterAlignment=twaTopLeft): TGlyphBoxes; overload;
    procedure UpdateDirectory;
    function LoadGlyphsInfo(AFilenameUTF8: string): TBGRAGlyphsInfo;

    property Resolution: integer read FResolution write SetResolution;
    property Style: TFontStyles read FStyle write SetStyle;
    property Name: string read FName write SetName;
    property LCLHeight: single read GetLCLHeight write SetLCLHeight;
    property EmHeight: single read GetEmHeight write SetEmHeight;
    property FullHeight: single read FFullHeight write SetFullHeight;
    property FontMatrix: TAffineMatrix read FFontMatrix write SetFontMatrix;
    property Orientation: single read FOrientation write SetOrientation;
    property QuadraticCurves: boolean read FQuadraticCurves write SetQuadraticCurves;
    property ItalicSlope: single read FItalicSlope write SetItalicSlope;
    property OnWordBreak: TWordBreakHandler read FWordBreakHandler write FWordBreakHandler;
    property Directory: string read FDirectory write SetDirectory;
    property FontEmHeightRatio: single read GetFontEmHeightRatio;
    property FontPixelMetric: TFontPixelMetric read GetFontPixelMetric;
    property FontFound: boolean read FFontFound;
    property VectorizeLCL: boolean read GetVectorizeLCL write SetVectorizeLCL;
  end;

Implementation
End.

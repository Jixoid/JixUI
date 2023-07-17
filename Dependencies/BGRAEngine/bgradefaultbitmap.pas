// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
 /**************************************************************************\
                             bgradefaultbitmap.pas
                             ---------------------
                 This unit defines basic operations on bitmaps.
                 It should NOT be added to the 'uses' clause.
                 Some operations may be slow, so there are
                 accelerated versions for some routines.
}

unit BGRADefaultBitmap;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
{$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
{$WARN 5025 off : Local variable "$1" not used}
{$WARN 6018 off : unreachable code}
{$WARN 3057 off : An inherited method is hidden by "$1"}
{$i bgrabitmap.map.inc}

interface

{ This unit contains TBGRADefaultBitmap class. This class contains basic drawing routines,
  and call functions from other units to perform advanced drawing functions. }

uses
  SysUtils, BGRAClasses, FPImage, BGRAGraphics, BGRABitmapTypes,
  {$IFDEF BGRABITMAP_USE_FPCANVAS}FPImgCanv,{$ENDIF}
  BGRACanvas, BGRACanvas2D, BGRATransform, BGRATextBidi,
  UniversalDrawer, BGRAGrayscaleMask;

type
  TBGRAPtrBitmap = class;
  {=== TBGRABitmap reference ===}
  { TBGRADefaultBitmap }
  {* This class is the base for all ''TBGRABitmap'' classes. It implements most
     function to the exception from implementations specific to the
     widgetset }{ in the doc, it is presented as
  TBGRABitmap = class(TBGRACustomBitmap)
  }
  TBGRADefaultBitmap = class(TBGRACustomBitmap)
  private
    { Bounds checking which are shared by drawing functions. These functions check
      if the coordinates are visible and return true if it is the case, swap
      coordinates if necessary and make them fit into the clipping rectangle }
    function CheckRectBounds(var x,y,x2,y2: integer; minsize: integer): boolean; inline;
    function CheckAntialiasRectBounds(var x,y,x2,y2: single; w: single): boolean;
    function GetCanvasBGRA: TBGRACanvas;
    function GetCanvas2D: TBGRACanvas2D;
    procedure GradientFillDithered(x, y, x2, y2: integer; c1, c2: TBGRAPixel;
      gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
      gammaColorCorrection: boolean = True; Sinus: Boolean=False;
      ditherAlgo: TDitheringAlgorithm = daFloydSteinberg); overload;
    procedure GradientFillDithered(x, y, x2, y2: integer; gradient: TBGRACustomGradient;
      gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
      Sinus: Boolean=False;
      ditherAlgo: TDitheringAlgorithm = daFloydSteinberg); overload;

  protected
    //Pixel data
    FDataModified: boolean;              //if data image has changed so TBitmap should be updated

    //GUI bitmap object
    FBitmap:   TBitmap;
    FBitmapModified: boolean;         //if TBitmap has changed so pixel data should be updated
    FCanvasOpacity: byte;             //opacity used with standard canvas functions
    FAlphaCorrectionNeeded: boolean;  //the alpha channel is not correct because standard functions do not
                                      //take it into account

    //FreePascal drawing routines
    {$IFDEF BGRABITMAP_USE_FPCANVAS}FCanvasFP: TFPImageCanvas;{$ENDIF}
    FCanvasDrawModeFP: TDrawMode;
    FCanvasPixelProcFP: procedure(x, y: int32or64; const col: TBGRAPixel) of object;

    //canvas-like with antialiasing and texturing
    FCanvasBGRA: TBGRACanvas;
    FCanvas2D: TBGRACanvas2D;

    //drawing options
    FFontHeight: integer;
    FFontRenderer: TBGRACustomFontRenderer;

    //Pixel data
    function LoadFromRawImage(ARawImage: TRawImage; DefaultOpacity: byte;
      AlwaysReplaceAlpha: boolean = False; RaiseErrorOnInvalidPixelFormat: boolean = True): boolean; virtual; abstract;

    //FreePascal drawing routines
    {$IFDEF BGRABITMAP_USE_FPCANVAS}function GetCanvasFP: TFPImageCanvas; override;{$ENDIF}
    procedure SetCanvasDrawModeFP(const AValue: TDrawMode); override;
    function GetCanvasDrawModeFP: TDrawMode; override;

    //GUI bitmap object
    function GetBitmap: TBitmap; override;
    function GetCanvas: TCanvas; override;
    function GetCanvasOpacity: byte; override;
    procedure SetCanvasOpacity(AValue: byte); override;
    function GetCanvasAlphaCorrection: boolean; override;
    procedure SetCanvasAlphaCorrection(const AValue: boolean); override;
    procedure DoAlphaCorrection;
    procedure DiscardBitmapChange; inline;
    procedure DoLoadFromBitmap; virtual;

    function CreatePtrBitmap(AWidth,AHeight: integer; AData: PBGRAPixel): TBGRAPtrBitmap; virtual;

    procedure RebuildBitmap; virtual; abstract;
    procedure FreeBitmap; virtual;

    procedure Init; override;

    {TFPCustomImage}
    procedure SetInternalColor(x, y: integer; const Value: TFPColor); override;
    function GetInternalColor(x, y: integer): TFPColor; override;
    procedure SetInternalPixel(x, y: integer; Value: integer); override;
    function GetInternalPixel(x, y: integer): integer; override;

    {Image functions}
    function FineResample(NewWidth, NewHeight: integer): TBGRACustomBitmap;
    function SimpleStretch(NewWidth, NewHeight: integer): TBGRACustomBitmap;
    function CheckEmpty: boolean; override;
    function GetHasTransparentPixels: boolean; override;
    function GetHasSemiTransparentPixels: boolean; override;
    function GetAverageColor: TColor; override;
    function GetAveragePixel: TBGRAPixel; override;

  protected //pen style accesors
    function GetPenJoinStyle: TPenJoinStyle; override;
    procedure SetPenJoinStyle(const AValue: TPenJoinStyle); override;
    function GetPenMiterLimit: single; override;
    procedure SetPenMiterLimit(const AValue: single); override;
    function GetCustomPenStyle: TBGRAPenStyle; override;
    procedure SetCustomPenStyle(const AValue: TBGRAPenStyle); override;
    procedure SetPenStyle(const AValue: TPenStyle); override;
    function GetPenStyle: TPenStyle; override;

    function GetArrowEndSize: TPointF; override;
    function GetArrowStartSize: TPointF; override;
    procedure SetArrowEndSize(AValue: TPointF); override;
    procedure SetArrowStartSize(AValue: TPointF); override;
    function GetArrowEndOffset: single; override;
    function GetArrowStartOffset: single; override;
    procedure SetArrowEndOffset(AValue: single); override;
    procedure SetArrowStartOffset(AValue: single); override;
    function GetArrowEndRepeat: integer; override;
    function GetArrowStartRepeat: integer; override;
    procedure SetArrowEndRepeat(AValue: integer); override;
    procedure SetArrowStartRepeat(AValue: integer); override;

  protected //font accessors
    function GetFontHeight: integer; override;
    procedure SetFontHeight(AHeight: integer); override;
    function GetFontFullHeight: integer; override;
    procedure SetFontFullHeight(AHeight: integer); override;
    function GetFontPixelMetric: TFontPixelMetric; override;
    function CreateDefaultFontRenderer: TBGRACustomFontRenderer; virtual; abstract;
    function GetFontVerticalAnchorOffset: single; override;
    function GetFontAnchorRotatedOffset: TPointF; overload;
    function GetFontAnchorRotatedOffset(ACustomOrientation: integer): TPointF; overload;
    function GetFontRenderer: TBGRACustomFontRenderer; override;
    procedure SetFontRenderer(AValue: TBGRACustomFontRenderer); override;

    function InternalGetPixelCycle256(ix,iy: int32or64; iFactX,iFactY: int32or64): TBGRAPixel;
    function InternalGetPixel256(ix,iy: int32or64; iFactX,iFactY: int32or64; smoothBorder: boolean): TBGRAPixel;
    procedure InternalTextOutCurved(ACursor: TBGRACustomPathCursor; sUTF8: string; AColor: TBGRAPixel; ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single);
    procedure InternalTextOutLetterSpacing(x,y: single; sUTF8: string; AColor: TBGRAPixel; ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single);
    procedure InternalCrossFade(ARect: TRect; Source1, Source2: IBGRAScanner; AFadePos: byte; AFadeMask: IBGRAScanner; mode: TDrawMode = dmDrawWithTransparency);

    procedure InternalArc(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; ABorderColor: TBGRAPixel; w: single;
      AFillColor: TBGRAPixel; AOptions: TArcOptions; ADrawChord: boolean = false; ATexture: IBGRAScanner = nil); override;
    function InternalNew: TBGRADefaultBitmap; override;

  public
    {** Provides a canvas with opacity and antialiasing }
    property CanvasBGRA: TBGRACanvas read GetCanvasBGRA;
    {** Provides a canvas with 2d transformation and similar to HTML5. }
    property Canvas2D: TBGRACanvas2D read GetCanvas2D;
    {** For more properties, see parent class [[TBGRACustomBitmap and IBGRAScanner#TBGRACustomBitmap|TBGRACustomBitmap]] }

    procedure SetSize(AWidth, AHeight: integer); override;

    {==== Constructors ====}

    {------------------------- Constructors from TBGRACustomBitmap-------------}

    {** Creates an image by copying the content of a ''TFPCustomImage'' }
    constructor Create(AFPImage: TFPCustomImage); overload; override;
    {** Creates an image by copying the content of a ''TBitmap'', apply transparent color if specified and bitmap is masked }
    constructor Create(ABitmap: TBitmap); overload; override;
    {** Creates an image by copying the content of a ''TBitmap'', enforce/disable use of transparent color }
    constructor Create(ABitmap: TBitmap; AUseTransparentColor: boolean); overload; override;

    {** Creates an image by loading its content from the file ''AFilename''.
        The encoding of the string is the default one for the operating system.
        It is recommended to use the next constructor and UTF8 encoding }
    constructor Create(AFilename: string); overload; override;

    {** Creates an image by loading its content from the file ''AFilename''.
        The boolean ''AIsUtf8Filename'' specifies if UTF8 encoding is assumed
        for the filename }
    constructor Create(AFilename: string; AIsUtf8: boolean); overload; override;
    constructor Create(AFilename: string; AIsUtf8: boolean; AOptions: TBGRALoadingOptions); overload; override;

    {** Creates an image by loading its content from the stream ''AStream'' }
    constructor Create(AStream: TStream); overload; override;
    {** Free the object and all its resources }
    destructor Destroy; override;

    {** Clear all channels of transparent pixels }
    procedure ClearTransparentPixels; override;

    {------------------------- Quasi-constructors -----------------------------}

    {** Can only be called from an existing instance of ''TBGRABitmap''.
        Creates a new instance with dimensions 0 x 0. }
    function NewBitmap: TBGRADefaultBitmap; overload; override;

    {** Can only be called from an existing instance of ''TBGRABitmap''.
        Creates a new instance with dimensions ''AWidth'' and ''AHeight'',
        containing transparent pixels. }
    function NewBitmap(AWidth, AHeight: integer): TBGRADefaultBitmap; overload; override;

    {* Example:
       <syntaxhighlight>
     * var bmp1, bmp2: TBGRABitmap;
     * begin
     *   bmp1 := TBGRABitmap.Create(100,100);
     *   bmp2 := bmp1.NewBitmap(100,100);
     *   ...
     * end;</syntaxhighlight>
       See tutorial 2 on [[BGRABitmap_tutorial_2|how to load and display an image]].
     * See reference on [[TBGRACustomBitmap_and_IBGRAScanner#Load_and_save_files|loading and saving files]] }

    {** Can only be called from an existing instance of ''TBGRABitmap''.
        Creates a new instance with dimensions ''AWidth'' and ''AHeight'',
        and fills it with Color }
    function NewBitmap(AWidth, AHeight: integer; const Color: TBGRAPixel): TBGRADefaultBitmap; overload; override;
    function NewBitmap(AWidth, AHeight: integer; AColor: Pointer): TBGRADefaultBitmap; overload; override;

    {** Can only be called from an existing instance of ''TBGRABitmap''.
        Creates a new instance with by loading its content
        from the file ''Filename''. The encoding of the string
        is the default one for the operating system }
    function NewBitmap(Filename: string): TBGRADefaultBitmap; overload; override;

    {** Can only be called from an existing instance of ''TBGRABitmap''.
        Creates a new instance with by loading its content
        from the file ''Filename'' }
    function NewBitmap(Filename: string; AIsUtf8: boolean): TBGRADefaultBitmap; overload; override;
    function NewBitmap(Filename: string; AIsUtf8: boolean; AOptions: TBGRALoadingOptions): TBGRADefaultBitmap; overload; override;

    {** Can only be called from an existing instance of ''TBGRABitmap''.
        Creates an image by copying the content of a ''TFPCustomImage'' }
    function NewBitmap(AFPImage: TFPCustomImage): TBGRADefaultBitmap; overload; override;

    {** Assign the content of the specified ''Source''. It can be a ''TBGRACustomBitmap'' or
        a ''TFPCustomImage'' }
    procedure Assign(Source: TPersistent); overload; override;
    procedure AssignWithFixedTransparent(Source: TBitmap); overload;
    procedure Assign(Source: TBitmap; AUseTransparentColor: boolean); overload;

    {** Stores the image in the stream without compression nor header }
    procedure Serialize(AStream: TStream); override;
    {** Reads the image in a stream that was previously serialized }
    procedure Deserialize(AStream: TStream); override;

    // universal brushes
    procedure SolidBrushIndirect(out ABrush: TUniversalBrush; AColor: Pointer; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
    class procedure SolidBrush(out ABrush: TUniversalBrush; const AColor: TBGRAPixel; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
    class procedure ScannerBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner; ADrawMode: TDrawMode = dmDrawWithTransparency;
                                 AOffsetX: integer = 0; AOffsetY: integer = 0); override;
    class procedure MaskBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner;
                              AOffsetX: integer = 0; AOffsetY: integer = 0); override;
    class procedure EraseBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;
    class procedure AlphaBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;

    {==== Pixel functions ====}
    {** Sets the pixel by replacing the content at (''x'',''y'') with the specified color.
        Alpha value is set to 255 (opaque) }
    procedure SetPixel(x, y: int32or64; c: TColor); overload; override;
    {** Applies a logical '''xor''' to the content of the pixel with the specified value.
        This includes the alpha channel, so if you want to preserve the opacity, provide
        a color ''c'' with alpha channel equal to zero }
    procedure XorPixel(x, y: int32or64; const c: TBGRAPixel); override;
    {** Draws a pixel with gamma correction at (''x'',''y''). Pixel is supplied
        in sRGB colorspace }
    procedure DrawPixel(x, y: int32or64; const c: TBGRAPixel); overload; override;
    {** Draws a pixel without gamma correction at (''x'',''y''). Pixel is supplied
        in sRGB colorspace }
    procedure FastBlendPixel(x, y: int32or64; const c: TBGRAPixel); override;
    {** Erase the content of the pixel by reducing the value of the
        alpha channel. ''alpha'' specifies how much to decrease.
        If the resulting alpha reaches zero, the content
        is replaced by ''BGRAPixelTransparent'' }
    procedure ErasePixel(x, y: int32or64; alpha: byte); override;
    {** Sets the alpha value at (''x'',''y''). If ''alpha'' = 0, the
        pixel is replaced by ''BGRAPixelTransparent'' }
    procedure AlphaPixel(x, y: int32or64; alpha: byte); override;
    {** Computes the value of the pixel at a floating point coordiante
        by interpolating the values of the pixels around it.
      * There is a one pixel wide margin around the pixel where the pixels are
        still considered inside. If ''smoothBorder'' is set to true, pixel fade
        to transparent.
      * If it is more out of the bounds, the result is ''BGRAPixelTransparent''.
      * ''AResampleFilter'' specifies how pixels must be interpolated. Accepted
        values are ''rfBox'', ''rfLinear'', ''rfHalfCosine'' and ''rfCosine'' }
    function GetPixel(x, y: single; AResampleFilter: TResampleFilter = rfLinear; smoothBorder: boolean = true): TBGRAPixel; overload; override;
    {** Similar to previous ''GetPixel'' function, but the fractional part of
        the coordinate is supplied with a number from 0 to 255. The actual
        coordinate is (''x'' + ''fracX256''/256, ''y'' + ''fracY256''/256) }
    function GetPixel256(x, y, fracX256,fracY256: int32or64; AResampleFilter: TResampleFilter = rfLinear; smoothBorder: boolean = true): TBGRAPixel; override;
    {** Computes the value of the pixel at a floating point coordiante
        by interpolating the values of the pixels around it. If the pixel
        is out of bounds, the image is repeated.
      * ''AResampleFilter'' specifies how pixels must be interpolated. Accepted
        values are ''rfBox'', ''rfLinear'', ''rfHalfCosine'' and ''rfCosine'' }
    function GetPixelCycle(x, y: single; AResampleFilter: TResampleFilter = rfLinear): TBGRAPixel; overload; override;
    {** Similar to previous ''GetPixel'' function, but the fractional part of
        the coordinate is supplied with a number from 0 to 255. The actual
        coordinate is (''x'' + ''fracX256''/256, ''y'' + ''fracY256''/256) }
    function GetPixelCycle256(x, y, fracX256,fracY256: int32or64; AResampleFilter: TResampleFilter = rfLinear): TBGRAPixel; overload; override;
    {** Computes the value of the pixel at a floating point coordiante
        by interpolating the values of the pixels around it. ''repeatX'' and
        ''repeatY'' specifies if the image is to be repeated or not.
      * ''AResampleFilter'' specifies how pixels must be interpolated. Accepted
        values are ''rfBox'', ''rfLinear'', ''rfHalfCosine'' and ''rfCosine'' }
    function GetPixelCycle(x, y: single; AResampleFilter: TResampleFilter; repeatX: boolean; repeatY: boolean): TBGRAPixel; overload; override;
    {** Similar to previous ''GetPixel'' function, but the fractional part of
        the coordinate is supplied with a number from 0 to 255. The actual
        coordinate is (''x'' + ''fracX256''/256, ''y'' + ''fracY256''/256) }
    function GetPixelCycle256(x, y, fracX256,fracY256: int32or64; AResampleFilter: TResampleFilter; repeatX: boolean; repeatY: boolean): TBGRAPixel; overload; override;

    {==== Drawing lines and polylines (integer coordinates) ====}
    {* These functions do not take into account current pen style/cap/join.
       See [[BGRABitmap tutorial 13|coordinate system]]. }

    {** Applies xor to the pixels at line ''y'' and
        at columns ''x'' to ''x2'' included, using specified color.
        This includes the alpha channel, so if you want to preserve the
        opacity, provide a color ''c'' with alpha channel equal to zero }
    procedure XorHorizLine(x, y, x2: int32or64; c: TBGRAPixel); override;
    {** Draws an horizontal line with gamma correction at line ''y'' and
        at columns ''x'' to ''x2'' included, using specified color }
    procedure DrawHorizLine(x, y, x2: int32or64; ec: TExpandedPixel); override; overload;
    {** Draws an horizontal line without gamma correction at line ''y'' and
        at columns ''x'' to ''x2'' included, using specified color }
    procedure FastBlendHorizLine(x, y, x2: int32or64; c: TBGRAPixel); override;
    {** Replaces the alpha value of the pixels at line ''y'' and
        at columns ''x'' to ''x2'' included }
    procedure AlphaHorizLine(x, y, x2: int32or64; alpha: byte); override;
    {** Draws an horizontal line with gamma correction at line ''y'' and
        at columns ''x'' to ''x2'' included, using specified color,
        and with a transparency that increases with the color difference
        with ''compare''. If the difference is greater than ''maxDiff'',
        pixels are not changed }
    procedure DrawHorizLineDiff(x, y, x2: int32or64; c, compare: TBGRAPixel;
      maxDiff: byte); override;
    procedure HorizLineDiff(x, y, x2: int32or64; const ABrush: TUniversalBrush;
      ACompare: TBGRAPixel; AMaxDiffW: word); override;

    {** Xors a vertical line at column ''x'' and at row ''y'' to ''y2'' }
    procedure XorVertLine(x, y, y2: int32or64; c: TBGRAPixel); override;
    {** Draws a vertical line with gamma correction at column ''x'' and at row ''y'' to ''y2'' }
    procedure DrawVertLine(x, y, y2: int32or64; c: TBGRAPixel); override;
    {** Draws a vertical line without gamma correction at column ''x'' and at row ''y'' to ''y2'' }
    procedure FastBlendVertLine(x, y, y2: int32or64; c: TBGRAPixel); override;
    {** Replace alpha values in a vertical line at column ''x'' and at row ''y'' to ''y2'' }
    procedure AlphaVertLine(x, y, y2: int32or64; alpha: byte); override;

    {** Fills completely a rectangle, without any border, with the specified ''texture'' and
    with the specified ''mode'' }
    procedure FillRect(x, y, x2, y2: integer; texture: IBGRAScanner; mode: TDrawMode; AScanOffset: TPoint; ditheringAlgorithm: TDitheringAlgorithm); overload; override;

    {==== Rectangles, ellipses and path (floating point coordinates) ====}
    {* These functions use the current pen style/cap/join. The parameter ''w''
       specifies the width of the line and the base unit for dashes
     * The coordinates are pixel-centered, so that when filling a rectangle,
       if the supplied values are integers, the border will be half transparent.
       If you want the border to be completely filled, you can subtract/add
       0.5 to the coordinates to include the remaining thin border.
       See [[BGRABitmap tutorial 13|coordinate system]]. }

    {==== Multi-shape fill ====}

    {** Draws and fill a polyline using current pen style/cap/join in one go.
        The stroke is stricly over the fill even if partially transparent.
        ''fillcolor'' specifies a color to fill the polygon formed by the points }
    procedure DrawPolyLineAntialias(const points: array of TPointF; c: TBGRAPixel; w: single; fillcolor: TBGRAPixel); overload; override;
    {** Draws a filled polygon using current pen style/cap/join in one go.
        The stroke is stricly over the fill even if partially transparent.
        The polygon is always closed. You don't need to set the last point
        to be the same as the first point. }
    procedure DrawPolygonAntialias(const points: array of TPointF; c: TBGRAPixel; w: single; fillcolor: TBGRAPixel); overload; override;

    {** Draws and fills an ellipse }
    procedure EllipseAntialias(x, y, rx, ry: single; c: TBGRAPixel; w: single; back: TBGRAPixel); overload; override;
    procedure EllipseAntialias(AOrigin, AXAxis, AYAxis: TPointF; c: TBGRAPixel; w: single; back: TBGRAPixel); overload; override;

    {** Draws and fills a path }
    procedure DrawPath(APath: IBGRAPath; AStrokeColor: TBGRAPixel; AWidth: single; AFillColor: TBGRAPixel); overload; override;
    procedure DrawPath(APath: IBGRAPath; AStrokeTexture: IBGRAScanner; AWidth: single; AFillColor: TBGRAPixel); overload; override;
    procedure DrawPath(APath: IBGRAPath; AStrokeColor: TBGRAPixel; AWidth: single; AFillTexture: IBGRAScanner); overload; override;
    procedure DrawPath(APath: IBGRAPath; AStrokeTexture: IBGRAScanner; AWidth: single; AFillTexture: IBGRAScanner); overload; override;

    {** Draws and fills a path with a matrix transform }
    procedure DrawPath(APath: IBGRAPath; AMatrix: TAffineMatrix; AStrokeColor: TBGRAPixel; AWidth: single; AFillColor: TBGRAPixel); overload; override;
    procedure DrawPath(APath: IBGRAPath; AMatrix: TAffineMatrix; AStrokeTexture: IBGRAScanner; AWidth: single; AFillColor: TBGRAPixel); overload; override;
    procedure DrawPath(APath: IBGRAPath; AMatrix: TAffineMatrix; AStrokeColor: TBGRAPixel; AWidth: single; AFillTexture: IBGRAScanner); overload; override;
    procedure DrawPath(APath: IBGRAPath; AMatrix: TAffineMatrix; AStrokeTexture: IBGRAScanner; AWidth: single; AFillTexture: IBGRAScanner); overload; override;

    {** Draws a rectangle with antialiasing and fills it with color ''back''.
        Note that the pixel (x2,y2) is included contrary to integer coordinates }
    procedure RectangleAntialias(x, y, x2, y2: single; c: TBGRAPixel; w: single; back: TBGRAPixel); overload; override;

    {** Draws a rounded rectangle border with antialiasing. The corners have an
        elliptical radius of ''rx'' and ''ry''. ''options'' specifies how to
        draw the corners. See [[BGRABitmap Geometry types|geometry types]] }
    procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; c: TBGRAPixel; w: single; options: TRoundRectangleOptions = []); overload; override;
    {** Draws a rounded rectangle border with the specified texture.
        The corners have an elliptical radius of ''rx'' and ''ry''.
        ''options'' specifies how to draw the corners.
        See [[BGRABitmap Geometry types|geometry types]] }
    procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; texture: IBGRAScanner; w: single; options: TRoundRectangleOptions = []); overload; override;
    {** Draws and fills a round rectangle }
    procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; pencolor: TBGRAPixel; w: single; fillcolor: TBGRAPixel; options: TRoundRectangleOptions = []); overload; override;
    {** Draws and fills a round rectangle with textures }
    procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; penTexture: IBGRAScanner; w: single; fillTexture: IBGRAScanner; options: TRoundRectangleOptions = []); overload; override;

    {==== Gradient polygons ====}

    procedure FillTriangleLinearColor(pt1,pt2,pt3: TPointF; c1,c2,c3: TBGRAPixel); override;
    procedure FillTriangleLinearColorAntialias(pt1,pt2,pt3: TPointF; c1,c2,c3: TBGRAPixel); override;
    procedure FillTriangleLinearMapping(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF; TextureInterpolation: Boolean= True); override;
    procedure FillTriangleLinearMappingLightness(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF; light1,light2,light3: word; TextureInterpolation: Boolean= True); override;
    procedure FillTriangleLinearMappingAntialias(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF); override;

    procedure FillQuadLinearColor(pt1,pt2,pt3,pt4: TPointF; c1,c2,c3,c4: TBGRAPixel); override;
    procedure FillQuadLinearColorAntialias(pt1,pt2,pt3,pt4: TPointF; c1,c2,c3,c4: TBGRAPixel); override;
    procedure FillQuadLinearMapping(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; TextureInterpolation: Boolean= True; ACulling: TFaceCulling = fcNone; ACropToPolygon: boolean = true); override;
    procedure FillQuadLinearMappingLightness(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; light1,light2,light3,light4: word; TextureInterpolation: Boolean= True); override;
    procedure FillQuadLinearMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; ACulling: TFaceCulling = fcNone); override;
    procedure FillQuadPerspectiveMapping(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
    procedure FillQuadPerspectiveMapping(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; ACleanBorders: TRect; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
    procedure FillQuadPerspectiveMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF); override;
    procedure FillQuadPerspectiveMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; ACleanBorders: TRect); override;
    procedure FillQuadAffineMapping(Orig,HAxis,VAxis: TPointF; AImage: TBGRACustomBitmap; APixelCenteredCoordinates: boolean = true; ADrawMode: TDrawMode = dmDrawWithTransparency; AOpacity: byte = 255); override;
    procedure FillQuadAffineMappingAntialias(Orig,HAxis,VAxis: TPointF; AImage: TBGRACustomBitmap; APixelCenteredCoordinates: boolean = true; AOpacity: byte = 255); override;

    {** Fills an ellipse with a gradient of color. ''outercolor'' specifies
        the end color of the gradient on the border of the ellipse and
        ''innercolor'' the end color of the gradient at the center of the ellipse }
    procedure FillEllipseLinearColorAntialias(x, y, rx, ry: single; outercolor, innercolor: TBGRAPixel); overload; override;
    procedure FillEllipseLinearColorAntialias(AOrigin, AXAxis, AYAxis: TPointF; outercolor, innercolor: TBGRAPixel); overload; override;

    procedure FillPolyLinearMapping(const points: array of TPointF; texture: IBGRAScanner; texCoords: array of TPointF; TextureInterpolation: Boolean); override;
    procedure FillPolyLinearMappingLightness(const points: array of TPointF; texture: IBGRAScanner; texCoords: array of TPointF; lightnesses: array of word; TextureInterpolation: Boolean); override;
    procedure FillPolyLinearColor(const points: array of TPointF; AColors: array of TBGRAPixel); override;
    procedure FillPolyPerspectiveMapping(const points: array of TPointF; const pointsZ: array of single; texture: IBGRAScanner; texCoords: array of TPointF; TextureInterpolation: Boolean; zbuffer: psingle = nil); override;
    procedure FillPolyPerspectiveMappingLightness(const points: array of TPointF; const pointsZ: array of single; texture: IBGRAScanner; texCoords: array of TPointF; lightnesses: array of word; TextureInterpolation: Boolean; zbuffer: psingle = nil); override;

    procedure ArrowStartAsNone; override;
    procedure ArrowStartAsClassic(AFlipped: boolean = false; ACut: boolean = false; ARelativePenWidth: single = 1); override;
    procedure ArrowStartAsTriangle(ABackOffset: single = 0; ARounded: boolean = false; AHollow: boolean = false; AHollowPenWidth: single = 0.5); override;
    procedure ArrowStartAsTail; override;

    procedure ArrowEndAsNone; override;
    procedure ArrowEndAsClassic(AFlipped: boolean = false; ACut: boolean = false; ARelativePenWidth: single = 1); override;
    procedure ArrowEndAsTriangle(ABackOffset: single = 0; ARounded: boolean = false; AHollow: boolean = false; AHollowPenWidth: single = 0.5); override;
    procedure ArrowEndAsTail; override;

    { Draws the UTF8 encoded string, with color c.
      If align is taLeftJustify, (x,y) is the top-left corner.
      If align is taCenter, (x,y) is at the top and middle of the text.
      If align is taRightJustify, (x,y) is the top-right corner.
      The value of FontOrientation is taken into account, so that the text may be rotated. }
    procedure TextOut(x, y: single; const sUTF8: string; c: TBGRAPixel; align: TAlignment; ARightToLeft: boolean); overload; override;

    { Same as above functions, except that the text is filled using texture.
      The value of FontOrientation is taken into account, so that the text may be rotated. }
    procedure TextOut(x, y: single; const sUTF8: string; texture: IBGRAScanner; align: TAlignment; ARightToLeft: boolean); overload; override;

    procedure TextOut(x, y: single; const sUTF8: string; AColor: TBGRAPixel; AAlign: TAlignment; ALetterSpacing: single); overload; override;
    procedure TextOut(x, y: single; const sUTF8: string; ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single); overload; override;

    { Same as above, except that the orientation is specified, overriding the value of the property FontOrientation. }
    procedure TextOutAngle(x, y: single; orientationTenthDegCCW: integer; const sUTF8: string; c: TBGRAPixel; align: TAlignment; ARightToLeft: boolean); overload; override;
    procedure TextOutAngle(x, y: single; orientationTenthDegCCW: integer; const sUTF8: string; texture: IBGRAScanner; align: TAlignment; ARightToLeft: boolean); overload; override;

    procedure TextOutCurved(ACursor: TBGRACustomPathCursor; const sUTF8: string; AColor: TBGRAPixel; AAlign: TAlignment; ALetterSpacing: single); overload; override;
    procedure TextOutCurved(ACursor: TBGRACustomPathCursor; const sUTF8: string; ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single); overload; override;

    procedure TextMultiline(ALeft,ATop,AWidth: single; const sUTF8: string; c: TBGRAPixel; AAlign: TBidiTextAlignment = btaNatural; AVertAlign: TTextLayout = tlTop; AParagraphSpacing: single = 0); overload; override;
    procedure TextMultiline(ALeft,ATop,AWidth: single; const sUTF8: string; ATexture: IBGRAScanner; AAlign: TBidiTextAlignment = btaNatural; AVertAlign: TTextLayout = tlTop; AParagraphSpacing: single = 0); overload; override;

    { Draw the UTF8 encoded string at the coordinate (x,y), clipped inside the rectangle ARect.
      Additional style information is provided by the style parameter.
      The color c or texture is used to fill the text. No rotation is applied. }
    procedure TextRect(ARect: TRect; x, y: integer; const sUTF8: string; style: TTextStyle; c: TBGRAPixel); overload; override;
    procedure TextRect(ARect: TRect; x, y: integer; const sUTF8: string; style: TTextStyle; texture: IBGRAScanner); overload; override;

    { Returns the total size of the string provided using the current font.
      Orientation is not taken into account, so that the width is along the text. End of lines are stripped from the string. }
    function TextSize(const sUTF8: string): TSize; override;
    function TextSizeMultiline(const sUTF8: string; AMaxWidth: single = EmptySingle; AParagraphSpacing: single = 0): TSize; override;

    { Returns the affine box of the string provided using the current font.
      Orientation is taken into account. End of lines are stripped from the string. }
    function TextAffineBox(const sUTF8: string): TAffineBox; override;

    { Returns the total size of a paragraph i.e. with word break }
    function TextSize(const sUTF8: string; AMaxWidth: integer): TSize; override;
    function TextSize(const sUTF8: string; AMaxWidth: integer; ARightToLeft: boolean): TSize; override;
    function TextFitInfo(const sUTF8: string; AMaxWidth: integer): integer; override;

    {Spline}
    function ComputeClosedSpline(const APoints: array of TPointF; AStyle: TSplineStyle): ArrayOfTPointF; override;
    function ComputeOpenedSpline(const APoints: array of TPointF; AStyle: TSplineStyle): ArrayOfTPointF; override;

    function ComputeBezierCurve(const ACurve: TCubicBezierCurve): ArrayOfTPointF; overload; override;
    function ComputeBezierCurve(const ACurve: TQuadraticBezierCurve): ArrayOfTPointF; overload; override;
    function ComputeBezierSpline(const ASpline: array of TCubicBezierCurve): ArrayOfTPointF; overload; override;
    function ComputeBezierSpline(const ASpline: array of TQuadraticBezierCurve): ArrayOfTPointF; overload; override;

    function ComputeWidePolyline(const points: array of TPointF; w: single): ArrayOfTPointF; overload; override;
    function ComputeWidePolyline(const points: array of TPointF; w: single; ClosedCap: boolean): ArrayOfTPointF; overload; override;
    function ComputeWidePolygon(const points: array of TPointF; w: single): ArrayOfTPointF; overload; override;

    function ComputeEllipseContour(x,y,rx,ry: single; quality: single = 1): ArrayOfTPointF;  overload; override;
    function ComputeEllipseContour(AOrigin, AXAxis, AYAxis: TPointF; quality: single = 1): ArrayOfTPointF;  overload; override;
    function ComputeEllipseBorder(x,y,rx,ry,w: single; quality: single = 1): ArrayOfTPointF; overload; override;
    function ComputeEllipseBorder(AOrigin, AXAxis, AYAxis: TPointF; w: single; quality: single = 1): ArrayOfTPointF; override; overload;
    function ComputeArc65536(x,y,rx,ry: single; start65536,end65536: word; quality: single = 1): ArrayOfTPointF; override;
    function ComputeArcRad(x,y,rx,ry: single; startRad,endRad: single; quality: single = 1): ArrayOfTPointF; override;
    function ComputeRoundRect(x1,y1,x2,y2,rx,ry: single; quality: single = 1): ArrayOfTPointF; overload; override;
    function ComputeRoundRect(x1,y1,x2,y2,rx,ry: single; options: TRoundRectangleOptions; quality: single = 1): ArrayOfTPointF; overload; override;
    function ComputePie65536(x,y,rx,ry: single; start65536,end65536: word; quality: single = 1): ArrayOfTPointF; override;
    function ComputePieRad(x,y,rx,ry: single; startRad,endRad: single; quality: single = 1): ArrayOfTPointF; override;

    {Filling}
    procedure Fill(c: TBGRAPixel; start, Count: integer); overload; override;
    procedure DrawPixels(c: TBGRAPixel; start, Count: integer); override;
    procedure AlphaFill(alpha: byte; start, Count: integer); overload; override;
    procedure FillMask(x,y: integer; AMask: TCustomUniversalBitmap; const AColor: TBGRAPixel; ADrawMode: TDrawMode); overload; override;
    procedure FillMask(x,y: integer; AMask: TCustomUniversalBitmap; ATexture: IBGRAScanner; ADrawMode: TDrawMode; AOpacity: byte); overload; override;
    procedure EraseMask(x,y: integer; AMask: TBGRACustomBitmap; alpha: byte=255); override;
    procedure FillClearTypeMask(x,y: integer; xThird: integer; AMask: TBGRACustomBitmap; color: TBGRAPixel; ARGBOrder: boolean = true); override;
    procedure FillClearTypeMask(x,y: integer; xThird: integer; AMask: TBGRACustomBitmap; texture: IBGRAScanner; ARGBOrder: boolean = true); override;
    procedure ReplaceColor(before, after: TColor); overload; override;
    procedure ReplaceColor(ABounds: TRect; before, after: TColor); overload; override;
    procedure ParallelFloodFill(X, Y: integer; Dest: TCustomUniversalBitmap; Color: TBGRAPixel;
      mode: TFloodfillMode; Tolerance: byte = 0; DestOfsX: integer = 0; DestOfsY: integer = 0); overload; override;
    procedure ParallelFloodFill(X, Y: integer; Dest: TCustomUniversalBitmap; const Brush: TUniversalBrush;
      Progressive: boolean; ToleranceW: Word = $00ff; DestOfsX: integer = 0; DestOfsY: integer = 0); overload; override;
    procedure GradientFill(x, y, x2, y2: integer; c1, c2: TBGRAPixel;
      gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
      gammaColorCorrection: boolean = True; Sinus: Boolean=False;
      ditherAlgo: TDitheringAlgorithm = daNearestNeighbor); override;
    procedure GradientFill(x, y, x2, y2: integer; gradient: TBGRACustomGradient;
      gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
      Sinus: Boolean=False; ditherAlgo: TDitheringAlgorithm = daNearestNeighbor); override;

    function ScanAtInteger(X,Y: integer): TBGRAPixel; override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt(X,Y: Single): TBGRAPixel; override;
    function IsScanPutPixelsDefined: boolean; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;

    {Canvas drawing functions}
    procedure Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean = True); overload; override;
    procedure Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean = True); overload; override;
    procedure InvalidateBitmap; override;         //call if you modify with Scanline
    procedure LoadFromBitmapIfNeeded; override;   //call to ensure that bitmap data is up to date
    procedure NotifyBitmapChange; inline;

    {BGRA bitmap functions}
    procedure CrossFade(ARect: TRect; Source1, Source2: IBGRAScanner; AFadePosition: byte; mode: TDrawMode = dmDrawWithTransparency); overload; override;
    procedure CrossFade(ARect: TRect; Source1, Source2: IBGRAScanner; AFadeMask: IBGRAScanner; mode: TDrawMode = dmDrawWithTransparency); overload; override;
    procedure PutImage(X, Y: integer; ASource: TCustomUniversalBitmap; AMode: TDrawMode; AOpacity: byte); overload; override;
    procedure PutImageAffine(AMatrix: TAffineMatrix; Source: TBGRACustomBitmap; AOutputBounds: TRect; AResampleFilter: TResampleFilter; AMode: TDrawMode; AOpacity: Byte=255; APixelCenteredCoords: boolean = true); overload; override;
    function GetImageAffineBounds(AMatrix: TAffineMatrix; ASourceBounds: TRect; AClipOutput: boolean = true; APixelCenteredCoords: boolean = true): TRect; overload; override;
    class function IsAffineRoughlyTranslation(AMatrix: TAffineMatrix; ASourceBounds: TRect): boolean; override;

    procedure StretchPutImage(ARect: TRect; Source: TBGRACustomBitmap; mode: TDrawMode; AOpacity: byte = 255); override;
    procedure BlendRect(ADest: TRect; AColor: TBGRAPixel; AOperation: TBlendOperation; AExcludeChannels: TChannels); overload; override;
    procedure BlendRectOver(ADest: TRect; AColor: TBGRAPixel; AOperation: TBlendOperation; AOpacity: byte; ALinearBlend: boolean; AExcludeChannels: TChannels); overload; override;
    procedure BlendImage(x, y: integer; ASource: TBGRACustomBitmap; AOperation: TBlendOperation); overload; override;
    procedure BlendImage(ADest: TRect; ASource: IBGRAScanner; AOffsetX, AOffsetY: integer; AOperation: TBlendOperation); overload; override;
    procedure BlendImageOver(x, y: integer; ASource: TBGRACustomBitmap; AOperation: TBlendOperation; AOpacity: byte = 255; ALinearBlend: boolean = false); overload; override;
    procedure BlendImageOver(ADest: TRect; ASource: IBGRAScanner; AOffsetX, AOffsetY: integer; AOperation: TBlendOperation; AOpacity: byte = 255; ALinearBlend: boolean = false); overload; override;

    function GetPtrBitmap(Top,Bottom: Integer): TBGRACustomBitmap; override;
    function MakeBitmapCopy(BackgroundColor: TColor; AMasked: boolean = False): TBitmap; override;

    function Resample(newWidth, newHeight: integer;
      mode: TResampleMode = rmFineResample): TBGRADefaultBitmap; override;
    procedure Negative; override;
    procedure NegativeRect(ABounds: TRect); override;
    procedure LinearNegative; override;
    procedure LinearNegativeRect(ABounds: TRect); override;
    procedure InplaceGrayscale(AGammaCorrection: boolean = true); overload; override;
    procedure InplaceGrayscale(ABounds: TRect; AGammaCorrection: boolean = true); overload; override;
    procedure InplaceNormalize(AEachChannel: boolean = True); overload; override;
    procedure InplaceNormalize(ABounds: TRect; AEachChannel: boolean = True); overload; override;
    procedure SwapRedBlue; override;
    procedure SwapRedBlue(ARect: TRect); override;
    procedure GrayscaleToAlpha; override;
    procedure AlphaToGrayscale; override;
    function GetMaskFromAlpha: TBGRADefaultBitmap; override;
    function GetGrayscaleMaskFromAlpha: TGrayscaleMask;
    procedure ConvertToLinearRGB; override;
    procedure ConvertFromLinearRGB; override;

    {Filters}
    function FilterSmartZoom3(Option: TMedianOption): TBGRADefaultBitmap; override;
    function FilterMedian(Option: TMedianOption): TBGRADefaultBitmap; override;
    function FilterSmooth: TBGRADefaultBitmap; override;
    function FilterSharpen(Amount: single = 1): TBGRADefaultBitmap; overload; override;
    function FilterSharpen(ABounds: TRect; Amount: single = 1): TBGRADefaultBitmap; overload; override;
    function FilterContour(AGammaCorrection: boolean = false): TBGRADefaultBitmap; override;
    function FilterPixelate(pixelSize: integer; useResample: boolean; filter: TResampleFilter = rfLinear): TBGRADefaultBitmap; override;
    function FilterEmboss(angle: single; AStrength: integer= 64; AOptions: TEmbossOptions = []): TBGRADefaultBitmap; overload; override;
    function FilterEmboss(angle: single; ABounds: TRect; AStrength: integer= 64; AOptions: TEmbossOptions = []): TBGRADefaultBitmap; overload; override;
    function FilterEmbossHighlight(FillSelection: boolean): TBGRADefaultBitmap; overload; override;
    function FilterEmbossHighlight(FillSelection: boolean; BorderColor: TBGRAPixel): TBGRADefaultBitmap; overload; override;
    function FilterEmbossHighlight(FillSelection: boolean; BorderColor: TBGRAPixel; var Offset: TPoint): TBGRADefaultBitmap; overload; override;
    function FilterGrayscale: TBGRADefaultBitmap; overload; override;
    function FilterGrayscale(ABounds: TRect): TBGRADefaultBitmap; overload; override;
    function FilterNormalize(eachChannel: boolean = True): TBGRADefaultBitmap; overload; override;
    function FilterNormalize(ABounds: TRect; eachChannel: boolean = True): TBGRADefaultBitmap; overload; override;
    function FilterRotate(origin: TPointF; angle: single; correctBlur: boolean = false): TBGRADefaultBitmap; override;
    function FilterAffine(AMatrix: TAffineMatrix; correctBlur: boolean = false): TBGRADefaultBitmap; override;
    function FilterSphere: TBGRADefaultBitmap; override;
    function FilterTwirl(ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRADefaultBitmap; overload; override;
    function FilterTwirl(ABounds: TRect; ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRADefaultBitmap; overload; override;
    function FilterCylinder: TBGRADefaultBitmap; override;
    function FilterPlane: TBGRADefaultBitmap; override;
  end;

  { TBGRAPtrBitmap }

  TBGRAPtrBitmap = class(TBGRADefaultBitmap)
  protected
    function GetLineOrder: TRawImageLineOrder; override;
    procedure SetLineOrder(AValue: TRawImageLineOrder); override;
    procedure ReallocData; override;
    procedure FreeData; override;
    procedure CannotResize;
    procedure NotImplemented;
    procedure RebuildBitmap; override;

    function CreateDefaultFontRenderer: TBGRACustomFontRenderer; override; //to override
    function LoadFromRawImage({%H-}ARawImage: TRawImage; {%H-}DefaultOpacity: byte;
      {%H-}AlwaysReplaceAlpha: boolean=False; {%H-}RaiseErrorOnInvalidPixelFormat: boolean
      =True): boolean; override; //to override
  public
    constructor Create(AWidth, AHeight: integer; AData: Pointer); overload;
    procedure SetDataPtr(AData: Pointer);
    property LineOrder: TRawImageLineOrder Read GetLineOrder Write SetLineOrder;

    procedure DataDrawTransparent({%H-}ACanvas: TCanvas; {%H-}Rect: TRect; {%H-}AData: Pointer;
      {%H-}ALineOrder: TRawImageLineOrder; {%H-}AWidth, {%H-}AHeight: integer); override; //to override
    procedure DataDrawOpaque({%H-}ACanvas: TCanvas; {%H-}Rect: TRect; {%H-}AData: Pointer;
      {%H-}ALineOrder: TRawImageLineOrder; {%H-}AWidth, {%H-}AHeight: integer); override; //to override
    procedure GetImageFromCanvas({%H-}CanvasSource: TCanvas; {%H-}x, {%H-}y: integer); override; //to override

    procedure Assign({%H-}Source: TPersistent); override;
    procedure TakeScreenshot({%H-}ARect: TRect); override;
    procedure TakeScreenshotOfPrimaryMonitor; override;
    procedure LoadFromDevice({%H-}DC: HDC); override;
    procedure LoadFromDevice({%H-}DC: HDC; {%H-}ARect: TRect); override;
  end;

  { TBGRAMemoryStreamBitmap }

  TBGRAMemoryStreamBitmap = class(TBGRAPtrBitmap)
  private
    function GetOwnStream: boolean;
    procedure SetOwnStream(AValue: boolean);
  protected
    FStream: TMemoryStream;
    FStreamOffset: IntPtr;
    FOwnStream: boolean;
  public
    constructor Create(AWidth, AHeight: integer; AStream: TMemoryStream; AStreamOffset: IntPtr; AOwnStream: boolean);
    constructor Create(AWidth, AHeight: integer); override;
    constructor Create(AWidth, AHeight: integer; AColor: TBGRAPixel);
    destructor Destroy; override;
    property OwnStream: boolean read GetOwnStream write SetOwnStream;
    property Stream: TMemoryStream read FStream;
  end;

var
  DefaultTextStyle: TTextStyle;

procedure BGRAGradientFill(bmp: TBGRACustomBitmap; x, y, x2, y2: integer;
  c1, c2: TBGRAPixel; gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
  gammaColorCorrection: boolean = True; Sinus: Boolean=False);

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAPolygon;

{$mode objfpc}{$H+}

{ This unit contains polygon drawing functions and spline functions.

  Shapes are drawn using a TBGRACustomFillInfo object, which calculates the
  intersection of an horizontal line and the polygon.

  Various shapes are handled :
  - TFillPolyInfo : polygon scanned in any order
  - TSimpleFillPolyInfo : polygon with few points
  - TOnePassFillPolyInfo : polygon scanned from top to bottom
  - TFillEllipseInfo : ellipse
  - TFillBorderEllipseInfo : ellipse border
  - TFillRoundRectangleInfo : round rectangle (or other corners)
  - TFillBorderRoundRectInfo : round rectangle border

  Various fill modes :
  - Alternate : each time there is an intersection, it enters or go out of the polygon
  - Winding : filled when the sum of ascending and descending intersection is non zero
  - Color : fill with a color defined as a TBGRAPixel argument
  - Erase : erase with an alpha in the TBGRAPixel argument
  - Texture : draws a texture with the IBGRAScanner argument

  Various border handling :
  - aliased : one horizontal line intersection is calculated per pixel in the vertical loop
  - antialiased : more lines are calculated and a density is computed by adding them together
  - multi-polygon antialiasing and superposition (TBGRAMultiShapeFiller) : same as above but
    by combining multiple polygons at the same time, and optionally subtracting top polygons
  }

interface

uses
  BGRAClasses, SysUtils, BGRAGraphics, BGRABitmapTypes, BGRAFillInfo, BGRAPath;

procedure FillShapeAliased(bmp: TBGRACustomBitmap; shapeInfo: TBGRACustomFillInfo;
  c: TBGRAPixel; EraseMode: boolean; scan: IBGRAScanner; NonZeroWinding: boolean; drawmode: TDrawMode; AliasingIncludeBottomRight: Boolean= false);
procedure FillShapeAliased(bmp: TCustomUniversalBitmap; shapeInfo: TBGRACustomFillInfo;
  brush: TUniversalBrush; Alpha: Word; NonZeroWinding: boolean; AliasingIncludeBottomRight: Boolean= false);
procedure FillPolyAliased(bmp: TBGRACustomBitmap; points: array of TPointF;
  c: TBGRAPixel; EraseMode: boolean; NonZeroWinding: boolean; drawmode: TDrawMode; APixelCenteredCoordinates: boolean = true);
procedure FillPolyAliasedWithTexture(bmp: TBGRACustomBitmap; const points: array of TPointF;
  scan: IBGRAScanner; NonZeroWinding: boolean; drawmode: TDrawMode; APixelCenteredCoordinates: boolean = true);
procedure FillPolyAliased(bmp: TCustomUniversalBitmap; const points: array of TPointF;
  brush: TUniversalBrush; Alpha: Word; NonZeroWinding: boolean; APixelCenteredCoordinates: boolean = true);

procedure FillShapeAntialias(bmp: TBGRACustomBitmap; shapeInfo: TBGRACustomFillInfo;
  c: TBGRAPixel; EraseMode: boolean; scan: IBGRAScanner; NonZeroWinding: boolean; LinearBlend: boolean = false);
procedure FillShapeAntialias(bmp: TBGRACustomBitmap; shapeInfo: TBGRACustomFillInfo;
  c: TBGRAPixel; EraseMode: boolean; scan: IBGRAScanner; NonZeroWinding: boolean; drawmode: TDrawMode);
procedure FillShapeAntialias(bmp: TCustomUniversalBitmap; shapeInfo: TBGRACustomFillInfo;
  brush: TUniversalBrush; NonZeroWinding: boolean);
procedure FillShapeAntialiasWithTexture(bmp: TBGRACustomBitmap; shapeInfo: TBGRACustomFillInfo;
  scan: IBGRAScanner; NonZeroWinding: boolean; LinearBlend: boolean = false);
procedure FillPolyAntialias(bmp: TBGRACustomBitmap; points: array of TPointF;
  c: TBGRAPixel; EraseMode: boolean; NonZeroWinding: boolean; LinearBlend: boolean = false; APixelCenteredCoordinates: boolean = true);
procedure FillPolyAntialiasWithTexture(bmp: TBGRACustomBitmap; const points: array of TPointF;
  scan: IBGRAScanner; NonZeroWinding: boolean; LinearBlend: boolean = false; APixelCenteredCoordinates: boolean = true);
procedure FillPolyAntialias(bmp: TCustomUniversalBitmap; const points: array of TPointF;
  brush: TUniversalBrush; NonZeroWinding: boolean; APixelCenteredCoordinates: boolean = true);

type

  { TBGRAMultishapeFiller }

  TBGRAMultishapeFiller = class
  protected
    nbShapes: integer;
    shapes: array of record
        info: TBGRACustomFillInfo;
        internalInfo: boolean;
        texture: IBGRAScanner;
        internalTexture: TObject;
        color: TExpandedPixel;
        bounds: TRect;
        fillMode: TFillMode;
        fillModeOverride: boolean;
      end;
    function AddShape(AInfo: TBGRACustomFillInfo; AInternalInfo: boolean; ATexture: IBGRAScanner; AInternalTexture: TObject; AColor: TBGRAPixel): integer; overload;
    function CheckRectangleBorderBounds(var x1, y1, x2, y2: single; w: single): boolean;
    procedure InternalAddStroke(const APoints: array of TPointF; AClosed: boolean; AData: Pointer);
  public
    FillMode : TFillMode;
    PolygonOrder: TPolygonOrder;
    Antialiasing: Boolean;
    AliasingIncludeBottomRight: Boolean;
    constructor Create;
    destructor Destroy; override;
    function AddShape(AShape: TBGRACustomFillInfo; AColor: TBGRAPixel): integer; overload;
    function AddShape(AShape: TBGRACustomFillInfo; ATexture: IBGRAScanner): integer; overload;
    function AddPolygon(const points: array of TPointF; AColor: TBGRAPixel): integer; overload;
    function AddPolygon(const points: array of TPointF; ATexture: IBGRAScanner): integer; overload;
    procedure AddPathStroke(APath: TBGRAPath; AColor: TBGRAPixel; AWidth: single; AStroker: TBGRACustomPenStroker); overload;
    procedure AddPathStroke(APath: TBGRAPath; ATexture: IBGRAScanner; AWidth: single; AStroker: TBGRACustomPenStroker); overload;
    procedure AddPathStroke(APath: TBGRAPath; AMatrix: TAffineMatrix; AColor: TBGRAPixel; AWidth: single; AStroker: TBGRACustomPenStroker); overload;
    procedure AddPathStroke(APath: TBGRAPath; AMatrix: TAffineMatrix; ATexture: IBGRAScanner; AWidth: single; AStroker: TBGRACustomPenStroker); overload;
    function AddPathFill(APath: TBGRAPath; AColor: TBGRAPixel): integer; overload;
    function AddPathFill(APath: TBGRAPath; ATexture: IBGRAScanner): integer; overload;
    function AddPathFill(APath: TBGRAPath; AMatrix: TAffineMatrix; AColor: TBGRAPixel): integer; overload;
    function AddPathFill(APath: TBGRAPath; AMatrix: TAffineMatrix; ATexture: IBGRAScanner): integer; overload;
    function AddPolylineStroke(const points: array of TPointF; AColor: TBGRAPixel; AWidth: single; AStroker: TBGRACustomPenStroker): integer; overload;
    function AddPolylineStroke(const points: array of TPointF; ATexture: IBGRAScanner; AWidth: single; AStroker: TBGRACustomPenStroker): integer; overload;
    function AddPolygonStroke(const points: array of TPointF; AColor: TBGRAPixel; AWidth: single; AStroker: TBGRACustomPenStroker): integer; overload;
    function AddPolygonStroke(const points: array of TPointF; ATexture: IBGRAScanner; AWidth: single; AStroker: TBGRACustomPenStroker): integer; overload;
    function AddTriangleLinearColor(pt1, pt2, pt3: TPointF; c1, c2, c3: TBGRAPixel): integer;
    function AddTriangleLinearMapping(pt1, pt2, pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF): integer;
    procedure AddQuadLinearColor(pt1, pt2, pt3, pt4: TPointF; c1, c2, c3, c4: TBGRAPixel);
    procedure AddQuadLinearMapping(pt1, pt2, pt3, pt4: TPointF; texture: IBGRAScanner; tex1, tex2, {%H-}tex3, tex4: TPointF;
       ACulling: TFaceCulling = fcNone);
    procedure AddQuadPerspectiveMapping(pt1, pt2, pt3, pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF);
    function AddEllipse(x, y, rx, ry: single; AColor: TBGRAPixel): integer; overload;
    function AddEllipse(x, y, rx, ry: single; ATexture: IBGRAScanner): integer; overload;
    function AddEllipseBorder(x, y, rx, ry, w: single; AColor: TBGRAPixel): integer; overload;
    function AddEllipseBorder(x, y, rx, ry, w: single; ATexture: IBGRAScanner): integer; overload;
    function AddRoundRectangle(x1, y1, x2, y2, rx, ry: single; AColor: TBGRAPixel; options: TRoundRectangleOptions= []): integer; overload;
    function AddRoundRectangle(x1, y1, x2, y2, rx, ry: single; ATexture: IBGRAScanner; options: TRoundRectangleOptions= []): integer; overload;
    function AddRoundRectangleBorder(x1, y1, x2, y2, rx, ry, w: single; AColor: TBGRAPixel; options: TRoundRectangleOptions= []): integer; overload;
    function AddRoundRectangleBorder(x1, y1, x2, y2, rx, ry, w: single; ATexture: IBGRAScanner; options: TRoundRectangleOptions= []): integer; overload;
    function AddRectangle(x1, y1, x2, y2: single; AColor: TBGRAPixel): integer; overload;
    function AddRectangle(x1, y1, x2, y2: single; ATexture: IBGRAScanner): integer; overload;
    function AddRectangleBorder(x1, y1, x2, y2, w: single; AColor: TBGRAPixel): integer; overload;
    function AddRectangleBorder(x1, y1, x2, y2, w: single; ATexture: IBGRAScanner): integer; overload;
    procedure OverrideFillMode(AShapeIndex: integer; AFillMode: TFillMode);
    procedure Draw(dest: TBGRACustomBitmap; ADrawMode: TDrawMode = dmDrawWithTransparency);
    property ShapeCount: integer read nbShapes;
  end;

procedure FillEllipseAntialias(bmp: TCustomUniversalBitmap; x, y, rx, ry: single; ABrush: TUniversalBrush);
procedure FillEllipseAntialias(bmp: TBGRACustomBitmap; x, y, rx, ry: single;
  c: TBGRAPixel; EraseMode: boolean; LinearBlend: boolean = false);
procedure FillEllipseAntialiasWithTexture(bmp: TBGRACustomBitmap; x, y, rx, ry: single;
  scan: IBGRAScanner; LinearBlend: boolean = false);

procedure BorderEllipseAntialias(bmp: TCustomUniversalBitmap; x, y, rx, ry, w: single; ABrush: TUniversalBrush);
procedure BorderEllipseAntialias(bmp: TBGRACustomBitmap; x, y, rx, ry, w: single;
  c: TBGRAPixel; EraseMode: boolean; LinearBlend: boolean = false);
procedure BorderEllipseAntialiasWithTexture(bmp: TBGRACustomBitmap; x, y, rx, ry, w: single;
  scan: IBGRAScanner; LinearBlend: boolean = false);

procedure BorderEllipse(bmp: TCustomUniversalBitmap; x, y, rx, ry, w: single; ABrush: TUniversalBrush; AAlpha: word = 65535);
procedure BorderEllipse(bmp: TBGRACustomBitmap; x, y, rx, ry, w: single;
  c: TBGRAPixel; EraseMode: boolean; drawmode: TDrawMode);
procedure BorderEllipseWithTexture(bmp: TBGRACustomBitmap; x, y, rx, ry, w: single;
  scan: IBGRAScanner; drawmode: TDrawMode);

procedure FillRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1, x2, y2, rx, ry: single;
  options: TRoundRectangleOptions; c: TBGRAPixel; EraseMode: boolean; LinearBlend: boolean = false; APixelCenteredCoordinates: boolean = true);
procedure FillRoundRectangleAntialiasWithTexture(bmp: TBGRACustomBitmap; x1, y1, x2, y2, rx, ry: single;
  options: TRoundRectangleOptions; scan: IBGRAScanner; LinearBlend: boolean = false; APixelCenteredCoordinates: boolean = true);

procedure BorderRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1, x2, y2, rx, ry, w: single;
  options: TRoundRectangleOptions; c: TBGRAPixel; EraseMode: boolean; LinearBlend: boolean = false; APixelCenteredCoordinates: boolean = true);
procedure BorderRoundRectangleAntialiasWithTexture(bmp: TBGRACustomBitmap; x1, y1, x2, y2, rx, ry, w: single;
  options: TRoundRectangleOptions; scan: IBGRAScanner; LinearBlend: boolean = false; APixelCenteredCoordinates: boolean = true);

procedure BorderAndFillRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1, x2, y2, rx, ry, w: single;
  options: TRoundRectangleOptions; bordercolor,fillcolor: TBGRAPixel; bordertexture,filltexture: IBGRAScanner; EraseMode: boolean; APixelCenteredCoordinates: boolean = true);

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit UniversalDrawer;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPImage, BGRABitmapTypes, BGRAGraphics, BGRAPen, BGRAArrow;

type

  { TUniversalDrawer }

  TUniversalDrawer = class(TCustomUniversalDrawer)

    class function GetMaxColorChannelDepth(ADest: TCustomUniversalBitmap): byte;

    {==== Load and save files ====}

    //there are UTF8 functions that are different from standard function as those
    //depend on TFPCustomImage that does not clearly handle UTF8

    {** Load image from a file. ''filename'' is an ANSI string }
    class procedure LoadFromFile(ADest: TCustomUniversalBitmap; const AFilename: string); overload; override;
    class procedure LoadFromFile(ADest: TCustomUniversalBitmap; const AFilename: string; AOptions: TBGRALoadingOptions); overload; override;
    {** Load image from a file with the specified image reader. ''filename'' is an ANSI string }
    class procedure LoadFromFile(ADest: TCustomUniversalBitmap; const AFilename:String; AHandler:TFPCustomImageReader); overload; override;
    class procedure LoadFromFile(ADest: TCustomUniversalBitmap; const AFilename:String; AHandler:TFPCustomImageReader; AOptions: TBGRALoadingOptions); overload; override;
    {** Load image from a file. ''filename'' is an UTF8 string }
    class procedure LoadFromFileUTF8(ADest: TCustomUniversalBitmap; const AFilenameUTF8: string; AOptions: TBGRALoadingOptions = []); overload; override;
    {** Load image from a file with the specified image reader. ''filename'' is an UTF8 string }
    class procedure LoadFromFileUTF8(ADest: TCustomUniversalBitmap; const AFilenameUTF8: string; AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions = []); overload; override;
    {** Load image from a stream. Format is detected automatically }
    class procedure LoadFromStream(ADest: TCustomUniversalBitmap; AStream: TStream); overload; override;
    class procedure LoadFromStream(ADest: TCustomUniversalBitmap; AStream: TStream; AOptions: TBGRALoadingOptions); overload; override;
    {** Load image from a stream. The specified image reader is used }
    class procedure LoadFromStream(ADest: TCustomUniversalBitmap; AStream: TStream; AHandler: TFPCustomImageReader); overload; override;
    class procedure LoadFromStream(ADest: TCustomUniversalBitmap; AStream: TStream; AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions); overload; override;
    {** Load image from an embedded Lazarus resource. Format is detected automatically }
    class procedure LoadFromResource(ADest: TCustomUniversalBitmap; AFilename: string); overload; override;
    class procedure LoadFromResource(ADest: TCustomUniversalBitmap; AFilename: string; AOptions: TBGRALoadingOptions); overload; override;
    {** Load image from an embedded Lazarus resource. The specified image reader is used }
    class procedure LoadFromResource(ADest: TCustomUniversalBitmap; AFilename: string; AHandler: TFPCustomImageReader); overload; override;
    class procedure LoadFromResource(ADest: TCustomUniversalBitmap; AFilename: string; AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions); overload; override;

    {** Save image to a file. The format is guessed from the file extension. ''filename'' is an ANSI string }
    class procedure SaveToFile(ASource: TCustomUniversalBitmap; const AFilename: string); overload; override;
    {** Save image to a file with the specified image writer. ''filename'' is an ANSI string }
    class procedure SaveToFile(ASource: TCustomUniversalBitmap; const AFilename: string; AHandler:TFPCustomImageWriter); overload; override;
    {** Save image to a file. The format is guessed from the file extension. ''filename'' is an ANSI string }
    class procedure SaveToFileUTF8(ASource: TCustomUniversalBitmap; const AFilenameUTF8: string); overload; override;
    {** Save image to a file with the specified image writer. ''filename'' is an UTF8 string }
    class procedure SaveToFileUTF8(ASource: TCustomUniversalBitmap; const AFilenameUTF8: string; AHandler:TFPCustomImageWriter); overload; override;

    {** Save image to a stream in the specified image format }
    class procedure SaveToStreamAs(ASource: TCustomUniversalBitmap; AStream: TStream; AFormat: TBGRAImageFormat); override;
    {** Save image to a stream in PNG format }
    class procedure SaveToStreamAsPng(ASource: TCustomUniversalBitmap; AStream: TStream); override;

    {==== Pixelwise drawing ====}

    class function CheckRectBounds(var x,y,x2,y2: integer; minsize: integer): boolean;
    class function CheckAntialiasRectBounds(var x, y, x2, y2: single; w: single): boolean;

    {** Draws an aliased line from (x1,y1) to (x2,y2) using Bresenham's algorithm.
        ''DrawLastPixel'' specifies if (x2,y2) must be drawn. }
    class procedure DrawLine(ADest: TCustomUniversalBitmap; x1, y1, x2, y2: integer; const ABrush: TUniversalBrush; DrawLastPixel: boolean; AAlpha: Word = 65535); override;
    {** Draws an antialiased line from (x1,y1) to (x2,y2) using an improved version of Bresenham's algorithm
        ''c'' specifies the color. ''DrawLastPixel'' specifies if (x2,y2) must be drawn }
    class procedure DrawLineAntialias(ADest: TCustomUniversalBitmap; x1, y1, x2, y2: integer; const ABrush: TUniversalBrush; DrawLastPixel: boolean; AAlpha: Word = 65535); overload; override;
    {** Draws an antialiased line with two colors ''c1'' and ''c2'' as dashes of length ''dashLen''.
        ''DashPos'' can be used to specify the start dash position and to retrieve the dash position at the end
        of the line, in order to draw a polyline with consistent dashes }
    class procedure DrawLineAntialias(ADest: TCustomUniversalBitmap; x1, y1, x2, y2: integer; const ABrush1, ABrush2: TUniversalBrush; ADashLen: integer; var DashPos: integer; DrawLastPixel: boolean; AAlpha: Word = 65535); override;

    class procedure DrawPolyLine(ADest: TCustomUniversalBitmap; const points: array of TPoint; const ABrush: TUniversalBrush; DrawLastPixel: boolean; AAlpha: Word = 65535); override;
    class procedure DrawPolyLineAntialias(ADest: TCustomUniversalBitmap; const points: array of TPoint; const ABrush: TUniversalBrush; DrawLastPixel: boolean; AAlpha: Word = 65535); overload; override;
    class procedure DrawPolyLineAntialias(ADest: TCustomUniversalBitmap; const points: array of TPoint; const ABrush1, ABrush2: TUniversalBrush; ADashLen: integer; DrawLastPixel: boolean; AAlpha: Word = 65535); overload; override;

    class procedure DrawPolygon(ADest: TCustomUniversalBitmap; const points: array of TPoint; const ABrush: TUniversalBrush; AAlpha: Word = 65535); override;
    class procedure DrawPolygonAntialias(ADest: TCustomUniversalBitmap; const points: array of TPoint; const ABrush: TUniversalBrush; AAlpha: Word = 65535); overload; override;
    class procedure DrawPolygonAntialias(ADest: TCustomUniversalBitmap; const points: array of TPoint; const ABrush1, ABrush2: TUniversalBrush; ADashLen: integer; AAlpha: Word = 65535); overload; override;

    {** Draw the border of a rectangle }
    class procedure Rectangle(ADest: TCustomUniversalBitmap; x, y, x2, y2: integer; const ABrush: TUniversalBrush; AAlpha: Word = 65535); overload; override;
    {** Draw a filled rectangle with a border }
    class procedure Rectangle(ADest: TCustomUniversalBitmap; x, y, x2, y2: integer; const ABorderBrush, AFillBrush: TUniversalBrush; AAlpha: Word = 65535); overload; override;

    class procedure RoundRect(ADest: TCustomUniversalBitmap; X1, Y1, X2, Y2: integer; DX, DY: integer; const ABorderBrush, AFillBrush: TUniversalBrush; AAlpha: Word = 65535); overload; override;
    class procedure RoundRect(ADest: TCustomUniversalBitmap; X1, Y1, X2, Y2: integer; DX, DY: integer; const ABorderBrush: TUniversalBrush; AAlpha: Word = 65535); overload; override;
    class procedure FillRoundRect(ADest: TCustomUniversalBitmap; X1, Y1, X2, Y2: integer; DX, DY: integer; const AFillBrush: TUniversalBrush; AAlpha: Word = 65535); override;

    class procedure FillShape(ADest: TCustomUniversalBitmap; AShape: TBGRACustomFillInfo; AFillMode: TFillMode; ABrush: TUniversalBrush; AAlpha: Word = 65535); override;
    class procedure FillPoly(ADest: TCustomUniversalBitmap; const APoints: array of TPointF; AFillMode: TFillMode; ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean = true; AAlpha: Word = 65535); override;

    {==== Using pen ====}
    class function CreatePenStroker: TBGRACustomPenStroker; override;
    class function CreateArrow: TBGRACustomArrow; override;

    class procedure RectangleAntialias(ADest: TCustomUniversalBitmap; APen: TBGRACustomPenStroker; x, y, x2, y2: single;
                       const ABrush: TUniversalBrush; AWidth: single); override;
    class procedure DrawPolygonAntialias(ADest: TCustomUniversalBitmap; APen: TBGRACustomPenStroker;
                       const APoints: array of TPointF; const ABrush: TUniversalBrush; AWidth: single); overload; override;

    class procedure Ellipse(ADest: TCustomUniversalBitmap; APen: TBGRACustomPenStroker; x, y, rx, ry: single;
        const ABrush: TUniversalBrush; AWidth: single; AAlpha: Word=65535); overload; override;
    class procedure Ellipse(ADest: TCustomUniversalBitmap; APen: TBGRACustomPenStroker; const AOrigin, AXAxis, AYAxis: TPointF;
        const ABrush: TUniversalBrush; AWidth: single; AAlpha: Word=65535); overload; override;
    class procedure EllipseAntialias(ADest: TCustomUniversalBitmap; APen: TBGRACustomPenStroker; x, y, rx, ry: single;
        const ABrush: TUniversalBrush; AWidth: single); overload; override;
    class procedure EllipseAntialias(ADest: TCustomUniversalBitmap; APen: TBGRACustomPenStroker; const AOrigin, AXAxis, AYAxis: TPointF;
        const ABrush: TUniversalBrush; AWidth: single); overload; override;

    {==== Filling ====}
    class procedure FillRectAntialias(ADest: TCustomUniversalBitmap;
                    x, y, x2, y2: single; const ABrush: TUniversalBrush;
                    APixelCenteredCoordinates: boolean = true); override;
    class procedure FillRoundRectAntialias(ADest: TCustomUniversalBitmap;
                    x,y,x2,y2, rx,ry: single; const ABrush: TUniversalBrush;
                    AOptions: TRoundRectangleOptions = []; APixelCenteredCoordinates: boolean = true); override;
    class procedure FillShapeAntialias(ADest: TCustomUniversalBitmap;
                    AShape: TBGRACustomFillInfo; AFillMode: TFillMode;
                    ABrush: TUniversalBrush); override;
    class procedure FillPolyAntialias(ADest: TCustomUniversalBitmap;
                    const APoints: array of TPointF; AFillMode: TFillMode;
                    ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean); override;
    class procedure FillEllipseAntialias(ADest: TCustomUniversalBitmap;
                    x, y, rx, ry: single; const ABrush: TUniversalBrush); overload; override;
    class procedure FillEllipseAntialias(ADest: TCustomUniversalBitmap;
                    const AOrigin, AXAxis, AYAxis: TPointF; const ABrush: TUniversalBrush); overload; override;

    //filters
    class procedure FilterBlurRadial(ASource: TCustomUniversalBitmap; const ABounds: TRect;
                              radiusX, radiusY: single; blurType: TRadialBlurType;
                              ADest: TCustomUniversalBitmap); override;
    class procedure FilterBlurMotion(ASource: TCustomUniversalBitmap; const ABounds: TRect;
                              distance: single; angle: single; oriented: boolean;
                              ADest: TCustomUniversalBitmap); override;
    class procedure FilterCustomBlur(ASource: TCustomUniversalBitmap; const ABounds: TRect;
                              mask: TCustomUniversalBitmap;
                              ADest: TCustomUniversalBitmap); override;

  end;

Implementation
End.

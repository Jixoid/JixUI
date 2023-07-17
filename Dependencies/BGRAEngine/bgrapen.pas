unit BGRAPen;

{$mode objfpc}{$H+}
{$WARN 5093 off : function result variable of a managed type does not seem to be initialized}
interface

{ This unit handles pen style and width, as well as line caps and join styles.

  A line consists in two points.
  A polyline consists in one or more lines, defined by two points or more than two points
  A poly-polyline consists in a series of polylines, defined by polyline points separated by empty points (see EmptyPointF) }

uses
  SysUtils, BGRAGraphics, BGRABitmapTypes, BGRATransform;

var   //predefined pen styles
  SolidPenStyle, DashPenStyle, DotPenStyle, DashDotPenStyle, DashDotDotPenStyle, ClearPenStyle: TBGRAPenStyle;

type
  TPenJoinStyle = BGRAGraphics.TPenJoinStyle;
  TPenEndCap = BGRAGraphics.TPenEndCap;

  { TBGRAPenStroker }

  TBGRAPenStroker = class(TBGRACustomPenStroker)
    protected
      { Pen style can be defined by PenStyle property of by CustomPenStyle property.
      When PenStyle property is assigned, CustomPenStyle property is assigned the actual
      pen pattern. }
      FCustomPenStyle: TBGRAPenStyle;
      FPenStyle: TPenStyle;
      FArrow: TBGRACustomArrow;
      FArrowOwned: boolean;
      FOriginalStrokeMatrix,FStrokeMatrix,FStrokeMatrixInverse: TAffineMatrix;
      FStrokeZoom: single;
      FStrokeMatrixIdentity: boolean;
      FLineCap: TPenEndCap;
      FJoinStyle: TPenJoinStyle;
      FMiterLimit: single;

      function GetArrow: TBGRACustomArrow; override;
      function GetArrowOwned: boolean; override;
      function GetCustomPenStyle: TBGRAPenStyle; override;
      function GetJoinStyle: TPenJoinStyle; override;
      function GetLineCap: TPenEndCap; override;
      function GetMiterLimit: single; override;
      function GetPenStyle: TPenStyle; override;
      function GetStrokeMatrix: TAffineMatrix; override;
      procedure SetArrow(AValue: TBGRACustomArrow); override;
      procedure SetArrowOwned(AValue: boolean); override;
      procedure SetCustomPenStyle(AValue: TBGRAPenStyle); override;
      procedure SetJoinStyle(AValue: TPenJoinStyle); override;
      procedure SetLineCap(AValue: TPenEndCap); override;
      procedure SetMiterLimit(AValue: single); override;
      procedure SetPenStyle(AValue: TPenStyle); override;
      procedure SetStrokeMatrix(const AValue: TAffineMatrix); override;
    public
      constructor Create;
      destructor Destroy; override;
      function ComputePolyline(const APoints: array of TPointF; AWidth: single; AClosedCap: boolean = true): ArrayOfTPointF; overload; override;
      function ComputePolyline(const APoints: array of TPointF; AWidth: single; APenColor: TBGRAPixel; AClosedCap: boolean = true): ArrayOfTPointF; overload; override;
      function ComputePolylineAutocycle(const APoints: array of TPointF; AWidth: single): ArrayOfTPointF; override;
      function ComputePolygon(const APoints: array of TPointF; AWidth: single): ArrayOfTPointF; override;

  end;

  TBGRAPolyLineOption = (plRoundCapOpen, //specifies that the line ending is opened
                         plCycle,        //specifies that it is a polygon
                         plAutoCycle,    //specifies that a cycle must be used if the last point is the first point
                         plNoStartCap,
                         plNoEndCap);
  TBGRAPolyLineOptions = set of TBGRAPolyLineOption;
  TComputeArrowHeadProc = function(const APosition: TPointF; const ADirection: TPointF; const AWidth: single; const ACurrentPos: single): ArrayOfTPointF of object;

{ Compute the path for a polyline }
function ComputeWidePolylinePoints(const linepts: array of TPointF; width: single;
          pencolor: TBGRAPixel; linecap: TPenEndCap; joinstyle: TPenJoinStyle; const penstyle: TBGRAPenStyle;
          options: TBGRAPolyLineOptions; miterLimit: single = 2; arrow: TBGRACustomArrow = nil): ArrayOfTPointF;

{ Compute the path for a poly-polyline }
function ComputeWidePolyPolylinePoints(const linepts: array of TPointF; width: single;
          pencolor: TBGRAPixel; linecap: TPenEndCap; joinstyle: TPenJoinStyle; const penstyle: TBGRAPenStyle;
          options: TBGRAPolyLineOptions; miterLimit: single = 2; arrow: TBGRACustomArrow = nil): ArrayOfTPointF;

{--------------------- Pixel line procedures --------------------------}
{ These procedures take integer coordinates as parameters and do not handle pen styles and width.
  They are faster and can be useful for drawing a simple frame }

//aliased version
procedure BGRADrawLineAliased(dest: TBGRACustomBitmap; x1, y1, x2, y2: integer; c: TBGRAPixel; DrawLastPixel: boolean; ADrawMode: TDrawMode = dmDrawWithTransparency); deprecated;
procedure BGRAEraseLineAliased(dest: TBGRACustomBitmap; x1, y1, x2, y2: integer; alpha: byte; DrawLastPixel: boolean); deprecated;

//antialiased version
procedure BGRADrawLineAntialias({%H-}dest: TBGRACustomBitmap; x1, y1, x2, y2: integer;
  c: TBGRAPixel; DrawLastPixel: boolean; LinearBlend : boolean = false); overload; deprecated;
procedure BGRAEraseLineAntialias(dest: TBGRACustomBitmap; x1, y1, x2, y2: integer;
  calpha: byte; DrawLastPixel: boolean); overload; deprecated;

//antialiased version with bicolor dashes (to draw a frame)
procedure BGRADrawLineAntialias(dest: TBGRACustomBitmap; x1, y1, x2, y2: integer;
  c1, c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean; var DashPos: integer; LinearBlend : boolean = false); overload; deprecated;

//length added to ensure accepable alpha join (using TBGRAMultishapeFiller is still better)
function GetAlphaJoinFactor(alpha: byte): single;

//create standard brush texture
function CreateBrushTexture(prototype: TBGRACustomBitmap; brushstyle: TBrushStyle; PatternColor, BackgroundColor: TBGRAPixel;
    width: integer = 8; height: integer = 8; penwidth: single = 1): TBGRACustomBitmap;

//check special pen styles
function IsSolidPenStyle(ACustomPenStyle: TBGRAPenStyle): boolean;
function IsClearPenStyle(ACustomPenStyle: TBGRAPenStyle): boolean;
function DuplicatePenStyle(ACustomPenStyle: array of single): TBGRAPenStyle;
function PenStyleEqual(AStyle1, AStyle2: TBGRAPenStyle): boolean;
function BGRAToPenStyle(ACustomPenStyle: TBGRAPenStyle): TPenStyle;
function PenStyleToBGRA(APenStyle: TPenStyle): TBGRAPenStyle;

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAFillInfo;

{$mode objfpc}{$H+}
{$WARN 5093 off : function result variable of a managed type does not seem to be initialized}
interface

uses
  BGRAClasses, SysUtils, BGRABitmapTypes;

const
  AntialiasPrecision = 16;
  AntialiasPrecisionShift = 4;

type
  TDensity = word;
  PDensity = ^TDensity;

type
  { TFillShapeInfo }

  TFillShapeInfo = class(TBGRACustomFillInfo)
    protected
      FPointInsideInter : ArrayOfTIntersectionInfo;
      //compute intersections. the array must be big enough
      procedure ComputeIntersection(cury: single; var inter: ArrayOfTIntersectionInfo; var nbInter: integer); virtual;
      //sort from left to right
      procedure SortIntersection(var inter: ArrayOfTIntersectionInfo; nbInter: integer); virtual;
      procedure InternalQuickSortIntersection(inter0: pointer; idxL, idxH: Integer); virtual;
      //apply non-zero winding rule. it can change the number of intersections
      procedure ConvertFromNonZeroWinding(var inter: ArrayOfTIntersectionInfo; var nbInter: integer); virtual;
      //returns maximum of intersection per line
      function NbMaxIntersection: integer; virtual;

    public
      destructor Destroy; override;

      //returns true if the same segment number can be curved
      function SegmentsCurved: boolean; override;

      //returns integer bounds
      function GetBounds: TRect; override;

      //check if the point is inside the filling zone
      function IsPointInside(x,y: single; windingMode: boolean): boolean; override;

      //create an array that will contain computed intersections.
      //you may augment, in this case, use CreateIntersectionInfo for new items
      function CreateIntersectionArray: ArrayOfTIntersectionInfo; override;
      function CreateIntersectionInfo: TIntersectionInfo; override; //creates a single info
      procedure FreeIntersectionArray(var inter: ArrayOfTIntersectionInfo); override;

      //fill a previously created array of intersections with actual intersections at the current y coordinate.
      //nbInter gets the number of computed intersections
      procedure ComputeAndSort(cury: single; var inter: ArrayOfTIntersectionInfo; out nbInter: integer; windingMode: boolean); override;

      //can be called after ComputeAndSort or ComputeIntersection to determine the current horizontal slice
      //so that it can be checked if the intermediates scanlines can be skipped
      function GetSliceIndex: integer; override;

  end;

  { TFillEllipseInfo }

  TFillEllipseInfo = class(TFillShapeInfo)
  private
    FX, FY, FRX, FRY: single;
    FSliceIndex: integer;
    function GetCenter: TPointF;
  protected
    function NbMaxIntersection: integer; override;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  public
    WindingFactor: integer;
    constructor Create(x, y, rx, ry: single);
    function GetBounds: TRect; override;
    function SegmentsCurved: boolean; override;
    function GetSliceIndex: integer; override;
    property Center: TPointF read GetCenter;
    property RadiusX: single read FRX;
    property RadiusY: single read FRY;
  end;

  { TFillBorderEllipseInfo }

  TFillBorderEllipseInfo = class(TFillShapeInfo)
  private
    FInnerBorder, FOuterBorder: TFillEllipseInfo;
  protected
    function NbMaxIntersection: integer; override;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  public
    constructor Create(x, y, rx, ry, w: single);
    function GetBounds: TRect; override;
    function SegmentsCurved: boolean; override;
    destructor Destroy; override;
    function GetSliceIndex: integer; override;
    property InnerBorder: TFillEllipseInfo read FInnerBorder;
    property OuterBorder: TFillEllipseInfo read FOuterBorder;
  end;

  { TFillRoundRectangleInfo }

  TFillRoundRectangleInfo = class(TFillShapeInfo)
  private
    FX1, FY1, FX2, FY2, FRX, FRY: single;
    FOptions: TRoundRectangleOptions;
    function GetBottomRight: TPointF;
    function GetTopLeft: TPointF;
  protected
    function NbMaxIntersection: integer; override;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  public
    WindingFactor: integer;
    constructor Create(x1, y1, x2, y2, rx, ry: single; options: TRoundRectangleOptions; APixelCenteredCoordinates: boolean = true);
    function SegmentsCurved: boolean; override;
    function GetBounds: TRect; override;
    property TopLeft: TPointF read GetTopLeft;
    property BottomRight: TPointF read GetBottomRight;
    property RadiusX: single read FRX;
    property RadiusY: single read FRY;
  end;

  { TFillRectangleInfo }

  TFillRectangleInfo = class(TFillShapeInfo)
  private
    FX1, FY1, FX2, FY2: single;
    function GetBottomRight: TPointF;
    function GetTopLeft: TPointF;
  protected
    function NbMaxIntersection: integer; override;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  public
    WindingFactor: integer;
    constructor Create(x1, y1, x2, y2: single; APixelCenteredCoordinates: boolean = true);
    function GetBounds: TRect; override;
    property TopLeft: TPointF read GetTopLeft;
    property BottomRight: TPointF read GetBottomRight;
  end;

  { TFillBorderRoundRectInfo }

  TFillBorderRoundRectInfo = class(TFillShapeInfo)
  protected
    FInnerBorder, FOuterBorder: TFillRoundRectangleInfo;
    function NbMaxIntersection: integer; override;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  public
    constructor Create(x1, y1, x2, y2, rx, ry, w: single; options: TRoundRectangleOptions; APixelCenteredCoordinates: boolean = true);
    function GetBounds: TRect; override;
    function SegmentsCurved: boolean; override;
    destructor Destroy; override;
    property InnerBorder: TFillRoundRectangleInfo read FInnerBorder;
    property OuterBorder: TFillRoundRectangleInfo read FOuterBorder;
  end;

  PCustomPointRecord = ^TCustomPointRecord;
  TCustomPointRecord = record
    originalIndex: integer;
    slope: single;
    empty: boolean;
    next: integer;
    winding: integer;
    includeStartingPoint,includeEndingPoint: boolean;
    data: pointer;
    case boolean of
    false: (x,y,x2,y2: single);
    true: (coord,coord2: TPointF);
  end;

  { TCustomFillPolyInfo }

  TCustomFillPolyInfo = class(TFillShapeInfo)
  private
    function GetNbPoints: integer;
  protected
    FPoints: array of TCustomPointRecord;
    FSegmentsDataCreated: boolean;
    FBoundsF: TRectF;
    function NbMaxIntersection: integer; override;
    procedure SetIntersectionValues(AInter: TIntersectionInfo; AInterX: Single; AWinding, ANumSegment: integer; {%H-}dy: single; {%H-}AData: pointer); virtual;
    procedure InitPoints(const points: array of TPointF);
    procedure CreateSegmentsData; virtual;
  public
    constructor Create(const points: array of TPointF; APixelCenteredCoordinates: boolean = true);
    destructor Destroy; override;
    function CreateIntersectionArray: ArrayOfTIntersectionInfo; override;
    function CreateSegmentData({%H-}numPt, {%H-}nextPt: integer; {%H-}ASeg: PCustomPointRecord): pointer; virtual;
    procedure FreeSegmentData(data: pointer); virtual;
    function GetBounds: TRect; override;
    function GetBoundsF: TRectF;
    property NbPoints: integer read GetNbPoints;
  end;

  TPolySlice = record
    y1,y2: single;
    segments: array of record
                id: integer;
                custom: PCustomPointRecord;
              end;
    nbSegments: integer;
  end;

  { TFillPolyInfo }

  TFillPolyInfo = class(TCustomFillPolyInfo)
  protected
    FSlices:   array of TPolySlice;
    FCurSlice: integer;
    FMaxIntersection: integer;
    function NbMaxIntersection: integer; override;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  public
    constructor Create(const points: array of TPointF; APixelCenteredCoordinates: boolean = true);
    function GetSliceIndex: integer; override;
  end;

  POnePassRecord = ^TOnePassRecord;
  TOnePassRecord = record
                id: integer;
                custom: PCustomPointRecord;
                next: POnePassRecord;
                nextWaiting: POnePassRecord;
                nextDrawing: POnePassRecord;
            end;

  { TOnePassFillPolyInfo }

  TOnePassFillPolyInfo = class(TCustomFillPolyInfo)
  private
    procedure InsertionSortByY;
    function PartitionByY(left, right: integer): integer;
    procedure QuickSortByY(left, right: integer);
    procedure SortByY;
  protected
    FOnePass: array of TOnePassRecord;
    FSortedByY: array of POnePassRecord;
    FFirstWaiting, FFirstDrawing: POnePassRecord;
    FShouldInitializeDrawing: boolean;
    FSliceIndex: integer;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  public
    constructor Create(const points: array of TPointF; APixelCenteredCoordinates: boolean = true);
    function CreateIntersectionArray: ArrayOfTIntersectionInfo; override;
    function GetSliceIndex: integer; override;
  end;

  { TSimpleFillPolyInfo }

  TSimpleFillPolyInfo = class(TCustomFillPolyInfo)
  protected
    procedure ComputeIntersection(cury: single; var inter: ArrayOfTIntersectionInfo;
      var nbInter: integer); override;
  public
    constructor Create(const points: array of TPointF; APixelCenteredCoordinates: boolean = true);
  end;

procedure AddDensity(dest: PDensity; start,count: integer; value : word); inline;
function DivByAntialiasPrecision(value: UInt32or64): UInt32or64; inline;
function DivByAntialiasPrecision256(value: UInt32or64): UInt32or64; inline;
function DivByAntialiasPrecision65536(value: UInt32or64): UInt32or64; inline;
procedure ComputeAliasedRowBounds(x1,x2: single; minx,maxx: integer; out ix1,ix2: integer);

function IsPointInPolygon(const points: ArrayOfTPointF; point: TPointF; windingMode: boolean): boolean;
function IsPointInEllipse(x,y,rx,ry: single; point: TPointF): boolean;
function IsPointInRoundRectangle(x1, y1, x2, y2, rx, ry: single; point: TPointF): boolean;
function IsPointInRectangle(x1, y1, x2, y2: single; point: TPointF): boolean;

function BGRAShapeComputeMinMax(AShape: TBGRACustomFillInfo; out minx, miny, maxx, maxy: integer;
  bmpDest: TBGRACustomBitmap): boolean; overload;
function BGRAShapeComputeMinMax(AShape: TBGRACustomFillInfo; out minx, miny, maxx, maxy: integer;
  clip: TRect): boolean; overload;

Implementation
End.

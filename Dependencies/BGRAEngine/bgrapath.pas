// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAPath;

{$mode objfpc}{$H+}
{$WARN 5093 off : function result variable of a managed type does not seem to be initialized}
{$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
interface

//todo: tangent interpolation

{ There are different conventions for angles.

  First is about the unit. It can be one of the following:
  - degrees (0..360)
  - radian (0..2*Pi)
  - tenth of degrees (0..3600)
  - from 0 to 65536

  Second is about the origin. It can be one of the following:
  - right-most position (this is the default origin for radian and 65536)
  - top-most position (this is the default origin for degrees)

  Third is about the sign. It can be one of the following:
  - positive is clockwise (this is the default for degrees)
  - positive is counterclockwise (this is the default for radian and 65536)

  TBGRAPath and TBGRACanvas2D follow HTML5 convention which is:
    (radian, right-most, clockwise) that can be shortened to (radian, clockwise)
    because right-most is the default for radian. This is abbreviated as "radCW".

  When radian are CCW, it is also specified in order to make it clear, even
  if it is the default convention in mathematics.

  In order to make things easier, there are some functions that accept angles
  in degrees. The convention used here is the usual degree convention:
    (degrees, top-most, clockwise) that can be shortened to (degree)
    because top-most and clockwise is the default for degrees.

  }

uses
  BGRABitmapTypes, BGRATransform;

const
  DefaultDeviation = 0.1;

type
  TBGRAPathElementType = (peNone, peMoveTo, peLineTo, peCloseSubPath,
    peQuadraticBezierTo, peCubicBezierTo, peArc, peOpenedSpline,
    peClosedSpline);

  TBGRAPathDrawProc = BGRABitmapTypes.TBGRAPathDrawProc;
  TBGRAPathFillProc = BGRABitmapTypes.TBGRAPathFillProc;

  TBGRAPath = class;

  { TBGRAPathCursor }

  TBGRAPathCursor = class(TBGRACustomPathCursor)
  protected
    FPath: TBGRAPath;
    FDataPos: IntPtr;
    FAcceptedDeviation: single;
    FPathLength: single;
    FPathLengthComputed: boolean;
    FBounds: TRectF;
    FBoundsComputed: boolean;
    FArcPos: Single;

    FStartCoordinate: TPointF;
    FEndCoordinate: TPointF;
    FLoopClosedShapes,FLoopPath: boolean;

    FCurrentElementType: TBGRAPathElementType;
    FCurrentElement: Pointer;
    FCurrentElementArcPos,
    FCurrentElementArcPosScale: single;
    FCurrentElementStartCoord,
    FCurrentElementEndCoord: TPointF;
    FCurrentElementLength: single;
    FCurrentElementPoints: array of TPointF;
    FCurrentSegment: Int32or64;
    FCurrentSegmentPos: single;
    function GoToNextElement(ACanJump: boolean): boolean;
    function GoToPreviousElement(ACanJump: boolean): boolean;
    procedure MoveToEndOfElement;
    procedure MoveForwardInElement(ADistance: single);
    procedure MoveBackwardInElement(ADistance: single);
    function NeedPolygonalApprox: boolean;
    procedure OnPathFree; virtual;

    function GetLoopClosedShapes: boolean; override;
    function GetLoopPath: boolean; override;
    function GetStartCoordinate: TPointF; override;
    procedure SetLoopClosedShapes(AValue: boolean); override;
    procedure SetLoopPath(AValue: boolean); override;

    function GetArcPos: single; override;
    function GetCurrentTangent: TPointF; override;
    procedure SetArcPos(AValue: single); override;
    function GetBounds: TRectF; override;
    function GetPathLength: single; override;
    procedure PrepareCurrentElement; virtual;
    function GetCurrentCoord: TPointF; override;
    function GetPath: TBGRAPath; virtual;
  public
    constructor Create(APath: TBGRAPath; AAcceptedDeviation: single = DefaultDeviation);
    function MoveForward(ADistance: single; ACanJump: boolean = true): single; override;
    function MoveBackward(ADistance: single; ACanJump: boolean = true): single; override;
    destructor Destroy; override;
    property CurrentCoordinate: TPointF read GetCurrentCoord;
    property CurrentTangent: TPointF read GetCurrentTangent;
    property Position: single read GetArcPos write SetArcPos;
    property PathLength: single read GetPathLength;
    property Path: TBGRAPath read GetPath;
    property Bounds: TRectF read GetBounds;
    property StartCoordinate: TPointF read GetStartCoordinate;
    property LoopClosedShapes: boolean read GetLoopClosedShapes write SetLoopClosedShapes;
    property LoopPath: boolean read GetLoopPath write SetLoopPath;
    property AcceptedDeviation: single read FAcceptedDeviation;
  end;

  { TBGRAPath }

  TBGRAPath = class(TBGRACustomPath)
  protected
    FData: PByte;
    FDataCapacity: PtrInt;
    FDataPos: PtrInt;
    FLastSubPathElementType, FLastStoredElementType: TBGRAPathElementType;
    FLastMoveToDataPos: PtrInt;
    FLastCoord,FLastTransformedCoord,
    FSubPathStartCoord, FSubPathTransformedStartCoord: TPointF;
    FExpectedTransformedControlPoint: TPointF;
    FMatrix: TAffineMatrix; //this matrix must have a base of vectors
                            //orthogonal, of same length and with positive
                            //orientation in order to preserve arcs
    FScale,FAngleRadCW: single;
    FCursors: array of TBGRAPathCursor;
    FInternalDrawOffset: TPointF;
    procedure OnModify;
    procedure OnMatrixChange;
    procedure NeedSpace(count: integer);
    function AllocateElement(AElementType: TBGRAPathElementType;
  AExtraBytes: PtrInt = 0): Pointer;
    procedure Init;
    procedure DoClear;
    function CheckElementType(AElementType: TBGRAPathElementType): boolean;
    function GoToNextElement(var APos: PtrInt): boolean;
    function GoToPreviousElement(var APos: PtrInt): boolean;
    function PeekNextElement(APos: PtrInt): TBGRAPathElementType;
    function GetElementStartCoord(APos: PtrInt): TPointF;
    function GetElementEndCoord(APos: PtrInt): TPointF;
    function GetElementLength(APos: PtrInt; AAcceptedDeviation: single): Single;
    procedure GetElementAt(APos: PtrInt;
      out AElementType: TBGRAPathElementType; out AElement: pointer);
    function GetSvgString: string; virtual;
    procedure SetSvgString(const AValue: string); virtual;
    procedure RegisterCursor(ACursor: TBGRAPathCursor);
    procedure UnregisterCursor(ACursor: TBGRAPathCursor);
    function SetLastCoord(ACoord: TPointF): TPointF; inline;
    procedure ClearLastCoord;
    procedure BezierCurveFromTransformed(tcp1, cp2, pt:TPointF);
    procedure QuadraticCurveFromTransformed(tcp, pt: TPointF);
    function LastCoordDefined: boolean; inline;
    function GetPolygonalApprox(APos: IntPtr; AAcceptedDeviation: single; AIncludeFirstPoint: boolean): ArrayOfTPointF;
    function getPoints: ArrayOfTPointF; overload;override;
    function getPoints(AMatrix: TAffineMatrix): ArrayOfTPointF; overload;override;
    function getLength: single; override;
    function getCursor: TBGRACustomPathCursor; override;
    procedure InternalDraw(ADrawProc: TBGRAPathDrawProc; const AMatrix: TAffineMatrix; AAcceptedDeviation: single; AData: pointer);
    procedure BitmapDrawSubPathProc(const APoints: array of TPointF; AClosed: boolean; AData: pointer);
    function CorrectAcceptedDeviation(AAcceptedDeviation: single; const AMatrix: TAffineMatrix): single;
  public
    constructor Create; overload; override;
    constructor Create(ASvgString: string); overload;
    constructor Create(const APoints: ArrayOfTPointF); overload;
    constructor Create(APath: IBGRAPath); overload;
    destructor Destroy; override;
    procedure beginPath; override;
    procedure beginSubPath;
    procedure closePath; override;
    procedure translate(x,y: single);
    procedure resetTransform;
    procedure rotate(angleRadCW: single); overload;
    procedure rotateDeg(angleDeg: single); overload;
    procedure rotate(angleRadCW: single; center: TPointF); overload;
    procedure rotateDeg(angleDeg: single; center: TPointF); overload;
    procedure scale(factor: single);
    procedure moveTo(x,y: single); overload;
    procedure lineTo(x,y: single); overload;
    procedure moveTo(constref pt: TPointF); overload; override;
    procedure lineTo(constref pt: TPointF); overload; override;
    procedure polyline(const pts: array of TPointF);
    procedure polylineTo(const pts: array of TPointF); override;
    procedure polygon(const pts: array of TPointF);
    procedure quadraticCurveTo(cpx,cpy,x,y: single); overload;
    procedure quadraticCurveTo(constref cp,pt: TPointF); overload; override;
    procedure quadraticCurve(const curve: TQuadraticBezierCurve); overload;
    procedure quadraticCurve(p1,cp,p2: TPointF); overload;
    procedure smoothQuadraticCurveTo(x,y: single); overload;
    procedure smoothQuadraticCurveTo(const pt: TPointF); overload;
    procedure bezierCurveTo(cp1x,cp1y,cp2x,cp2y,x,y: single); overload;
    procedure bezierCurveTo(constref cp1,cp2,pt: TPointF); overload; override;
    procedure bezierCurve(const curve: TCubicBezierCurve); overload;
    procedure bezierCurve(p1,cp1,cp2,p2: TPointF); overload;
    procedure smoothBezierCurveTo(cp2x,cp2y,x,y: single); overload;
    procedure smoothBezierCurveTo(const cp2,pt: TPointF); overload;
    procedure rect(x,y,w,h: single);
    procedure roundRect(x,y,w,h,radius: single);
    procedure arc(cx, cy, radius, startAngleRadCW, endAngleRadCW: single; anticlockwise: boolean); overload;
    procedure arc(cx, cy, radius, startAngleRadCW, endAngleRadCW: single); overload;
    procedure arcDeg(cx, cy, radius, startAngleDeg, endAngleDeg: single; anticlockwise: boolean); overload;
    procedure arcDeg(cx, cy, radius, startAngleDeg, endAngleDeg: single); overload;
    procedure arcTo(x1, y1, x2, y2, radius: single); overload;
    procedure arcTo(const p1,p2: TPointF; radius: single); overload;
    procedure arc(constref arcDef: TArcDef); overload; override;
    procedure arc(cx, cy, rx,ry: single; xAngleRadCW, startAngleRadCW, endAngleRadCW: single); overload;
    procedure arc(cx, cy, rx,ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single; anticlockwise: boolean); overload;
    procedure arcTo(rx,ry, xAngleRadCW: single; largeArc, anticlockwise: boolean; x,y:single); overload;
    procedure copyTo(dest: IBGRAPath); override;
    procedure addPath(const AValue: string); overload;
    procedure addPath(source: IBGRAPath); overload;
    procedure openedSpline(const pts: array of TPointF; style: TSplineStyle); override;
    procedure closedSpline(const pts: array of TPointF; style: TSplineStyle); override;
    property SvgString: string read GetSvgString write SetSvgString;
    function ComputeLength(AAcceptedDeviation: single = DefaultDeviation): single;
    function ToPoints(AAcceptedDeviation: single = DefaultDeviation): ArrayOfTPointF; overload;
    function ToPoints(AMatrix: TAffineMatrix; AAcceptedDeviation: single = DefaultDeviation): ArrayOfTPointF; overload;
    function IsEmpty: boolean;
    function GetBounds(AAcceptedDeviation: single = DefaultDeviation): TRectF;
    procedure SetPoints(const APoints: ArrayOfTPointF);
    procedure stroke(ABitmap: TBGRACustomBitmap; AColor: TBGRAPixel; AWidth: single; AAcceptedDeviation: single = DefaultDeviation); overload;
    procedure stroke(ABitmap: TBGRACustomBitmap; ATexture: IBGRAScanner; AWidth: single; AAcceptedDeviation: single = DefaultDeviation); overload;
    procedure stroke(ABitmap: TBGRACustomBitmap; x,y: single; AColor: TBGRAPixel; AWidth: single; AAcceptedDeviation: single = DefaultDeviation); overload;
    procedure stroke(ABitmap: TBGRACustomBitmap; x,y: single; ATexture: IBGRAScanner; AWidth: single; AAcceptedDeviation: single = DefaultDeviation); overload;
    procedure stroke(ABitmap: TBGRACustomBitmap; const AMatrix: TAffineMatrix; AColor: TBGRAPixel; AWidth: single; AAcceptedDeviation: single = DefaultDeviation); overload;
    procedure stroke(ABitmap: TBGRACustomBitmap; const AMatrix: TAffineMatrix; ATexture: IBGRAScanner; AWidth: single; AAcceptedDeviation: single = DefaultDeviation); overload;
    procedure stroke(ADrawProc: TBGRAPathDrawProc; AData: pointer); overload; override;
    procedure stroke(ADrawProc: TBGRAPathDrawProc; const AMatrix: TAffineMatrix; AData: pointer); overload; override;
    procedure stroke(ADrawProc: TBGRAPathDrawProc; const AMatrix: TAffineMatrix; AAcceptedDeviation: single; AData: pointer = nil); overload;
    procedure fill(ABitmap: TBGRACustomBitmap; AColor: TBGRAPixel; AAcceptedDeviation: single = DefaultDeviation); overload;
    procedure fill(ABitmap: TBGRACustomBitmap; ATexture: IBGRAScanner; AAcceptedDeviation: single = DefaultDeviation); overload;
    procedure fill(ABitmap: TBGRACustomBitmap; x,y: single; AColor: TBGRAPixel; AAcceptedDeviation: single = DefaultDeviation); overload;
    procedure fill(ABitmap: TBGRACustomBitmap; x,y: single; ATexture: IBGRAScanner; AAcceptedDeviation: single = DefaultDeviation); overload;
    procedure fill(ABitmap: TBGRACustomBitmap; const AMatrix: TAffineMatrix; AColor: TBGRAPixel; AAcceptedDeviation: single = DefaultDeviation); overload;
    procedure fill(ABitmap: TBGRACustomBitmap; const AMatrix: TAffineMatrix; ATexture: IBGRAScanner; AAcceptedDeviation: single = DefaultDeviation); overload;
    procedure fill(AFillProc: TBGRAPathFillProc; AData: pointer); overload; override;
    procedure fill(AFillProc: TBGRAPathFillProc; const AMatrix: TAffineMatrix; AData: pointer); overload; override;
    procedure fill(AFillProc: TBGRAPathFillProc; const AMatrix: TAffineMatrix; AAcceptedDeviation: single; AData: pointer = nil); overload;
    function CreateCursor(AAcceptedDeviation: single = DefaultDeviation): TBGRAPathCursor;
    procedure Fit(ARect: TRectF; AAcceptedDeviation: single = DefaultDeviation);
    procedure FitInto(ADest: TBGRAPath; ARect: TRectF; AAcceptedDeviation: single = DefaultDeviation);
  end;

{----------------------- Spline ------------------}

function SplineVertexToSide(y0, y1, y2, y3: single; t: single): single;
function ComputeBezierCurve(const curve: TCubicBezierCurve; AAcceptedDeviation: single = DefaultDeviation): ArrayOfTPointF; overload;
function ComputeBezierCurve(const curve: TQuadraticBezierCurve; AAcceptedDeviation: single = DefaultDeviation): ArrayOfTPointF; overload;
function ComputeBezierSpline(const spline: array of TCubicBezierCurve; AAcceptedDeviation: single = DefaultDeviation): ArrayOfTPointF; overload;
function ComputeBezierSpline(const spline: array of TQuadraticBezierCurve; AAcceptedDeviation: single = DefaultDeviation): ArrayOfTPointF; overload;
function ComputeClosedSpline(const APoints: array of TPointF; AStyle: TSplineStyle; AAcceptedDeviation: single = DefaultDeviation): ArrayOfTPointF;
function ComputeClosedSpline(const APoints: array of TPointF; AStart, ACount: integer; AStyle: TSplineStyle; AAcceptedDeviation: single = DefaultDeviation): ArrayOfTPointF;
function ComputeOpenedSpline(const APoints: array of TPointF; AStyle: TSplineStyle; AEndCoeff: single = 0.25; AAcceptedDeviation: single = DefaultDeviation): ArrayOfTPointF;
function ComputeOpenedSpline(const APoints: array of TPointF; AStart, ACount: integer; AStyle: TSplineStyle; AEndCoeff: single = 0.25; AAcceptedDeviation: single = DefaultDeviation): ArrayOfTPointF;
function ClosedSplineStartPoint(const points: array of TPointF; Style: TSplineStyle): TPointF;
function ComputeEasyBezier(const curve: TEasyBezierCurve; AAcceptedDeviation: single = DefaultDeviation): ArrayOfTPointF;

{ Compute points to draw an antialiased ellipse }
function ComputeEllipse(x,y,rx,ry: single; quality: single = 1): ArrayOfTPointF; overload;
function ComputeEllipse(AOrigin, AXAxis, AYAxis: TPointF; quality: single = 1): ArrayOfTPointF; overload;
function ComputeArc65536(x, y, rx, ry: single; start65536,end65536: word; quality: single = 1): ArrayOfTPointF; overload;
function ComputeArc65536(AOrigin, AXAxis, AYAxis: TPointF; start65536,end65536: word; quality: single = 1): ArrayOfTPointF; overload;
function ComputeArcRad(x, y, rx, ry: single; startRadCCW,endRadCCW: single; quality: single = 1): ArrayOfTPointF; overload;
function ComputeArcRad(AOrigin, AXAxis, AYAxis: TPointF; startRadCCW,endRadCCW: single; quality: single = 1): ArrayOfTPointF; overload;
function ComputeArc(const arc: TArcDef; quality: single = 1): ArrayOfTPointF;
function ComputeRoundRect(x1,y1,x2,y2,rx,ry: single; quality: single = 1): ArrayOfTPointF; overload;
function ComputeRoundRect(x1,y1,x2,y2,rx,ry: single; options: TRoundRectangleOptions; quality: single = 1): ArrayOfTPointF; overload;

function Html5ArcTo(const p0, p1, p2: TPointF; radius: single): TArcDef;
function SvgArcTo(const p0: TPointF; rx, ry, xAngleRadCW: single; largeArc,
  anticlockwise: boolean; const p1: TPointF): TArcDef;
function ArcStartPoint(const arc: TArcDef): TPointF;
function ArcEndPoint(const arc: TArcDef): TPointF;
function IsLargeArc(const arc: TArcDef): boolean;

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAGradientOriginal;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRALayerOriginal, BGRABitmap, BGRABitmapTypes, BGRAGradientScanner,
  BGRASVG, BGRASVGShapes, BGRASVGType;

type
  TBGRAColorInterpolation = BGRAGradientScanner.TBGRAColorInterpolation;
  TBGRAGradientRepetition = BGRAGradientScanner.TBGRAGradientRepetition;
  TBGRALayerGradientOriginal = class;

  { TBGRAGradientOriginalDiff }

  TBGRAGradientOriginalDiff = class(TBGRAOriginalDiff)
  protected
    FStorageBefore, FStorageAfter: TBGRAMemOriginalStorage;
  public
    constructor Create(AOriginal: TBGRALayerGradientOriginal);
    procedure ComputeDifference(AOriginal: TBGRALayerGradientOriginal);
    destructor Destroy; override;
    procedure Apply(AOriginal: TBGRALayerCustomOriginal); override;
    procedure Unapply(AOriginal: TBGRALayerCustomOriginal); override;
    function CanAppend(ADiff: TBGRAOriginalDiff): boolean; override;
    procedure Append(ADiff: TBGRAOriginalDiff); override;
    function IsIdentity: boolean; override;
  end;

  { TBGRALayerGradientOriginal }

  TBGRALayerGradientOriginal = class(TBGRALayerCustomOriginal)
  private
    function GetIsOpaque: boolean;
    procedure SetColorInterpolation(AValue: TBGRAColorInterpolation);
    procedure SetEndColor(AValue: TBGRAPixel);
    procedure SetFocalPoint(AValue: TPointF);
    procedure SetFocalRadius(AValue: Single);
    procedure SetGradientType(AValue: TGradientType);
    procedure SetOrigin(AValue: TPointF);
    procedure SetRadius(AValue: Single);
    procedure SetRepetition(AValue: TBGRAGradientRepetition);
    procedure SetStartColor(AValue: TBGRAPixel);
    procedure SetXAxis(AValue: TPointF);
    procedure SetYAxis(AValue: TPointF);
  protected
    FStartColor,FEndColor: TBGRAPixel;
    FGradientType: TGradientType;
    FOrigin,FXAxis,FYAxis,FFocalPoint: TPointF;
    FOriginBackup,FXAxisBackup, FYAxisBackup: TPointF;
    FRadius,FFocalRadius: single;
    FColorInterpolation: TBGRAColorInterpolation;
    FRepetition: TBGRAGradientRepetition;
    FUpdateCount: integer;
    FUpdateDiff: TBGRAGradientOriginalDiff;
    function GetAverageColor: TBGRAPixel;
    function GetComputedRadius: single;
    function GetComputedYAxis: TPointF;
    function GetComputedFocalPoint: TPointF;
    function GetComputedFocalRadius: single;
    procedure OnMoveOrigin({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure OnMoveXAxis({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure OnMoveXAxisNeg({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure OnMoveYAxis({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure OnMoveFocalPoint({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure OnMoveFocalRadius({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure OnStartMove({%H-}ASender: TObject; {%H-}AIndex: integer; {%H-}AShift: TShiftState);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure NotifyChangeWithoutDiff;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ConvertToSVG(const AMatrix: TAffineMatrix; out AOffset: TPoint): TObject; override;
    function AddToSVGDefs(const AMatrix: TAffineMatrix; ADefs: TSVGDefine): TObject;
    function IsInfiniteSurface: boolean; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); overload; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean; ADrawMode: TDrawMode); overload;
    function CreateScanner(AMatrix: TAffineMatrix; ADraft: boolean = false): TBGRACustomScanner;
    procedure ConfigureEditor(AEditor: TBGRAOriginalEditor); override;
    function GetRenderBounds(ADestRect: TRect; {%H-}AMatrix: TAffineMatrix): TRect; override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    class function StorageClassName: RawByteString; override;
    class function CanConvertToSVG: boolean; override;
    property ComputedYAxis: TPointF read GetComputedYAxis;
    property ComputedRadius: single read GetComputedRadius;
    property ComputedFocalPoint: TPointF read GetComputedFocalPoint;
    property ComputedFocalRadius: single read GetComputedFocalRadius;
    procedure Transform(AMatrix: TAffineMatrix);
    procedure AssignExceptGeometry(AOther: TBGRALayerGradientOriginal);
    procedure FitGeometry(const ABox: TAffineBox);
    procedure SetColors(AStartColor, AEndColor: TBGRAPixel);
    procedure ApplyOpacity(AOpacity: byte);
    function Equals(Obj: TObject): boolean; override;

    property StartColor: TBGRAPixel read FStartColor write SetStartColor;
    property EndColor: TBGRAPixel read FEndColor write SetEndColor;
    property AverageColor: TBGRAPixel read GetAverageColor;
    property GradientType: TGradientType read FGradientType write SetGradientType;   //default gtLinear
    property Origin: TPointF read FOrigin write SetOrigin;
    property XAxis: TPointF read FXAxis write SetXAxis;
    property YAxis: TPointF read FYAxis write SetYAxis;
    property FocalPoint: TPointF read FFocalPoint write SetFocalPoint;     //default Origin
    property Radius: Single read FRadius write SetRadius;                  //default 1
    property FocalRadius: Single read FFocalRadius write SetFocalRadius;   //default 0
    property ColorInterpolation: TBGRAColorInterpolation read FColorInterpolation write SetColorInterpolation;
    property Repetition: TBGRAGradientRepetition read FRepetition write SetRepetition;
    property IsOpaque: boolean read GetIsOpaque;

  end;

Implementation
End.

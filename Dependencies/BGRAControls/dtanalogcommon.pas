// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Part of BGRA Controls. Made by third party.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit DTAnalogCommon;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ELSE}Types, {$ENDIF} Forms, Controls, Graphics, Dialogs,
  BCBaseCtrls, BGRABitmap, BGRABitmapTypes;

type
  TDTFillStyle = (fsnone, fsGradient{, fsTexture});

  TDTNeedleStyle = (nsLine, nsTriangle{, nsLineExt, nsTriangleExt});

  { TDTOrigin }

  TDTOrigin = packed record
    CenterPoint: TPoint;
    Radius: integer;
  end;

  { TDTPointerCapSettings }

  TDTPointerCapSettings = class(TPersistent)
  private
    FEdgeColor: TColor;
    FEdgeThickness: integer;
    FFillColor: TColor;
    FOnChange: TNotifyEvent;
    FRadius: integer;
    procedure SetEdgeColor(AValue: TColor);
    procedure SetEdgeThickness(AValue: integer);
    procedure SetFillColor(AValue: TColor);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetRadius(AValue: integer);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  published
    property EdgeColor: TColor read FEdgeColor write SetEdgeColor;
    property FillColor: TColor read FFillColor write SetFillColor;
    property Radius: integer read FRadius write SetRadius;
    property EdgeThickness: integer read FEdgeThickness write SetEdgeThickness;
  end;

  { TDTPointerSettings }

  TDTPointerSettings = class(TPersistent)
  private
    FColor: TColor;
    FLength: integer;
    FOnChange: TNotifyEvent;
    FThickness: integer;
    procedure SetColor(AValue: TColor);
    procedure SetLength(AValue: integer);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetThickness(AValue: integer);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  published
    property Color: TColor read FColor write SetColor;
    property Length: integer read FLength write SetLength;
    property Thickness: integer read FThickness write SetThickness;
  end;

  { TDTNeedleSettings }

  TDTNeedleSettings = class(TPersistent)
  private
    FCapColor: TColor;
    FCapEdgeColor: TColor;
    FCapRadius: integer;
    FNeedleColor: TColor;
    FNeedleLength: integer;
    FNeedleStyle: TDTNeedleStyle;
    FOnChange: TNotifyEvent;
    procedure SetCapColor(AValue: TColor);
    procedure SetCapEdgeColor(AValue: TColor);
    procedure SetCapRadius(AValue: integer);
    procedure SetNeedleColor(AValue: TColor);
    procedure SetNeedleLength(AValue: integer);
    procedure SetNeedleStyle(AValue: TDTNeedleStyle);
    procedure SetOnChange(AValue: TNotifyEvent);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property NeedleStyle: TDTNeedleStyle read FNeedleStyle write SetNeedleStyle;
  published
    property NeedleColor: TColor read FNeedleColor write SetNeedleColor;
    property NeedleLength: integer read FNeedleLength write SetNeedleLength;
    property CapRadius: integer read FCapRadius write SetCapRadius;
    property CapColor: TColor read FCapColor write SetCapColor;
    property CapEdgeColor: TColor read FCapEdgeColor write SetCapEdgeColor;
  end;

  { TDTScaleSettings }

  TDTScaleSettings = class(TPersistent)
  private
    FEnableScaleText: boolean;
    FMaximum: integer;
    FTesting: boolean;
    FTextFont: string;
    FTextRadius: integer;
    FTextSize: integer;
    FTickColor: TColor;
    FEnableMainTicks: boolean;
    FEnableRangeIndicator: boolean;
    FEnableSubTicks: boolean;
    FLengthMainTick: integer;
    FLengthSubTick: integer;
    FMainTickCount: integer;
    FMinimum: integer;
    FOnChange: TNotifyEvent;
    FSubTickCount: integer;
    FTextColor: TColor;
    FThicknessMainTick: integer;
    FThicknessSubTick: integer;
    FAngle: integer;
    procedure SetEnableScaleText(AValue: boolean);
    procedure SetFAngle(AValue: integer);
    procedure SetMaximum(AValue: integer);
    procedure SetTesting(AValue: boolean);
    procedure SetTextFont(AValue: string);
    procedure SetTextRadius(AValue: integer);
    procedure SetTextSize(AValue: integer);
    procedure SetTickColor(AValue: TColor);
    procedure SetEnableMainTicks(AValue: boolean);
    procedure SetEnableRangeIndicator(AValue: boolean);
    procedure SetEnableSubTicks(AValue: boolean);
    procedure SetLengthMainTick(AValue: integer);
    procedure SetLengthSubTick(AValue: integer);
    procedure SetMainTickCount(AValue: integer);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetSubTickCount(AValue: integer);
    procedure SetTextColor(AValue: TColor);
    procedure SetThicknessMainTick(AValue: integer);
    procedure SetThicknessSubTick(AValue: integer);
  protected
    property Testing: boolean read FTesting write SetTesting;
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  published
    property TickColor: TColor read FTickColor write SetTickColor;
    property TextColor: TColor read FTextColor write SetTextColor;
    property TextSize: integer read FTextSize write SetTextSize;
    property TextFont: string read FTextFont write SetTextFont;
    property EnableMainTicks: boolean read FEnableMainTicks write SetEnableMainTicks;
    property EnableSubTicks: boolean read FEnableSubTicks write SetEnableSubTicks;
    property EnableScaleText: boolean read FEnableScaleText write SetEnableScaleText;
    property Maximum: integer read FMaximum write SetMaximum;
    property MainTickCount: integer read FMainTickCount write SetMainTickCount;
    property SubTickCount: integer read FSubTickCount write SetSubTickCount;
    property LengthMainTick: integer read FLengthMainTick write SetLengthMainTick;
    property LengthSubTick: integer read FLengthSubTick write SetLengthSubTick;
    property ThicknessMainTick: integer read FThicknessMainTick write SetThicknessMainTick;
    property ThicknessSubTick: integer read FThicknessSubTick write SetThicknessSubTick;
    property TextRadius: integer read FTextRadius write SetTextRadius;
    property Angle: integer read FAngle write SetFAngle;
    property EnableRangeIndicator: boolean read FEnableRangeIndicator write SetEnableRangeIndicator;
    //property RangeMinValue: integer read FRangeMinValue write SetRangeMinValue;
    //property RangeMidValue: integer read FRangeMidValue write SetRangeMidValue;
    //property RangeMaxValue: integer read FRangeMaxValue write SetRangeMaxValue;
    //property RangeMinColor: TColor read FRangeMinColor write SetRangeMinColor;
    //property RangeMidColor: TColor read FRangeMidColor write SetRangeMidColor;
    //property RangeMaxColor: TColor read FRangeMaxColor write SetRangeMaxColor;
  end;

  { TDTFaceSettings }

  TDTFaceSettings = class(TPersistent)
  private
    FColorEnd: TColor;
    FColorFrame: TColor;
    FColorStart: TColor;
    FFillStyle: TDTFillStyle;
    FOnChange: TNotifyEvent;
    procedure SetColorEnd(AValue: TColor);
    procedure SetColorFrame(AValue: TColor);
    procedure SetColorStart(AValue: TColor);
    procedure SetFillStyle(AValue: TDTFillStyle);
    procedure SetOnChange(AValue: TNotifyEvent);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  published
    property FillStyle: TDTFillStyle read FFillStyle write SetFillStyle;
    property ColorFrame: TColor read FColorFrame write SetColorFrame;
    property ColorStart: TColor read FColorStart write SetColorStart;
    property ColorEnd: TColor read FColorEnd write SetColorEnd;
  end;

  { TDTBaseAnalogDevice }

  TDTBaseAnalogDevice = class(TBGRAGraphicCtrl)
  private
    FFaceSettings: TDTFaceSettings;
    FScaleSettings: TDTScaleSettings;
    procedure SetFaceSettings(AValue: TDTFaceSettings);
    procedure SetScaleSettings(AValue: TDTScaleSettings);
  protected
    procedure DoChange({%H-}Sender: TObject);
  public
    fGaugeBitmap: TBGRABitmap;
    FFrameBitmap: TBGRABitmap;
    FFaceBitmap: TBGRABitmap;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FaceSettings: TDTFaceSettings read FFaceSettings write SetFaceSettings;
    property ScaleSettings: TDTScaleSettings read FScaleSettings write SetScaleSettings;
    procedure Paint; override;
    procedure DrawGauge;
    procedure DrawFrame;
    procedure DrawFace;
  end;

function Initializebitmap(var Bitmap: TBGRABitmap; Width, Height: integer): TDTOrigin;

Implementation
End.

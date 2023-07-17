// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Iintially written by Circular.
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}

unit BGRAKnob;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Forms, Controls, Graphics, Dialogs,
  {$IFNDEF FPC}BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCBaseCtrls, BGRAGradients, BGRABitmap, BGRABitmapTypes;

type
  TBGRAKnobPositionType = (kptLineSquareCap, kptLineRoundCap, kptFilledCircle,
    kptHollowCircle);
  TBGRAKnobValueChangedEvent = procedure(Sender: TObject; Value: single) of object;

  { TBGRAKnob }

  TBGRAKnob = class(TBGRAGraphicCtrl)
  private
    { Private declarations }
    FPhong: TPhongShading;
    FCurveExponent: single;
    FKnobBmp: TBGRABitmap;
    FKnobColor: TColor;
    FAngularPos: single;
    FPositionColor: TColor;
    FPositionMargin: single;
    FPositionOpacity: byte;
    FPositionType: TBGRAKnobPositionType;
    FPositionWidth: single;
    FSettingAngularPos: boolean;
    FUsePhongLighting: boolean;
    FMinValue, FMaxValue: single;
    FOnKnobValueChange: TBGRAKnobValueChangedEvent;
    FStartFromBottom: boolean;
    procedure CreateKnobBmp;
    function GetLightIntensity: integer;
    function GetValue: single;
    procedure SetCurveExponent(const AValue: single);
    procedure SetLightIntensity(const AValue: integer);
    procedure SetStartFromBottom(const AValue: boolean);
    procedure SetValue(AValue: single);
    procedure SetMaxValue(AValue: single);
    procedure SetMinValue(AValue: single);
    procedure SetPositionColor(const AValue: TColor);
    procedure SetPositionMargin(AValue: single);
    procedure SetPositionOpacity(const AValue: byte);
    procedure SetPositionType(const AValue: TBGRAKnobPositionType);
    procedure SetPositionWidth(const AValue: single);
    procedure SetUsePhongLighting(const AValue: boolean);
    procedure UpdateAngularPos(X, Y: integer);
    procedure SetKnobColor(const AValue: TColor);
  protected
    { Protected declarations }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure Paint; override;
    procedure Resize; override;
    function ValueCorrection(var AValue: single): boolean; overload; virtual;
    function ValueCorrection: boolean; overload; virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    { Streaming }
    {$IFDEF FPC}
    procedure SaveToFile(AFileName: string);
    procedure LoadFromFile(AFileName: string);
    {$ENDIF}
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
  published
    { Published declarations }
    property Anchors;
    property CurveExponent: single read FCurveExponent write SetCurveExponent;
    property KnobColor: TColor read FKnobColor write SetKnobColor;
    property LightIntensity: integer read GetLightIntensity write SetLightIntensity;
    property PositionColor: TColor read FPositionColor write SetPositionColor;
    property PositionWidth: single read FPositionWidth write SetPositionWidth;
    property PositionOpacity: byte read FPositionOpacity write SetPositionOpacity;
    property PositionMargin: single read FPositionMargin write SetPositionMargin;
    property PositionType: TBGRAKnobPositionType
      read FPositionType write SetPositionType;
    property UsePhongLighting: boolean read FUsePhongLighting write SetUsePhongLighting;
    property MinValue: single read FMinValue write SetMinValue;
    property MaxValue: single read FMaxValue write SetMaxValue;
    property Value: single read GetValue write SetValue;
    property OnValueChanged: TBGRAKnobValueChangedEvent
      read FOnKnobValueChange write FOnKnobValueChange;
    property StartFromBottom: boolean read FStartFromBottom write SetStartFromBottom;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

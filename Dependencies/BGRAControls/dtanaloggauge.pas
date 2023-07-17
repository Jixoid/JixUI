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
unit DTAnalogGauge;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, Graphics, {$IFDEF FPC}LResources, {$ELSE} BGRAGraphics, {$ENDIF}Forms, Controls, Dialogs, DTAnalogCommon,
  BCBaseCtrls, BGRABitmap, BGRABitmapTypes;

type

  TDTGaugeStyle = (gsCustom, gsDark, gsLight);


  { TDTCustomAnalogGauge }

  TDTCustomAnalogGauge = class(TBGRAGraphicCtrl)
  private
    FFaceSettings: TDTFaceSettings;
    FGaugeStyle: TDTGaugeStyle;
    FNeedleSettings: TDTNeedleSettings;
    FPosition: integer;
    FResized: boolean;
    FGaugeBitmap: TBGRABitmap;
    FGaugeBodyBitmap: TBGRABitmap;
    FGaugeScaleBitmap: TBGRABitmap;
    FGaugeNeedleBitmap: TBGRABitmap;
    FScaleSettings: TDTScaleSettings;
    procedure SetFaceSettings(AValue: TDTFaceSettings);
    procedure DoChange({%H-}Sender: TObject);
    procedure SetGaugeStyle(AValue: TDTGaugeStyle);
    procedure SetNeedleSettings(AValue: TDTNeedleSettings);
    procedure SetPosition(AValue: integer);
    procedure SetScaleSettings(AValue: TDTScaleSettings);
    { Private declarations }
  protected
    { Protected declarations }
    procedure ResizeEvent({%H-}Sender: TObject);
    procedure ClearBitMap(var BitMap: TBGRABitmap);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure DrawGauge; virtual;
    procedure DrawGaugeBody; virtual;
    procedure DrawGaugeRange; virtual;
    procedure DrawGaugeFace; virtual;
    procedure DrawGaugeScale; virtual;
    procedure DrawGaugeNeedle; virtual;
  published
    { Published declarations }
    property Position: integer read FPosition write SetPosition;
    property FaceSettings: TDTFaceSettings read FFaceSettings write SetFaceSettings;
    property ScaleSettings: TDTScaleSettings read FScaleSettings write SetScaleSettings;
    property NeedleSettings: TDTNeedleSettings read FNeedleSettings write SetNeedleSettings;
    //property GaugeStyle: TDTGaugeStyle read FGaugeStyle write SetGaugeStyle;
  end;

  { TDTAnalogGauge }

  TDTAnalogGauge = class(TDTCustomAnalogGauge)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    property FaceSettings;
    property ScaleSettings;
    property NeedleSettings;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

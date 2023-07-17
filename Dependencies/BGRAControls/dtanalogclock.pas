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
unit DTAnalogClock;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF}
  Forms, Controls, Graphics, Dialogs, ExtCtrls,
  {$IFNDEF FPC}Types, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCBaseCtrls, BGRABitmap, BGRABitmapTypes, BGRAGradients;

type
  TClockStyle = (stlBlue, stlGreen, stlWhite);

  { TDTCustomAnalogClock }

  TDTCustomAnalogClock = class(TBGRAGraphicCtrl)
  private
    FClockStyle: TClockStyle;
    FBitmap: TBGRABitmap;
    FClockFace: TBGRABitmap;
    FEnabled: boolean;
    FMovingParts: TBGRABitmap;
    FTimer: TTimer;
    FResized: boolean;
    procedure SetClockStyle(AValue: TClockStyle);
    { Private declarations }
  protected
    procedure SetEnabled(AValue: boolean); override;
    { Protected declarations }
    procedure Paint; override;
    procedure DrawClock; virtual;
    procedure DrawClockFace; virtual;
    procedure DrawMovingParts; virtual;
    procedure SwitchTimer;

    procedure TimerEvent({%H-}Sender: TObject);
    procedure ResizeEvent({%H-}Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Enabled: boolean read FEnabled write SetEnabled;// default False;
  end;

  TDTAnalogClock = class(TDTCustomAnalogClock)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    //property ClockStyle;
    property Enabled;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

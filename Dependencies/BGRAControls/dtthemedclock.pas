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
unit dtthemedclock;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, ExtCtrls, {$IFDEF FPC}LResources,{$ENDIF} Forms, Controls, Graphics, Dialogs, DTAnalogCommon,
  {$IFNDEF FPC}Types, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BGRABitmap, BGRABitmapTypes;

type

  { TDTCustomThemedClock }

  TDTCustomThemedClock = class(TDTBaseAnalogDevice)
  private
    FClockFace: TBGRABitmap;
    FPointerBitmap: TBGRABitmap;
    FEnabled: boolean;
    FHoursPointerSettings: TDTPointerSettings;
    FMinutesPointerSettings: TDTPointerSettings;
    FPointerCapSettings: TDTPointerCapSettings;
    FPosition: integer;
    FSecondsPointerSettings: TDTPointerSettings;
    FTimer: TTimer;
    procedure SetHoursPointerSettings(AValue: TDTPointerSettings);
    procedure SetMinutesPointerSettings(AValue: TDTPointerSettings);
    procedure SetPointerCapSettings(AValue: TDTPointerCapSettings);
    procedure SetPosition(AValue: integer);
    procedure SetSecondsPointerSettings(AValue: TDTPointerSettings);
    { Private declarations }
  protected
    procedure SetEnabled(AValue: boolean); override;
    { Protected declarations }
    property SecondsPointerSettings: TDTPointerSettings read FSecondsPointerSettings write SetSecondsPointerSettings;
    property MinutesPointerSettings: TDTPointerSettings read FMinutesPointerSettings write SetMinutesPointerSettings;
    property HoursPointerSettings: TDTPointerSettings read FHoursPointerSettings write SetHoursPointerSettings;
    property PointerCapSettings: TDTPointerCapSettings read FPointerCapSettings write SetPointerCapSettings;
    property Position: integer read FPosition write SetPosition;
    property Enabled: boolean read FEnabled write SetEnabled;
    procedure TimerEvent({%H-}Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure DrawClock;
    procedure DrawPointers;
  end;

  TDTThemedClock = class(TDTCustomThemedClock)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Public declarations }
    property SecondsPointerSettings;
    property MinutesPointerSettings;
    property HoursPointerSettings;
    property PointerCapSettings;
    property ScaleSettings;
    property Position;
    property Enabled;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

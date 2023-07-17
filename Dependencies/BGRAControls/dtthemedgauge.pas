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
unit dtthemedgauge;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, Graphics, {$IFDEF FPC}LResources,{$ELSE} BGRAGraphics, {$ENDIF} Forms, Controls, Dialogs, DTAnalogCommon,
  BGRABitmap, BGRABitmapTypes;

type

  { TDTCustomThemedGauge }

  TDTCustomThemedGauge = class(TDTBaseAnalogDevice)
  private
    FPointerCapSettings: TDTPointerCapSettings;
    FPointerSettings: TDTPointerSettings;
    FScaleBitmap: TBGRABitmap;
    FPointerBitmap: TBGRABitmap;
    FPosition: integer;
    procedure SetPointerCapSettings(AValue: TDTPointerCapSettings);
    procedure SetPointerSettings(AValue: TDTPointerSettings);
    procedure SetPosition(AValue: integer);
    { Private declarations }
  protected
    { Protected declarations }
    property PointerSettings: TDTPointerSettings read FPointerSettings write SetPointerSettings;
    property PointerCapSettings: TDTPointerCapSettings read FPointerCapSettings write SetPointerCapSettings;
    property Position: integer read FPosition write SetPosition;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure DrawScale;
    procedure DrawPointer;
  end;

  { TDTThemedGauge }

  TDTThemedGauge = class(TDTCustomThemedGauge)
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
    property PointerSettings;
    property PointerCapSettings;
    property Position;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

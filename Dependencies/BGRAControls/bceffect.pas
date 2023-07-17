// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Created by BGRA Controls Team
  Dibo, Circular, lainz (007) and contributors.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCEffect;

{$I bgracontrols.map.inc}
{$IFDEF FPC}
{$modeswitch advancedrecords}
{$ENDIF}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LCLProc, LazUTF8, {$ELSE}Types, BGRAGraphics, GraphType, FPImage, {$ENDIF} BGRABitmapTypes;

{-- Fading --}

type
  TFadingMode = (fmSuspended, fmFadeIn, fmFadeOut, fmFadeInCycle, fmFadeOutCycle, fmFadeInOut, fmFadeOutIn);

const
  FadingModeStr: array[TFadingMode] of string = ('Suspended', 'Fade In', 'Fade Out', 'Fade In Cycle','Fade Out Cycle', 'Fade In Out', 'Fade Out In');

function StrToTFadingMode(const s: ansistring): TFadingMode;
procedure FadingModeStrList(s: TStrings);

type

  { TFading }

  TFading = record
  private
    FAlpha: byte;
    FMode: TFadingMode;
    FAlphaStep: byte;
    FDuration: integer;
    FPrevDate: TDateTime;
    FElapsedMsAccumulator: integer;
  public
    procedure SetFAlpha(AValue: byte);
    procedure SetFMode(AValue: TFadingMode);
    procedure SetFAlphaStep(AValue: byte);
    procedure SetFDuration(AValue: integer);
  public
    function Execute(AStepCount: integer= 1): byte; // execute and return new alpha
    function Reset: byte;   // reset and return new alpha
    procedure PutImage(ADestination: TBGRACustomBitmap; AX,AY: integer; ASource: TBGRACustomBitmap);
    procedure FillRect(ADestination: TBGRACustomBitmap; ARect: TRect; AColor: TBGRAPixel);
  public
    property Alpha: byte read FAlpha write SetFAlpha;
    property Mode: TFadingMode read FMode write SetFMode;
    property Step: byte read FAlphaStep write SetFAlphaStep;
    property Duration: integer read FDuration write SetFDuration;
  end;

{-- Fading --}

Implementation
End.

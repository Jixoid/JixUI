// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ This component partialy solve problem with no alpha in lazarus GTK.
  It is using BGRABitmap library for drawing icons.

  originally written in 2011 by Krzysztof Dibowski dibowski at interia.pl
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BGRASpeedButton;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Forms, Controls, Graphics, Dialogs, Buttons, BGRABitmap,
  {$IFNDEF FPC}Types, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BGRABitmapTypes;

{$IFDEF LCLgtk}
  {$DEFINE BGRA_DRAW}
{$ELSE}
  {$IFDEF LCLgtk2}
    {$DEFINE BGRA_DRAW}
  {$ENDIF}
{$ENDIF}

type

  { TBGRASpeedButton }

  TBGRASpeedButton = class(TSpeedButton)
  private
    { Private declarations }
    {$IFDEF BGRA_DRAW}
    FBGRA: TBGRABitmap;
    {$ENDIF}
  protected
    { Protected declarations }
    {$IFDEF BGRA_DRAW}
    function DrawGlyph(ACanvas: TCanvas; const AClient: TRect;
      const AOffset: TPoint; AState: TButtonState; {%H-}ATransparent: boolean;
      {%H-}BiDiFlags: longint): TRect; override;
    {$ENDIF}
  public
    { Public declarations }
    {$IFDEF BGRA_DRAW}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$ENDIF}
  published
    { Published declarations }
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

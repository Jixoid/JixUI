// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Created by Fox. Part of BGRA Controls.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BGRAResizeSpeedButton;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, Buttons, {$IFDEF FPC}LResources,{$ENDIF} Forms,
  Controls, Graphics, Dialogs,
  {$IFNDEF FPC}Types, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BGRASpeedButton, BGRABitmap;

type
  TBGRAResizeSpeedButton = class(TBGRASpeedButton)
  private
    { Private declarations }
    FBGRA: TBGRABitmap;
  protected
    { Protected declarations }
    function DrawGlyph(ACanvas: TCanvas; const AClient: TRect;
      const {%H-}AOffset: TPoint; AState: TButtonState; {%H-}ATransparent: boolean;
      {%H-}BiDiFlags: longint): TRect; {$IFDEF FPC}override;{$ENDIF}
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

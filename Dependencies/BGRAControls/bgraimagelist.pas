// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ This component partialy solve problem with no alpha in lazarus GTK.
  It is using BGRABitmap library for drawing icons.

  originally written in 2011 by Krzysztof Dibowski dibowski at interia.pl
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BGRAImageList;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Forms, Controls, Graphics, Dialogs,
  GraphType, BGRABitmap, BGRABitmapTypes, {%H-}ImgList;

{$IFDEF LCLgtk}
  { $DEFINE BGRA_DRAW}
{$ELSE}
  {$IFDEF LCLgtk2}
    { $DEFINE BGRA_DRAW}
  {$ENDIF}
{$ENDIF}

type

  { TBGRAImageList }

  TBGRAImageList = class(TImageList)
  private
    { Private declarations }
    {$IFDEF BGRA_DRAW}
    FBGRA: TBGRABitmap;
    FBmp:  TBitmap;
    {$ENDIF}
  protected
    { Protected declarations }
  public
    { Public declarations }
    {$IFDEF BGRA_DRAW}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Draw(ACanvas: TCanvas; AX, AY, AIndex: integer;
      ADrawingStyle: TDrawingStyle; AImageType: TImageType;
      ADrawEffect: TGraphicsDrawEffect); override;
    {$ENDIF}
  published
    { Published declarations }
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

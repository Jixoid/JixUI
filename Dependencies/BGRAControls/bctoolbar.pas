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
unit BCToolBar;

{$I bgracontrols.map.inc}

interface

uses
  Classes, {$IFDEF FPC}LResources,{$ELSE}types, BGRAGraphics, GraphType, FPImage,{$ENDIF}
  Forms, Controls, Graphics, Dialogs, ComCtrls,
  BGRABitmap, BGRABitmapTypes, BGRAGradients, BCTypes;

type

  { TBCToolBar }

  TBCToolBar = class(TToolBar)
  private
    FLimitMemoryUsage: boolean;
    { Private declarations }
    FOnRedraw: TBGRARedrawEvent;
    FBGRA: TBGRABitmap;
    procedure SetLimitMemoryUsage(AValue: boolean);
  protected
    { Protected declarations }
   {$IFDEF FPC}
   procedure Paint; override;
   {$ELSE}
   procedure Paint; virtual;
   procedure PaintWindow(DC: HDC); override;
   {$ENDIF}
    procedure CheckMemoryUsage; virtual;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property OnRedraw: TBGRARedrawEvent read FOnRedraw write FOnRedraw;
    property LimitMemoryUsage: boolean read FLimitMemoryUsage write SetLimitMemoryUsage;
  end;

procedure DrawWindows7ToolBar(Bitmap: TBGRABitmap; AColor: TColor = clDefault);

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

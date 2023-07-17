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
unit BCGameGrid;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources, LCLProc,{$ENDIF} Types, Forms, Controls, Graphics, Dialogs,
  {$IFNDEF FPC}BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCBaseCtrls, BGRABitmap, BGRABitmapTypes;

type

  TOnRenderControl = procedure(Sender: TObject; Bitmap: TBGRABitmap;
    r: TRect; n, x, y: integer) of object;
  TOnClickControl = procedure(Sender: TObject; n, x, y: integer) of object;

  { TBCCustomGrid }

  TBCCustomGrid = class(TBCGraphicControl)
  private
    FBGRA: TBGRABitmap;
    FGridWidth: integer;
    FGridHeight: integer;
    FBlockWidth: integer;
    FBlockHeight: integer;
    FOnRenderControl: TOnRenderControl;
    FOnClickControl: TOnClickControl;
  private
    procedure SetFBlockHeight(AValue: integer);
    procedure SetFBlockWidth(AValue: integer);
    procedure SetFGridHeight(AValue: integer);
    procedure SetFGridWidth(AValue: integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Click; override;
    procedure DrawControl; override;
    procedure RenderControl; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RenderAndDrawControl;
    property GridWidth: integer read FGridWidth write SetFGridWidth;
    property GridHeight: integer read FGridHeight write SetFGridHeight;
    property BlockWidth: integer read FBlockWidth write SetFBlockWidth;
    property BlockHeight: integer read FBlockHeight write SetFBlockHeight;
    property OnRenderControl: TOnRenderControl
      read FOnRenderControl write FOnRenderControl;
    property OnClickControl: TOnClickControl read FOnClickControl write FOnClickControl;
  published
    { Published declarations }
  end;

  TBCGameGrid = class(TBCCustomGrid)
  published
    property GridWidth;
    property GridHeight;
    property BlockWidth;
    property BlockHeight;
    // Support 'n, x, y'
    property OnRenderControl;
    property OnClickControl;
    // 'Classic' events, to be changed...
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    // Ok...
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCListBox;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LCLType, LResources, {$ENDIF}
  Forms, Controls, Graphics, Dialogs, StdCtrls,
  {$IFNDEF FPC}Types, BGRAGraphics, GraphType, FPImage, BCBaseCtrls,{$ENDIF}
  BGRAVirtualScreen, BGRABitmap, BGRASliceScaling;

type

  TBCListBox = class;
  TBCPaperPanel = class;

  { TBCPaperPanel }

  TBCPaperPanel = class(TBGRAVirtualScreen)
  private
    FShadow: TBGRASliceScaling;
    procedure LoadShadowFromBitmapResource;
  protected
    procedure BCRedraw(Sender: TObject; ABitmap: TBGRABitmap);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  { TBCListBox }

  TBCListBox = class(TListBox)
  private
    { Private declarations }
  protected
    procedure BCDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
  published
    { Published declarations }
  end;

  { TBCPaperListBox }

  TBCPaperListBox = class(TBCPaperPanel)
  private
    FListBox: TBCListBox;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property ListBox: TBCListBox read FListBox write FListBox;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

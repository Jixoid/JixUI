// SPDX-License-Identifier: LGPL-3.0-linking-exception
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCDefaultThemeManager;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources, CustomDrawnDrawers,{$ENDIF}
  Forms, Controls, Graphics, Dialogs,
  {$IFNDEF FPC}BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCButton, BCButtonFocus, BCNumericKeyboard, BCThemeManager,
  BCSamples, {$IFDEF FPC}BGRACustomDrawn,{$ENDIF} BCKeyboard;

type

  { TBCDefaultThemeManager }

  TBCDefaultThemeManager = class(TBCThemeManager)
  private
    FBCStyle: TBCSampleStyle;
    FButton: TBCButton;
    FButtonFocus: TBCButtonFocus;
    FCDStyle: TCDDrawStyle;
    procedure SetFBCStyle(AValue: TBCSampleStyle);
    procedure SetFButton(AValue: TBCButton);
    procedure SetFButtonFocus(AValue: TBCButtonFocus);
    procedure SetFCDStyle(AValue: TCDDrawStyle);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Apply(AControl: TWinControl); override;
    procedure Apply(); override;
  published
    property Button: TBCButton read FButton write SetFButton;
    property ButtonFocus: TBCButtonFocus read FButtonFocus write SetFButtonFocus;
    property BCStyle: TBCSampleStyle read FBCStyle write SetFBCStyle;
    property CDStyle: TCDDrawStyle read FCDStyle write SetFCDStyle;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

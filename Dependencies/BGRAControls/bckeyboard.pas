// SPDX-License-Identifier: LGPL-3.0-linking-exception
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCKeyboard;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LCLType, LResources, LMessages,{$ENDIF}Forms, Controls, Graphics, Dialogs,
  {$IFNDEF FPC}Types, Windows, Messages, BGRAGraphics, GraphType, FPImage, BCBaseCtrls,{$ENDIF}
  BCThemeManager, BCButton, BCPanel, MouseAndKeyInput;

type

  { TBCKeyboard }

  TBCKeyboard = class(TComponent)
  private
    FBCThemeManager: TBCThemeManager;
    FButton: TBCButton;
    FOnUserChange: TNotifyEvent;
    FPanel, FRow1, FRow2, FRow3, FRow4: TBCPanel;
    FPanelsColor: TColor;
    F_q, F_w, F_e, F_r, F_t, F_y, F_u, F_i, F_o, F_p, F_a, F_s, F_d,
    F_f, F_g, F_h, F_j, F_k, F_l, F_z, F_x, F_c, F_v, F_b, F_n, F_m,
    F_shift, F_space, F_back: TBCButton;
    FVisible: boolean;
    procedure SetFButton(AValue: TBCButton);
    procedure SetFPanel(AValue: TBCPanel);
    procedure SetFPanelsColor(AValue: TColor);
    procedure SetFThemeManager(AValue: TBCThemeManager);
  protected
    procedure PressVirtKey(p: longint);
    procedure PressShiftVirtKey(p: longint);
    procedure OnButtonClick(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer); virtual;
    { When value is changed by the user }
    property OnUserChange: TNotifyEvent read FOnUserChange write FOnUserChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Show in a custom form or panel
    procedure Show(AControl: TWinControl); overload;
    // Try to Show in the form where this component is placed
    procedure Show(); overload;
    // Hide the component
    procedure Hide();
    // Update buttons style
    procedure UpdateButtonStyle;
  public
    { The real panel that's used as container for all the numeric buttons }
    property Panel: TBCPanel read FPanel write SetFPanel;
    { The color of all the panels involved in the control }
    property PanelsColor: TColor read FPanelsColor write SetFPanelsColor;
    { A fake button that's used as style base for all the numeric buttons }
    property ButtonStyle: TBCButton read FButton write SetFButton;
    { If it's visible or not }
    property Visible: boolean read FVisible;
  published
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFThemeManager;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

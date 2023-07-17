// SPDX-License-Identifier: LGPL-3.0-linking-exception
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCNumericKeyboard;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LCLType, LResources, LMessages,{$ENDIF}
  Forms, Controls, Graphics, Dialogs, MouseAndKeyInput,
  {$IFNDEF FPC}Types, Windows, BGRAGraphics, GraphType, FPImage, BCBaseCtrls, {$ENDIF}
  BCPanel, BCButton, BCThemeManager;

type

  { TBCCustomNumericKeyboard }

  TBCCustomNumericKeyboard = class(TComponent)
  private
    FBCThemeManager: TBCThemeManager;
    procedure SetFThemeManager(AValue: TBCThemeManager);
  protected
    FOnChange: TNotifyEvent;
    FOnUserChange: TNotifyEvent;
    FPanel: TBCPanel;
    FButton: TBCButton;
    FBtn0, FBtn1, FBtn2, FBtn3, FBtn4, FBtn5, FBtn6, FBtn7, FBtn8,
    FBtn9, FBtnDot, FBtnClr: TBCButton;
    FValue: string;
    FVisible: boolean;
    procedure SetFButton(AValue: TBCButton);
    procedure SetFPanel(AValue: TBCPanel);
    procedure SetFValue(AValue: string);
  protected
    procedure OnButtonClick(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer); virtual;
  protected
    { The input value }
    property Value: string read FValue write SetFValue;
    { When value is changed by code or by the user }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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
    { A fake button that's used as style base for all the numeric buttons }
    property ButtonStyle: TBCButton read FButton write SetFButton;
    { If it's visible or not }
    property Visible: boolean read FVisible;
  published
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFThemeManager;
  end;

  TBCNumericKeyboard = class(TBCCustomNumericKeyboard)
  published
    property Value;
    property OnChange;
    property OnUserChange;
    property ThemeManager;
  end;

  { TBCRealNumericKeyboard }

  TBCRealNumericKeyboard = class(TBCCustomNumericKeyboard)
  protected
    procedure OnButtonClick(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer); override;
    procedure PressVirtKey(p: longint);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OnUserChange;
    property ThemeManager;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

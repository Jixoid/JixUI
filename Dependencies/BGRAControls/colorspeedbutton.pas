// SPDX-License-Identifier: LGPL-3.0-linking-exception
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit ColorSpeedButton;

{$I bgracontrols.map.inc}

{$ifdef windows}
{$define overridepaint}
{$endif}

interface

uses
  Classes, SysUtils, Types, {$IFDEF FPC}LCLType, LCLProc, LResources,{$ENDIF}
  {$IFNDEF FPC}BGRAGraphics, GraphType, FPImage, {$ENDIF}
  Forms, Controls, Graphics, Dialogs, Buttons, BGRASpeedButton, Themes
  {$ifdef overridepaint}, Math{$ENDIF};

type

  { TColorState }

  TColorState = class(TPersistent)
  private
    FOwner: TControl;
    FBorderColor: TColor;
    FBorderWidth: integer;
    FColor: TColor;
    procedure SetFBorderColor(AValue: TColor);
    procedure SetFBorderWidth(AValue: integer);
    procedure SetFColor(AValue: TColor);
  public
    constructor Create(AOwner: TControl);
  published
    property Color: TColor read FColor write SetFColor;
    property BorderColor: TColor read FBorderColor write SetFBorderColor;
    property BorderWidth: integer read FBorderWidth write SetFBorderWidth;
  end;

  { TColorSpeedButton }

  TColorSpeedButton = class(TBGRASpeedButton)
  private
    {$ifdef overridepaint}
    FLastDrawDetails: TThemedElementDetails;
    {$endif}
    FPopupMode: boolean;
    FPressed: boolean;
    FStateActive: TColorState;
    FStateDisabled: TColorState;
    FStateHover: TColorState;
    FStateNormal: TColorState;
    FTextAutoSize: boolean;
    FToggle: boolean;
    procedure SetFPopupMode(AValue: boolean);
    procedure SetFPressed(AValue: boolean);
    procedure SetFStateActive(AValue: TColorState);
    procedure SetFStateDisabled(AValue: TColorState);
    procedure SetFStateHover(AValue: TColorState);
    procedure SetFStateNormal(AValue: TColorState);
    procedure SetFTextAutoSize(AValue: boolean);
    procedure SetFToggle(AValue: boolean);
  protected
    {$ifdef overridepaint}
    procedure DrawText({%H-}ACanvas: TPersistent; {%H-}Details: TThemedElementDetails;
      const S: string; R: TRect; Flags, {%H-}Flags2: cardinal);
    procedure MeasureDraw(Draw: boolean; PaintRect: TRect;
      out PreferredWidth, PreferredHeight: integer);
    procedure Paint; override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      {%H-}WithThemeSpace: boolean); override;
    {$endif}
    procedure PaintBackground(var PaintRect: TRect); {$IFDEF FPC}override;{$ENDIF}
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property TextAutoSize: boolean read FTextAutoSize write SetFTextAutoSize;
    property Toggle: boolean read FToggle write SetFToggle;
    property Pressed: boolean read FPressed write SetFPressed;
    property PopupMode: boolean read FPopupMode write SetFPopupMode;
    property StateNormal: TColorState read FStateNormal write SetFStateNormal;
    property StateHover: TColorState read FStateHover write SetFStateHover;
    property StateActive: TColorState read FStateActive write SetFStateActive;
    property StateDisabled: TColorState read FStateDisabled write SetFStateDisabled;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

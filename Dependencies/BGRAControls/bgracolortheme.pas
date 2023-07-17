// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAColorTheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, BGRATheme,
  BGRABitmap, BGRABitmapTypes, BGRASVGImageList;

type

  { TBGRAColorTheme }

  TBGRAColorTheme = class(TBGRATheme)
  private
    FColorActive: TColor;
    FColorDisabled: TColor;
    FColorFocused: TColor;
    FColorHover: TColor;
    FColorNormal: TColor;
    FColorText: TColor;
    procedure SetFColorActive(AValue: TColor);
    procedure SetFColorDisabled(AValue: TColor);
    procedure SetFColorFocused(AValue: TColor);
    procedure SetFColorHover(AValue: TColor);
    procedure SetFColorNormal(AValue: TColor);
    procedure SetFColorText(AValue: TColor);

  protected

  public
    procedure DrawButton(Caption: string; State: TBGRAThemeButtonState;
      Focused: boolean; ARect: TRect; ASurface: TBGRAThemeSurface; AImageIndex: Integer = -1; AImageList: TBGRASVGImageList = nil); override;
    procedure DrawRadioButton(Caption: string; State: TBGRAThemeButtonState;
      {%H-}Focused: boolean; Checked: boolean; ARect: TRect; ASurface: TBGRAThemeSurface); override;
    procedure DrawCheckBox(Caption: string; State: TBGRAThemeButtonState;
      {%H-}Focused: boolean; Checked: boolean; ARect: TRect; ASurface: TBGRAThemeSurface); override;
  published
    property ColorNormal: TColor read FColorNormal write SetFColorNormal;
    property ColorHover: TColor read FColorHover write SetFColorHover;
    property ColorActive: TColor read FColorActive write SetFColorActive;
    property ColorDisabled: TColor read FColorDisabled write SetFColorDisabled;
    property ColorFocused: TColor read FColorFocused write SetFColorFocused;
    property ColorText: TColor read FColorText write SetFColorText;
  end;

procedure Register;

Implementation
End.

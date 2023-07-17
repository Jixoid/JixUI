// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAImageTheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, BGRATheme,
  BGRASliceScaling, BGRABitmap, BGRABitmapTypes, BGRASVGImageList;

type

  { TBGRAImageTheme }

  TBGRAImageTheme = class(TBGRATheme)
  private
    FBackgroundColor: TColor;
    FSliceScalingButton: TBGRAMultiSliceScaling;
    procedure SetFBackgroundColor(AValue: TColor);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadResources(AFileName: string);
    procedure DrawButton(Caption: string; State: TBGRAThemeButtonState;
      Focused: boolean; ARect: TRect; ASurface: TBGRAThemeSurface; AImageIndex: Integer = -1; AImageList: TBGRASVGImageList = nil); override;
  published
    property BackgroundColor: TColor read FBackgroundColor
      write SetFBackgroundColor default clForm;
  end;

procedure Register;

Implementation
End.

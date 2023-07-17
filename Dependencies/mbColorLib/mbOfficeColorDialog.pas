unit mbOfficeColorDialog;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType,
  SysUtils, Classes, Graphics, Forms, OfficeMoreColorsDialog, JCL.Controls;

type
  TmbOfficeColorDialog = class(TJixDialog)
  private
    FWin: TOfficeMoreColorsWin;
    FSelColor: TColor;
    FUseHint: boolean;
    FMaxHue, FMaxSat, FMaxLum: Integer;
    FPickerIndex: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: boolean; overload;
    function Execute(AColor: TColor): boolean; overload;
  published
    property SelectedColor: TColor read FSelColor write FSelColor default clWhite;
    property MaxHue: Integer read FMaxHue write FMaxHue default 360;
    property MaxSaturation: Integer read FMaxSat write FMaxSat default 255;
    property MaxLuminance: Integer read FMaxLum write FMaxLum default 255;
    property UseHints: boolean read FUseHint write FUseHint default false;
  end;

Implementation
End.

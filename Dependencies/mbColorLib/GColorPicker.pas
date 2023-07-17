unit GColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms,
  HTMLColors, mbTrackBarPicker;

type
  TGColorPicker = class(TmbTrackBarPicker)
  private
    FRed, FGreen, FBlue: integer;
    function ArrowPosFromGreen(g: integer): integer;
    function GreenFromArrowPos(p: integer): integer;
    procedure SetBlue(b: integer);
    procedure SetGreen(g: integer);
    procedure SetRed(r: integer);
  protected
    procedure Execute(tbaAction: integer); override;
    function GetArrowPos: integer; override;
    function GetGradientColor(AValue: Integer): TColor; override;
    function GetSelectedColor: TColor; override;
    function GetSelectedValue: integer; override;
    procedure SetSelectedColor(c: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Red: integer read FRed write SetRed default 128;
    property Green: integer read FGreen write SetGreen default 255;
    property Blue: integer read FBlue write SetBlue default 128;
    property SelectedColor default clRed;
    property Layout default lyVertical;
    property HintFormat;
  end;


Implementation
End.

unit RColorPicker;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms,
  HTMLColors, mbTrackBarPicker;

type

  { TRColorPicker }

  TRColorPicker = class(TmbTrackBarPicker)
  private
    FRed, FGreen, FBlue: integer;
    function ArrowPosFromRed(r: integer): integer;
    function RedFromArrowPos(p: integer): integer;
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
    property Red: integer read FRed write SetRed default 255;
    property Green: integer read FGreen write SetGreen default 128;
    property Blue: integer read FBlue write SetBlue default 128;
    property SelectedColor default clRed;
    property Layout default lyVertical;
    property HintFormat;
  end;


Implementation
End.

unit BColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType,
  SysUtils, Classes, Controls, Graphics, Forms,
  HTMLColors, mbTrackBarPicker;

type

  { TBColorPicker }

  TBColorPicker = class(TmbTrackBarPicker)
  private
    FRed, FGreen, FBlue: integer;
    function ArrowPosFromBlue(b: integer): integer;
    function BlueFromArrowPos(p: integer): integer;
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
    property Blue: integer read FBlue write SetBlue default 255;
    property Green: integer read FGreen write SetGreen default 128;
    property Red: integer read FRed write SetRed default 128;
    property SelectedColor default clRed;
    property Layout default lyVertical;
    property HintFormat;
  end;


Implementation
End.

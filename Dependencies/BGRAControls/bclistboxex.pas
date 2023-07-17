unit BCListBoxEx;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  LCLType;

type
  TBCListBoxEx = class(TCustomControl)
  private
    mousepos: TPoint;
    scrolly: integer;
    fitems: TStringList;
    itemselected: integer;
    itemheight: integer;
    lastitem: integer;
    invalidatecount: integer;
    scrollwidth: integer;
    function GetItemRect(index: integer): TRect;
    function GetItemVertically(y: integer): integer;
    procedure ScrollToItemTop();
    procedure ScrollToItemBottom();
    procedure ScrollToItem(index: integer);
    function ItemIsVisible(index: integer): boolean;
  protected
    procedure Click; override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): boolean; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Items: TStringList read Fitems;
  end;

procedure Register;

Implementation
End.

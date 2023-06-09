unit UnitDialogTime;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, BCPanel,
  BCLabel, BCButton, windows, MiniJixoid_JCL, JixPas, JForms;

type

  { TFormDialogTime }

  TFormDialogTime = class(jJixForm)
    BCButMAdd: TBCButton;
    BCButSAdd: TBCButton;
    BCButMMinus: TBCButton;
    BCButSMinus: TBCButton;
    BCButOK: TBCButton;
    Edit1: TEdit;
    Edit2: TEdit;
    procedure BCButHaltClick(Sender: TObject);
    procedure BCButMAddClick(Sender: TObject);
    procedure BCButMAddMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure BCButMMinusClick(Sender: TObject);
    procedure BCButMMinusMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure BCButOKClick(Sender: TObject);
    procedure BCButOKMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure BCButSAddClick(Sender: TObject);
    procedure BCButSAddMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure BCButSMinusClick(Sender: TObject);
    procedure BCButSMinusMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure BCLabelCaptionMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OnChangeTheme(Sender: TObject; ThemeColor: jARGB; ThemeDark: Boolean;
      FontColor: jARGB);
  private

  public

  end;

var
  FormDialogTime: TFormDialogTime;
  Used: Boolean;

implementation
Uses
  JDialogs;

{$R *.lfm}

{ TFormDialogTime }

procedure TFormDialogTime.OnChangeTheme(Sender: TObject; ThemeColor: jARGB;
  ThemeDark: Boolean; FontColor: jARGB);
Begin
  //RePaintComponent(BCButMAdd);
  //RePaintComponent(BCButMMinus);
  //RePaintComponent(BCButSAdd);
  //RePaintComponent(BCButSMinus);
  //RePaintComponent(BCButOK);
end;

Var
  CurX, CurY: Integer;

procedure TFormDialogTime.BCLabelCaptionMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  Begin
    Self.Left:=Mouse.CursorPos.X-CurX;
    Self.Top :=Mouse.CursorPos.Y-CurY;
  end else
  Begin
    CurX:=Mouse.CursorPos.X-Self.Left;
    CurY:=Mouse.CursorPos.Y-Self.Top;
  end;
end;

procedure TFormDialogTime.Edit1Change(Sender: TObject);
begin
  if not((Edit1.Text = '') or (Edit2.Text = '')) then
  Begin
    if (StrToInt(Edit1.Text) > 23) then
      Edit1.Text:= '23'
    else if (StrToInt(Edit1.Text) < 0) then
      Edit1.Text:= '0';
  end;
end;

procedure TFormDialogTime.Edit2Change(Sender: TObject);
begin
  if not((Edit1.Text = '') or (Edit2.Text = '')) then
  Begin
    if (StrToInt(Edit2.Text) > 59) then
      Edit2.Text:= '59'
    else if (StrToInt(Edit2.Text) < 0) then
      Edit2.Text:= '0';
  End
end;

procedure TFormDialogTime.FormCreate(Sender: TObject);
begin
  WhenChangeTheme:= @OnChangeTheme;
end;

procedure TFormDialogTime.FormShow(Sender: TObject);
Var
  H, W: Int16U;
begin
  H:= Round(Scaling(Self, 200));
  W:= Round(Scaling(Self, 130));

  Top:= Top-10;
  MoveWindow(Self.Handle,
    Monitor.Left+Round((Monitor.Width-W)/2),
    Monitor.Top+Round((Monitor.Height-H)/2),
    W,H, True
  );

  Used:= False;
  Edit1.Text:= FormatDateTime('H', Time);
  Edit2.Text:= FormatDateTime('M', Time);
end;

procedure TFormDialogTime.BCButHaltClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TFormDialogTime.BCButMAddClick(Sender: TObject);
begin
  Edit1.Text:= (StrToInt(Edit1.Text)+1).ToString;
end;

procedure TFormDialogTime.BCButMAddMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  ButtonRadialEffect(BCButMAdd, X,Y);
end;

procedure TFormDialogTime.BCButMMinusClick(Sender: TObject);
begin
  Edit1.Text:= (StrToInt(Edit1.Text)-1).ToString;
end;

procedure TFormDialogTime.BCButMMinusMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  ButtonRadialEffect(BCButMMinus, X,Y);
end;

procedure TFormDialogTime.BCButOKClick(Sender: TObject);
begin
  if not((Edit1.Text = '') or (Edit2.Text = '')) then
  Begin
    Used:= True;
    Self.Close;
  end else
    CustomErrorSend('Please fill in the blanks', '', CustomErrorFlag.Soft);
end;

procedure TFormDialogTime.BCButOKMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  ButtonRadialEffect(BCButOK, X,Y);
end;

procedure TFormDialogTime.BCButSAddClick(Sender: TObject);
begin
  Edit2.Text:= (StrToInt(Edit2.Text)+1).ToString;
end;

procedure TFormDialogTime.BCButSAddMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  ButtonRadialEffect(BCButSAdd, X,Y);
end;

procedure TFormDialogTime.BCButSMinusClick(Sender: TObject);
begin
  Edit2.Text:= (StrToInt(Edit2.Text)-1).ToString;
end;

procedure TFormDialogTime.BCButSMinusMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  ButtonRadialEffect(BCButSMinus, X,Y);
end;

end.


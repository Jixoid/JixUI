unit UnitDialogDate;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, BCPanel,
  BCLabel, BCButton, BCComboBox, windows, MiniJixoid_JCL, JixAPI, JixPas, JForms;

type

  { TFormDialogDate }

  TFormDialogDate = class(jJixForm)
    BCButMAdd: TBCButton;
    BCButSAdd: TBCButton;
    BCButMMinus: TBCButton;
    BCButSMinus: TBCButton;
    BCButOK: TBCButton;
    BCComboMount: TBCComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    procedure BCButMAddClick(Sender: TObject);
    procedure BCButMAddMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure BCButMMinusClick(Sender: TObject);
    procedure BCButOKClick(Sender: TObject);
    procedure BCButSAddClick(Sender: TObject);
    procedure BCButSMinusClick(Sender: TObject);
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
  FormDialogDate: TFormDialogDate;
  Used: Boolean;

implementation
Uses
  JDialogs;

{$R *.lfm}

Function GetDayCount(Year: Int16U; Month: Int8U): Int8U;
Var
  LeapYear: Boolean;
Begin
  LeapYear := ((Year mod 4 = 0) and (Year mod 100 <> 0)) or (Year mod 400 = 0);
  Case Month Of
    1, 3, 5, 7, 8, 10, 12: Result := 31;
    4, 6, 9, 11: Result := 30;
    2: if LeapYear then Result := 29 else Result := 28;
  Else
    Result := 0;
  End;
End;

{ TFormDialogDate }

procedure TFormDialogDate.OnChangeTheme(Sender: TObject; ThemeColor: jARGB;
  ThemeDark: Boolean; FontColor: jARGB);
Begin
  //RePaintComponent(BCButMAdd);
  //RePaintComponent(BCButMMinus);
  //RePaintComponent(BCButSAdd);
  //RePaintComponent(BCButSMinus);
  //RePaintComponent(BCButOK);
  //RePaintComponent(BCComboMount);
end;

Var
  CurX, CurY: Integer;

procedure TFormDialogDate.Edit1Change(Sender: TObject);
begin
  if not((Edit1.Text = '') or (Edit2.Text = '')) then
  Begin
    if (StrToInt(Edit1.Text) > GetDayCount(StrToInt(Edit2.Text), BCComboMount.ItemIndex+1)) then
      Edit1.Text:= GetDayCount(StrToInt(Edit2.Text), BCComboMount.ItemIndex+1).ToString
    else if (StrToInt(Edit1.Text) < 0) then
      Edit1.Text:= '0';
  end;
end;

procedure TFormDialogDate.Edit2Change(Sender: TObject);
begin
  if not((Edit1.Text = '') or (Edit2.Text = '')) then
  Begin
    if (StrToInt(Edit2.Text) > 9999) then
      Edit2.Text:= '9999'
    else if (StrToInt(Edit2.Text) < 0) then
      Edit2.Text:= '0';
  End
end;

procedure TFormDialogDate.FormCreate(Sender: TObject);
begin
  WhenChangeTheme:= @OnChangeTheme;
end;

procedure TFormDialogDate.FormShow(Sender: TObject);
Var
  H, W: Int16U;
begin
  H:= Round(Scaling(Self, 200));
  W:= Round(Scaling(Self, 240));

  Top:= Top-10;
  MoveWindow(Self.Handle,
    Monitor.Left+Round((Monitor.Width-W)/2),
    Monitor.Top+Round((Monitor.Height-H)/2),
    W,H, True
  );

  Used:= False;
  Edit1.Text:= FormatDateTime('D', Date);
  Edit2.Text:= FormatDateTime('YYYY', Date); 
  BCComboMount.ItemIndex:= StrToInt(FormatDateTime('M', Date))-1;
end;

procedure TFormDialogDate.BCButMAddClick(Sender: TObject);
begin
  Edit1.Text:= (StrToInt(Edit1.Text)+1).ToString;
end;

procedure TFormDialogDate.BCButMAddMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  ButtonRadialEffect(TBCButton(Sender), X,Y);
end;

procedure TFormDialogDate.BCButMMinusClick(Sender: TObject);
begin
  Edit1.Text:= (StrToInt(Edit1.Text)-1).ToString;
end;

procedure TFormDialogDate.BCButOKClick(Sender: TObject);
begin
  if not((Edit1.Text = '') or (Edit2.Text = '')) then
  Begin
    Used:= True;
    Self.Close;
  end else
    CustomErrorSend('Please fill in the blanks', '', CustomErrorFlag.Soft);
end;

procedure TFormDialogDate.BCButSAddClick(Sender: TObject);
begin
  Edit2.Text:= (StrToInt(Edit2.Text)+1).ToString;
end;

procedure TFormDialogDate.BCButSMinusClick(Sender: TObject);
begin
  Edit2.Text:= (StrToInt(Edit2.Text)-1).ToString;
end;

end.


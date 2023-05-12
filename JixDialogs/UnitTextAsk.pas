unit UnitTextAsk;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  BCPanel, BCButton, BCLabel, MiniJixoid_JCL, JixAPI, Windows, JForms, JixPas;

type

  { TFormTextAsk }

  TFormTextAsk = class(jJixForm)
    BCPanelEdit: TBCPanel;
    BCButCancel: TBCButton;
    BCButOK: TBCButton;
    EditText: TEdit;
    LabelSubCapion: TLabel;
    procedure BCButCancelClick(Sender: TObject);
    procedure BCButCancelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure BCButOKClick(Sender: TObject);
    procedure BCLabelCaptionMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OnChangeTheme(Sender: TObject; ThemeColor: jARGB;
      ThemeDark: Boolean; FontColor: jARGB);
  private

  public

  end;

var
  FormTextAsk: TFormTextAsk;
  Used: Boolean;

implementation

{$R *.lfm}

{ TFormTextAsk }

procedure TFormTextAsk.FormShow(Sender: TObject);
Var
  H, W: Int16U;
begin
  H:= Round(Scaling(Self, 130));
  W:= Round(Scaling(Self, 10 +LabelSubCapion.Width));
  if W < 320 then W:= 320;

  Top:= Top-10;
  MoveWindow(Self.Handle,
    Monitor.Left+Round((Monitor.Width-W)/2),
    Monitor.Top+Round((Monitor.Height-H)/2),
    W,H, True
  );

  Used:= False;
end;

procedure TFormTextAsk.OnChangeTheme(Sender: TObject; ThemeColor: jARGB;
  ThemeDark: Boolean; FontColor: jARGB);
Begin
  //RePaintComponent(BCButOK);
  //RePaintComponent(BCButCancel);
end;

procedure TFormTextAsk.BCButOKClick(Sender: TObject);
begin
  Used:= True;
  Close;
end;

procedure TFormTextAsk.BCButCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormTextAsk.BCButCancelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  ButtonRadialEffect(TBCButton(Sender), X,Y);
end;

Var
  CurX, CurY: Integer;

procedure TFormTextAsk.BCLabelCaptionMouseMove(Sender: TObject;
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

procedure TFormTextAsk.FormCreate(Sender: TObject);
begin
  WhenChangeTheme:= @OnChangeTheme;
end;

end.


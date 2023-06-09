unit UnitQuestAsk;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  BCButton, BCLabel, BGRATheme, Windows, JForms, JixPas, MiniJixoid_JCL;

type

  { TFormQuestAsk }

  TFormQuestAsk = class(jJixForm)
    BCButCancel: TBCButton;
    BCButOK: TBCButton;
    LabelSubCapion: TLabel;
    procedure BCButCancelClick(Sender: TObject);
    procedure BCButCancelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure BCButOKClick(Sender: TObject);
    procedure BCLabelCaptionMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure OnChangeTheme(Sender: TObject; ThemeColor: jARGB; ThemeDark: Boolean;
      FontColor: jARGB);
  private

  public

  end;

var
  FormQuestAsk: TFormQuestAsk;
  Used: Boolean;

implementation

{$R *.lfm}

{ TFormQuestAsk }

procedure TFormQuestAsk.FormShow(Sender: TObject);
Var
  H, W: Int16U;
begin
  H:= Round(Scaling(Self, 110+LabelSubCapion.Height));
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

procedure TFormQuestAsk.OnChangeTheme(Sender: TObject; ThemeColor: jARGB;
  ThemeDark: Boolean; FontColor: jARGB);
Begin
  //RePaintComponent(BCButOK);
  //RePaintComponent(BCButCancel);
end;

procedure TFormQuestAsk.BCButOKClick(Sender: TObject);
begin
  Used:= True;
  Close;
end;

procedure TFormQuestAsk.BCButCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormQuestAsk.BCButCancelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  ButtonRadialEffect(Sender as TBCButton, X,Y);
end;

Var
  CurX, CurY: Integer;

procedure TFormQuestAsk.BCLabelCaptionMouseMove(Sender: TObject;
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

procedure TFormQuestAsk.FormCreate(Sender: TObject);
begin
  WhenChangeTheme:= @OnChangeTheme;
end;

end.


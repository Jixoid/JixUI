unit CustomErrorSoft;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BCLabel, BCPanel, BCButton, MiniJixoid_JCL, JixPas;

type

  { TFormCustomErrorSoft }

  TFormCustomErrorSoft = class(TForm)
    BCButton1: TBCButton;
    BCLabelCaption: TBCLabel;
    BCLabelInfo: TBCLabel;
    BCPanel1: TBCPanel;
    ImageCaption: TImage;
    procedure BCButton1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    Procedure Up;
    Procedure Down;
  private

  public

  end;

var
  FormCustomErrorSoft: TFormCustomErrorSoft;
  First: Boolean = True;

implementation

{$R *.lfm}

{ TFormCustomErrorSoft }

procedure Anim(SL,FL,ST,FT,SW,FW,SH,FH: Integer; Element: TControl; Speed: Integer) inline;
Var
  i: Integer;
  SR,FR,SB,FB: Integer;
Begin
  SR:= SW+SL;
  FR:= FW+FL;
  SB:= SH+ST;
  FB:= FH+FT;

  For i:= 1 to Speed do
  Begin
    Element.Left:=  Round((FL-SL)/Speed*i)+SL;
    Element.Top:=   Round((FT-ST)/Speed*i)+ST;
    Element.Width:= Round((FR-SR)/Speed*i)+SR-Round((FL-SL)/Speed*i)-SL;
    Element.Height:=Round((FB-SB)/Speed*i)+SB-Round((FT-ST)/Speed*i)-ST;
    Application.ProcessMessages;
  end;
end;

Procedure TFormCustomErrorSoft.Up;
begin
  Anim(
    Self.Left, Self.Left,
    20, 0,
    Self.Width, Self.Width,
    60, 0, Self, 10
  );
End;

Procedure TFormCustomErrorSoft.Down;
begin
  Anim(
    Self.Left, Self.Left,
    0, 20,
    Self.Width, Self.Width,
    0, 60, Self, 10
  );
end;

procedure TFormCustomErrorSoft.FormCreate(Sender: TObject);
begin
  //GlassForm(Self);
  Self.Width:= Round(Monitor.Width/3*2);
  Self.Left := Round((Monitor.Width-Self.Width)/2);

  BCPanel1.Border.Color:= GetThemeColor;
  BCPanel1.Background.Gradient1.StartColor:= GetThemeColor;
  BCButton1.StateClicked.Background.Color:= GetThemeColor;
end;

procedure TFormCustomErrorSoft.FormResize(Sender: TObject);
begin
  //FormRadial(Self, 12);
end;

procedure TFormCustomErrorSoft.BCButton1Click(Sender: TObject);
begin
  Self.Up;
  Self.Close;
end;

procedure TFormCustomErrorSoft.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

end.


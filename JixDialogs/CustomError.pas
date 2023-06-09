unit CustomError;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, windows, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, BCButton, BCPanel, MiniJixoid_JCL, JixPas;

type

  { TFormCustomError }

  TFormCustomError = class(TForm)
    BCButHalt: TBCButton;
    BCButton1: TBCButton;
    BCPanel1: TBCPanel;
    LabelCaption: TLabel;
    MemoLog: TMemo;
    Panel1: TBCPanel;
    procedure BCButHaltClick(Sender: TObject);
    procedure BCButton1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabelCaptionMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private

  public

  end;

var
  FormCustomError: TFormCustomError;

implementation

{$R *.lfm}

{ TFormCustomError }

procedure TFormCustomError.BCButton1Click(Sender: TObject);
begin
  Self.Top:= Self.Top-75;
  Self.Height:= 220;
  BCButton1.Visible:= False;
end;

procedure TFormCustomError.BCButHaltClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TFormCustomError.FormResize(Sender: TObject);
begin
  //FormRadial(Self, 12);
end;

procedure TFormCustomError.FormShow(Sender: TObject);
Var
  bColor: jARGB;

  Procedure ColorButton(Button: TBCButton);
  Begin
    Button.StateHover.Background.Color:= bColor;
    Button.StateClicked.Background.Color:= bColor;
  end;

begin
  bColor:= GetThemeColor;

  BCPanel1.Border.Color:= bColor;
  Panel1.Border.Color:= bColor;
  ColorButton(BCButton1);
end;

procedure TFormCustomError.LabelCaptionMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  Begin
    ReleaseCapture;
    SendMessage(Self.Handle, WM_SYSCOMMAND, 61458, 0) ;
  end;
end;

end.


unit JixScrollBar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Math,
  JixComponent, JixPas;

type
  eScrollbarOrientation = (
    jsoVertical,
    jsoHoriznal
  );

  TJixScrollBar = class(TJixControl)
  private
    Var ClickP: TPoint;
    Var FEdgeSpace: Int8U;
    Var FPage: Int16U;
    Var FLegPos: Int16U;
    Var FPosition: Int16U;
    Var FOnChange: mNotify;
    Var FOrientation: eScrollbarOrientation;

    Procedure SetEdgeSpace(Value: Int8U);
    Procedure SetPage(Value: Int16U);
    Procedure SetPosition(Value: Int16U);
    Procedure SetOrientation(Value: eScrollbarOrientation);

  protected
    Procedure Paint; Override;
    Procedure Mousedown(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer); Override;
    Procedure Mousemove(Shift: Tshiftstate; X, Y: Integer); Override;

  public
    Constructor Create(Aowner: Tcomponent); Override;

  published
    Property EdgeSpace: Int8U  Read FEdgeSpace   Write SetEdgeSpace   Default 22;
    Property Page: Int16U      Read FPage        Write SetPage;
    Property Position: Int16U  Read FPosition    Write SetPosition;
    Property OnChange: mNotify Read FOnChange    Write FOnChange;
    Property Orientation: eScrollbarOrientation
                               Read FOrientation Write SetOrientation Default jsoHoriznal;

    Property Tooltip;
    Property ShowTooltip;

    Property StateNormal;
    Property StateHover;
    Property StateClick;

    Property Static;

    Property Anchors;
    //Property AutoSize;
    //Property Caption;
    Property Enabled;
    Property Visible;

    Property OnChangeBounds;
    Property OnPaint;
    Property OnClick;
    Property OnDblClick;
    Property OnMouseDown;
    Property OnMouseEnter;
    Property OnMouseLeave;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnMouseWheel;
    Property OnMouseWheelDown;
    Property OnMouseWheelUp;
    Property OnResize;
    Property PopupMenu;
  end;

procedure Register;

implementation

Constructor TJixScrollBar.Create(Aowner: Tcomponent);
Begin
  Inherited;

  FPosition:= 1;
  FPage:= 10;

  FOnChange:= Nil;
  FOrientation:= jsoHoriznal;

  FEdgeSpace:= 0;
  UseCustomRect:= True;
End;

Procedure TJixScrollBar.Paint;
Var
  W,H: Int32S;
  Cac: jJixState;
  S: Boolean;
Begin
  Caption:= '';

  Cac:= NowState;
  S:= Static;
  Static:= True;
  NowState:= StateNormal;

  ScreenRect.Left:= 0;
  ScreenRect.Top:= 0;
  ScreenRect.Width:= Width;
  ScreenRect.Height:= Height;

  Inherited;

  Static:= S;
  NowState:= Cac;

  Case Orientation of
    jsoHoriznal: Begin

        Caption:= '|||';

        W:= Max(20, Min(Width, Round(Width/Page*Width)));
        H:= Height;

        ScreenRect.Left:= Round((Width-W)/Page*(Position-1));
        ScreenRect.Top:= 0;
        ScreenRect.Width:= W;
        ScreenRect.Height:= H;

        Inherited;
      End;

    jsoVertical: Begin

        Caption:= '-'+#13+'-'+#13+'-';

        W:= Width;
        H:= Max(20, Min(Height, Round(Height/Page*Height)));

        ScreenRect.Left:= 0;
        ScreenRect.Top:= Round((Height-H)/Page*(Position-1));
        ScreenRect.Width:= W;
        ScreenRect.Height:= H;

        Inherited;
      End;
  End;

End;

Procedure TJixScrollBar.Mousemove(Shift: Tshiftstate; X, Y: Integer);
Begin
  Inherited;

  if ssLeft in Shift then
    Case Orientation of
      jsoHoriznal:
        if (X -ClickP.X) <> 0 then
          Position:= FLegPos +Round((X -ClickP.X)/((Width-ScreenRect.Width)/Page));

      jsoVertical:
        if (Y -ClickP.Y) <> 0 then
          Position:= FLegPos +Round((Y -ClickP.Y)/((Height-ScreenRect.Height)/Page));
    End;

End;

Procedure TJixScrollBar.Mousedown(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer);
Begin
  Inherited;

  if Button = mbLeft then
  Begin
    FLegPos:= Position;

    ClickP.X:= X;
    ClickP.Y:= Y;
  End;
End;

Procedure TJixScrollBar.SetEdgeSpace(Value: Int8U);
Begin
  if FEdgeSpace = Value then Exit;

  FEdgeSpace:= Value;

  Invalidate;
End;

Procedure TJixScrollBar.SetPage(Value: Int16U);
Begin
  if Value = 0 then Exit;

  if FPage = Value then Exit;

  if FPosition > Value then
    FPosition:= Value;

  FPage:= Value;

  Invalidate;
End;

Procedure TJixScrollBar.SetPosition(Value: Int16U);
Begin
  if (Value = 0) then Exit;

  if FPosition = Value then Exit;

  if FPage >= Value then
    FPosition:= Value;

  Invalidate;

  if Assigned(OnChange) then
    OnChange(Self);
End;

Procedure TJixScrollBar.SetOrientation(Value: eScrollbarOrientation);
Var
  C: Integer;
Begin
  if FOrientation = Value then Exit;

  FOrientation:= Value;

  C:= Width;
  Width:= Height;
  Height:= C;

  Invalidate;
End;


procedure Register;
begin
  {$I jixscrollbar_icon.lrs}
  RegisterComponents('JixUI',[TJixScrollBar]);
end;

end.

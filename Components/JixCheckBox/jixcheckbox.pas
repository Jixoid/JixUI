unit JixCheckBox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes, BGRAGradients, BGRAGradientScanner,
  JixPas, JixComponent, JForms;

type
  TJixCheckBox = class(TJixControl)
  Private
    Var FChecked: Boolean;
    Var FOnChange: mNotify;

    Var FButtonSize: Int8U;

    Procedure SetChecked(Value: Boolean);

  Protected
    Procedure Click; Override;
    Procedure RealSetText(const Value: TCaption); Override;
    Procedure Paint; Override;

  Public
    Constructor Create(AOwner: TComponent); Override;

  Published
    Property Tooltip;
    Property ShowTooltip;

    Property Static;

    Property Anchors;
    //Property AutoSize;
    Property Caption;
    Property Enabled;
    Property Visible;

    Property Checked: Boolean             Read FChecked      Write SetChecked;
    Property StateNormal;
    Property StateHover;
    Property StateClick;

    Property ButtonSize: Int8U            Read FButtonSize   Write FButtonSize   Default 16;

    Property OnChange: mNotify            Read FOnChange     Write FOnChange;
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
  End;

procedure Register;

implementation

Constructor TJixCheckBox.Create(AOwner: TComponent);
Begin
  Inherited;

  ButtonSize:= 16;

  OnChange:= Nil;
  Checked:= False;

  Height:= 17;
  Width:= 100;
End;

Procedure TJixCheckBox.Click;
Begin
  Checked:= not Checked;
End;

Procedure TJixCheckBox.RealSetText(const Value: TCaption);
Begin
  Inherited;

  Invalidate;
End;

procedure TJixCheckBox.Paint;

  Function AddAlpha(Color: TColor; Alpha: Int8U): TBGRAPixel;
  Var
    C: jARGB;
    luma: Byte;
  Begin
    C:= Color;

    if Enabled then
      Result:= BGRA(C.Red, C.Green, C.Blue, Alpha)

    Else Begin
      luma:= Round((0.299 * C.Red) + (0.587 * C.Green) + (0.114 * C.Blue));

      Result:= BGRA(luma, luma, luma, Alpha);
    End;

  End;

Var
  TextStyle: TTextStyle;
  BS: Byte = 16;

  g: TBGRAMultiGradient;
  gs: TBGRAGradientScanner;
Begin
  if not(Visible or (csDesigning in ComponentState)) then Exit;

  BS:= ButtonSize;

  Canvas.Changing;

  Bmp:= TBGRABitmap.Create(ClientWidth, ClientHeight, BGRA(0,0,0,0));
  Bmp.JoinStyle:= pjsRound;

  {%Region Background}
  Case NowState.Background.Style of
    jbgsColor:
      Begin
        Bmp.FillRoundRectAntialias(
          0, 0, BS, BS,
          NowState.Rounding.RoundX, NowState.Rounding.RoundY,
          AddAlpha(NowState.Background.Color ,NowState.Background.Opacity),
          NowState.Rounding.Options
        );
      End;

    jbgsGradient:
      Begin
        g:= TBGRAMultiGradient.Create(
          [
            AddAlpha(
              NowState.Background.Point1.Color,
              NowState.Background.Point1.Opacity
            ),
            AddAlpha(
              NowState.Background.Point2.Color,
              NowState.Background.Point2.Opacity
            )
          ],

          [0, 1], True
        );

        gs:= TBGRAGradientScanner.Create(
          g,
          NowState.Background.GradientType,
          PointF(
            NowState.Background.Point1.XPercent/100*BS,
            NowState.Background.Point1.YPercent/100*BS
          ),
          PointF(
            NowState.Background.Point2.XPercent/100*BS,
            NowState.Background.Point2.YPercent/100*BS
          )
        );
        gs.Sinus:= NowState.Background.Sinus;

        Bmp.FillRoundRectAntialias(
          0, 0, BS, BS,
          NowState.Rounding.RoundX, NowState.Rounding.RoundY,
          gs, NowState.Rounding.Options
        );
        g.Free;
        gs.Free;
      End;
  End;
  {%EndRegion}

  {%Region Border}
  if NowState.Border.Style = jbsSolid then
  Begin
    Bmp.RoundRectAntialias(
      0, 0, BS, BS,
      NowState.Rounding.RoundX, NowState.Rounding.RoundY,
      AddAlpha(
        NowState.Border.ExColor,
        NowState.Border.ExOpacity
      ),
      NowState.Border.ExWidth,
      NowState.Rounding.Options
    );
  End;
  {%EndRegion}

  {%Region Text}
  Bmp.FontQuality:= fqSystemClearType;
  Bmp.FontHeight:= NowState.FontEx.Height;
  Bmp.FontName:= NowState.FontEx.Name;
  Bmp.FontStyle:= NowState.FontEx.Style;

  TextStyle.Alignment:= NowState.FontEx.Alignment;
  TextStyle.Layout:= NowState.FontEx.Layout;
  TextStyle.SingleLine:= NowState.FontEx.SingleLine;
  TextStyle.EndEllipsis:= False;
  TextStyle.Wordbreak:= NowState.FontEx.WordBreak;

  Bmp.TextRect(
    Rect(
      NowState.FontEx.PaddingLeft +BS,
      NowState.FontEx.PaddingTop,
      Width  -NowState.FontEx.PaddingRight,
      Height -NowState.FontEx.PaddingBottom
    ),
    NowState.FontEx.PaddingLeft +BS,
    NowState.FontEx.PaddingTop,

    Caption,
    TextStyle,
    AddAlpha(
      NowState.FontEx.Color,
      255
    )
  );
  {%EndRegion}

  {%Region Check}
  if Checked then
  Begin
    Bmp.DrawPolyLineAntialias(
      [
        PointF((BS/4), BS/2),
        PointF(BS/5*2, BS-(BS/4)),
        PointF(BS-(BS/4), BS/4)
      ],
      AddAlpha(
        NowState.FontEx.Color,
        NowState.FontEx.Opacity
      ),
      (BS/8)
    );
  End;
  {%EndRegion}

  Bmp.Draw(Canvas, 0,0, False);

  Bmp.Free;

  Canvas.Changed;
End;

Procedure TJixCheckBox.SetChecked(Value: Boolean);
Begin
  if FChecked = Value Then Exit;

  FChecked:= Value;
  if Assigned(OnChange) then
    OnChange(Self);

  Invalidate;
End;


procedure Register;
begin
  {$I jixcheckbox_icon.lrs}
  RegisterComponents('JixUI',[TJixCheckBox]);
end;

end.

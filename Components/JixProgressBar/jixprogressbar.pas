unit JixProgressBar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes, BGRAGradients, BGRAGradientScanner,
  JixPas, JixComponent, JForms;

type
  eProgressOrientation = (
    jpoLeftToRight,
    jpoRightToLeft,
    jpoTopToBottom,
    jpoBottomToTop
  );

  TJixProgressBar = class(TJixControl)
  Private
    Var FMin: Int16S;
    Var FMax: Int16S;
    Var FProgress: Int16S;
    Var FBarBlur: Int8U;
    Var FOrientation: eProgressOrientation;

    Procedure SetMin(Value: Int16S);
    Procedure SetMax(Value: Int16S);
    Procedure SetValue(Value: Int16S);
    Procedure SetBarBlur(Value: Int8U);
    Procedure SetOrientation(Value: eProgressOrientation);

  Protected
    Procedure Paint; Override;
    Procedure SetTemplate(Temp: Jtemplatecollectionitem); Override;

  Public
    Constructor Create(Aowner: Tcomponent); Override;

  Published
    Property Min: Int16S      Read FMin         Write SetMin         Default 0;
    Property Max: Int16S      Read FMax         Write SetMax         Default 100;
    Property Progress: Int16S Read FProgress    Write SetValue;
    Property BarBlur: Int8U   Read FBarBlur     Write SetBarBlur     Default 10;
    Property Orientation: eProgressOrientation
                              Read FOrientation Write SetOrientation Default jpoLeftToRight;

    Property Tooltip;
    Property ShowTooltip;

    Property StateNormal;

    Property Anchors;
    //Property AutoSize;
    Property Caption;
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
  End;

procedure Register;

implementation

Constructor TJixProgressBar.Create(Aowner: Tcomponent);
Begin
  Inherited;

  Static:= True;

  Height:= 25;
  Width:= 100;

  FMin:= 0;
  FMax:= 100;
  FProgress:= 30;
  FBarBlur:= 10;
  FOrientation:= jpoLeftToRight;

  ThemeName:= 'Default_Progress';
  SetTemplate(FormManager.Templates.Items[0].Templates.Items[1] as jTemplateCollectionItem);
End;

Procedure TJixProgressBar.Paint;

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

  Function CalcProgressPer: IntPer;
  Begin
    if (Progress-Min) <> 0 then
      Result:= Round(((Progress-Min)/(Max-Min))*100)
    Else
      Result:= 0;
  End;

Var
  TextStyle: TTextStyle;

  g: TBGRAMultiGradient;
  gs: TBGRAGradientScanner;
Begin
  if not(Visible or (csDesigning in ComponentState)) then Exit;

  Canvas.Changing;

  Bmp:= TBGRABitmap.Create(ClientWidth, ClientHeight, BGRA(0,0,0,0));
  Bmp.JoinStyle:= pjsRound;

  {%Region Background}
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

  Case Orientation of
    jpoLeftToRight:
      gs:= TBGRAGradientScanner.Create(
        g,
        NowState.Background.GradientType,
        PointF(
          CalcProgressPer/100*Width,
          0
        ),
        PointF(
          (CalcProgressPer+BarBlur)/100*Width,
          0
        )
      );

    jpoRightToLeft:
      gs:= TBGRAGradientScanner.Create(
        g,
        NowState.Background.GradientType,
        PointF(
          (CalcProgressPer+BarBlur)/100*Width,
          0
        ),
        PointF(
          CalcProgressPer/100*Width,
          0
        )
      );

    jpoTopToBottom:
      gs:= TBGRAGradientScanner.Create(
        g,
        NowState.Background.GradientType,
        PointF(
          0,
          CalcProgressPer/100*Height
        ),
        PointF(
          0,
          (CalcProgressPer+BarBlur)/100*Height
        )
      );

    jpoBottomToTop:
      gs:= TBGRAGradientScanner.Create(
        g,
        NowState.Background.GradientType,
        PointF(
          0,
          (CalcProgressPer+BarBlur)/100*Height
        ),
        PointF(
          0,
          CalcProgressPer/100*Height
        )
      );
  End;

  gs.Sinus:= NowState.Background.Sinus;

  Bmp.FillRoundRectAntialias(
    0, 0, Width-1, Height-1,
    NowState.Rounding.RoundX, NowState.Rounding.RoundY,
    gs, NowState.Rounding.Options
  );
  g.Free;
  gs.Free;
  {%EndRegion}

  {%Region Border}
  if NowState.Border.Style = jbsSolid then
  Begin
    Bmp.RoundRectAntialias(
      0, 0, Width-1, Height-1,
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
      NowState.FontEx.PaddingLeft,
      NowState.FontEx.PaddingTop,
      Width  -NowState.FontEx.PaddingRight,
      Height -NowState.FontEx.PaddingBottom
    ),
    NowState.FontEx.PaddingLeft,
    NowState.FontEx.PaddingTop,

    Caption,
    TextStyle,
    AddAlpha(
      NowState.FontEx.Color,
      255
    )
  );
  {%EndRegion}

  Bmp.Draw(Canvas, 0,0, False);

  Bmp.Free;

  Canvas.Changed;
End;

Procedure TJixProgressBar.SetTemplate(Temp: jTemplateCollectionItem);
Begin
  Inherited;
  Exit;

  Self.StateNormal.Background.Point2.Opacity:= 0;
  Self.StateHover.Background.Point2.Opacity:= 0;
  Self.StateClick.Background.Point2.Opacity:= 0;

  Invalidate;
End;

Procedure TJixProgressBar.SetMin(Value: Int16S);
Begin
  if FMin = Value then Exit;

  if FProgress < Value then
  Begin
    FProgress:= Value;
  End;

  FMin:= Value;

  Invalidate;
End;

Procedure TJixProgressBar.SetMax(Value: Int16S);
Begin
  if FMax = Value then Exit;

  if FProgress > Value then
  Begin
    FProgress:= Value;
  End;

  FMax:= Value;

  Invalidate;
End;

Procedure TJixProgressBar.SetValue(Value: Int16S);
Begin
  if
    (FProgress = Value) or
    (Value < FMin) or
    (Value > FMax)
  then Exit;

  FProgress:= Value;

  Invalidate;
End;

Procedure TJixProgressBar.SetBarBlur(Value: Int8U);
Begin
  if FBarBlur = Value then Exit;

  FBarBlur:= Value;

  Invalidate;
End;

Procedure TJixProgressBar.SetOrientation(Value: eProgressOrientation);
Begin
  if FOrientation = Value then Exit;

  FOrientation:= Value;

  Invalidate;
End;


procedure Register;
begin
  {$I jixprogressbar_icon.lrs}
  RegisterComponents('JixUI',[TJixProgressBar]);
end;

end.

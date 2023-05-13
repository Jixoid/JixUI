unit JixCheckBox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes, BGRAGradients, BGRAGradientScanner,
  BCButton, BCTypes, JixPas, JixComponent, JixTemplate, JForms, MiniJixoid_JCL;

type
  TJixCheckBox = class(TGraphicControl, jJixComponent)
  Private
    Var FChecked: Boolean;
    Var FOnChange: mNotify;

    Var FRounding: TBCRounding;
    Var FStateNormal: TBCButtonState;
    Var FStateHover: TBCButtonState;
    Var FStateClicked: TBCButtonState;

    Var FThemeName: String;

    Procedure SetThemeName(Value: String);
    Function GetThemeName: String;

    Procedure SetChecked(Value: Boolean);
    Procedure StateChanged({%H-}ASender: TObject; {%H-}AData: PtrInt);

  Protected
    Var NowState: ^TBCButtonState;
    Var Bmp: TBGRABitmap;

    Procedure Click; Override;
    Procedure RealSetText(const Value: TCaption); Override;
    Procedure Paint; Override;
    Procedure Mouseenter; Override;
    Procedure Mouseleave; Override;
    Procedure Mousedown(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer); Override;
    Procedure Mouseup(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer); Override;

  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure SetTemplate(Temp: jTemplateCollectionItem);

  Published
    Property Anchors;
    //Property AutoSize;
    Property Caption;
    Property Enabled;
    Property Visible;

    Property Checked: Boolean             Read FChecked      Write SetChecked;
    Property Rounding: TBCRounding        Read FRounding     Write FRounding;
    Property StateNormal: TBCButtonState  Read FStateNormal  Write FStateNormal;
    Property StateHover: TBCButtonState   Read FStateHover   Write FStateHover;
    Property StateClicked: TBCButtonState Read FStateClicked Write FStateClicked;

    Property ThemeName: String            Read GetThemeName  Write SetThemeName;

    Property OnChange: mNotify            Read FOnChange     Write FOnChange;
    Property OnChangeBounds;
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

  OnChange:= Nil;
  Checked:= False;

  Height:= 17;
  Width:= 100;

  Rounding:= TBCRounding.Create(Self);
  StateNormal:= TBCButtonState.Create(Self);
  StateHover:= TBCButtonState.Create(Self);
  StateClicked:= TBCButtonState.Create(Self);

  StateNormal.OnChange:= @StateChanged;
  StateHover.OnChange:= @StateChanged;
  StateClicked.OnChange:= @StateChanged;
  Rounding.OnChange:= @StateChanged;

  NowState:= @StateNormal;

  ThemeName:= 'Default';
  SetTemplate(FormManager.Templates.Items[0].Templates.Items[0] as jTemplateCollectionItem);
End;

Destructor TJixCheckBox.Destroy;
Begin
  Rounding.Free;
  StateNormal.Free;
  StateHover.Free;
  StateClicked.Free;

  Inherited;
End;

Procedure TJixCheckBox.SetThemeName(Value: String);
Begin
  FThemeName:= Value;
End;

Function TJixCheckBox.GetThemeName: String;
Begin
  Result:= FThemeName;
End;

Procedure TJixCheckBox.SetTemplate(Temp: jTemplateCollectionItem);
Begin
  if not Assigned(Temp) then
    {$ifopt D-}
      CustomErrorSend(
        'Access Violation',
        'JixAPI Error!',
        CustomErrorFlag.Close_Halt
      );
    {$Else}
       Raise Exception.Create('Template is Nil') at Self;
    {$Endif}

  Self.StateNormal.Assign(Temp.StateNormal);
  Self.StateHover.Assign(Temp.StateHover);
  Self.StateClicked.Assign(Temp.StateClick);

  Self.StateNormal.FontEx.Color:= FindFontColor(Temp.BackgroundColor);
  Self.StateHover.FontEx.Color:= FindFontColor(Temp.BackgroundColor);
  Self.StateClicked.FontEx.Color:= FindFontColor(Temp.BackgroundColor);

  if not Temp.IgnoreRound then
  Begin
    Self.Rounding.RoundX:= Temp.CornerRound;
    Self.Rounding.RoundY:= Temp.CornerRound;
  End;
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
  Txt: TBGRABitmap;
  BS: Integer = 16;

  g: TBGRAMultiGradient;
  gs: TBGRAGradientScanner;
Begin
  if not(Visible or (csDesigning in ComponentState)) then Exit;

  Canvas.Changing;

  Bmp:= TBGRABitmap.Create(ClientWidth, ClientHeight, BGRA(0,0,0,0));
  Bmp.JoinStyle:= pjsRound;

  {%Region Background}
  Case NowState^.Background.Style of
    bbsColor:
      Begin
        Bmp.FillRoundRectAntialias(
          0, 0, BS, BS,
          Rounding.RoundX, Rounding.RoundY,
          AddAlpha(NowState^.Background.Color ,NowState^.Background.ColorOpacity),
          Rounding.RoundOptions
        );
      End;

    bbsGradient:
      Begin
        //Gradient 1
        g:= TBGRAMultiGradient.Create(
          [
            AddAlpha(
              NowState^.Background.Gradient1.StartColor,
              NowState^.Background.Gradient1.StartColorOpacity
            ),
            AddAlpha(
              NowState^.Background.Gradient1.EndColor,
              NowState^.Background.Gradient1.EndColorOpacity
            )
          ],

          [0, 1], True
        );

        gs:= TBGRAGradientScanner.Create(
          g,
          NowState^.Background.Gradient1.GradientType,
          PointF(
            NowState^.Background.Gradient1.Point1XPercent/100*BS,
            NowState^.Background.Gradient1.Point1YPercent/100*BS
          ),
          PointF(
            NowState^.Background.Gradient1.Point2XPercent/100*BS,
            NowState^.Background.Gradient1.Point2YPercent/100*BS
          )
        );
        gs.Sinus:= NowState^.Background.Gradient1.Sinus;

        Bmp.FillRoundRectAntialias(
          0, 0, BS, BS,
          Rounding.RoundX, Rounding.RoundY,
          gs, Rounding.RoundOptions
        );
        g.Free;
        gs.Free;

        //Gradient 2
        g:= TBGRAMultiGradient.Create(
          [
            AddAlpha(
              NowState^.Background.Gradient2.StartColor,
              NowState^.Background.Gradient2.StartColorOpacity
            ),
            AddAlpha(
              NowState^.Background.Gradient2.EndColor,
              NowState^.Background.Gradient2.EndColorOpacity
            )
          ],

          [0, 1], True
        );

        gs:= TBGRAGradientScanner.Create(
          g,
          NowState^.Background.Gradient2.GradientType,
          PointF(
            NowState^.Background.Gradient2.Point1XPercent/100*BS,
            NowState^.Background.Gradient2.Point1YPercent/100*BS
          ),
          PointF(
            NowState^.Background.Gradient2.Point2XPercent/100*BS,
            NowState^.Background.Gradient2.Point2YPercent/100*BS
          )
        );
        gs.Sinus:= NowState^.Background.Gradient2.Sinus;

        Bmp.FillRoundRect(
          0, Round(NowState^.Background.Gradient1EndPercent/100*BS), BS, BS,
          Rounding.RoundX, Rounding.RoundY,
          gs, NowState^.Background.Gradient2.DrawMode
        );
        g.Free;
        gs.Free;
      End;
  End;
  {%EndRegion}

  {%Region Border}
  if NowState^.Border.Style = bboSolid then
  Begin
    Bmp.RoundRectAntialias(
      0, 0, BS, BS,
      Rounding.RoundX, Rounding.RoundY,
      AddAlpha(
        NowState^.Border.Color,
        NowState^.Border.ColorOpacity
      ),
      NowState^.Border.Width,
      Rounding.RoundOptions
    );
  End;
  {%EndRegion}

  {%Region Text}
  if NowState^.FontEx.Shadow then
  Begin
    Txt:= TextShadow(
      ClientWidth,ClientHeight, Caption,
      NowState^.FontEx.Height,
      NowState^.FontEx.Color,
      AddAlpha(
        NowState^.FontEx.ShadowColor,
        NowState^.FontEx.ShadowColorOpacity
      ),
      NowState^.FontEx.ShadowOffsetX,
      NowState^.FontEx.ShadowOffsetY,
      NowState^.FontEx.ShadowRadius,
      NowState^.FontEx.Style,
      NowState^.FontEx.Name,
      NowState^.FontEx.Shadow
    );

    Bmp.PutImage(16,0, Txt,dmDrawWithTransparency);
    Txt.Free;
  End Else
  Begin
    Bmp.FontQuality:= NowState^.FontEx.FontQuality;
    Bmp.FontHeight:= NowState^.FontEx.Height;
    Bmp.FontName:= NowState^.FontEx.Name;
    Bmp.FontStyle:= NowState^.FontEx.Style;

    Case NowState^.FontEx.TextAlignment of
      bcaLeftTop, bcaLeftCenter, bcaLeftBottom:
        TextStyle.Alignment:= taLeftJustify;

      bcaRightTop, bcaRightCenter, bcaRightBottom:
        TextStyle.Alignment:= taRightJustify;

      bcaCenterTop, bcaCenter, bcaCenterBottom:
        TextStyle.Alignment:= taCenter;
    End;

    Case NowState^.FontEx.TextAlignment of
      bcaLeftTop, bcaCenterTop, bcaRightTop:
        TextStyle.Layout:= tlTop;

      bcaLeftCenter, bcaCenter, bcaRightCenter:
        TextStyle.Layout:= tlCenter;

      bcaLeftBottom, bcaCenterBottom, bcaRightBottom:
        TextStyle.Layout:= tlBottom;
    End;

    TextStyle.Layout:= tlTop;
    TextStyle.SingleLine:= NowState^.FontEx.SingleLine;
    TextStyle.EndEllipsis:= NowState^.FontEx.EndEllipsis;
    TextStyle.Wordbreak:= NowState^.FontEx.WordBreak;

    Bmp.TextRect(
      Rect(
        NowState^.FontEx.PaddingLeft +BS,
        NowState^.FontEx.PaddingTop,
        Width  -NowState^.FontEx.PaddingRight,
        Height -NowState^.FontEx.PaddingBottom
      ),
      NowState^.FontEx.PaddingLeft +BS,
      NowState^.FontEx.PaddingTop,

      Caption,
      TextStyle,
      AddAlpha(
        NowState^.FontEx.Color,
        255
      )
    );
  End;
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
      NowState^.FontEx.Color,
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

Procedure TJixCheckBox.StateChanged(ASender: TObject; AData: PtrInt);
Begin
  Invalidate;
End;

Procedure TJixCheckBox.Mouseenter;
Begin
  Inherited;

  NowState:= @StateHover;
  Invalidate;
End;

Procedure TJixCheckBox.Mouseleave;
Begin
  Inherited;

  NowState:= @StateNormal;
  Invalidate;
End;

Procedure TJixCheckBox.Mousedown(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer);
Begin
  Inherited;

  if Button = mbLeft then
  Begin
    NowState:= @StateClicked;
    Invalidate;
  End;
End;

Procedure TJixCheckBox.Mouseup(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer);
Begin
  Inherited;

  if Button = mbLeft then
  Begin
    if MouseInClient then
      NowState:= @StateHover
    else
      NowState:= @StateNormal;

    Invalidate;
  End;
End;

procedure Register;
begin
  {$I jixcheckbox_icon.lrs}
  RegisterComponents('JixUI',[TJixCheckBox]);
end;

end.

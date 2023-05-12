{%region Title}

  {%region FormEvent}

  Type
    jJixFormTitleEvent = Class
      procedure PressHalt(Sender: TObject);
      procedure PressMax(Sender: TObject);
      procedure PressMinimize(Sender: TObject);
      procedure PressHelp(Sender: TObject);

      procedure LabelCaption_OnMouseMove(Sender: TObject; Shift: TShiftState; X,Y: objpas.Integer);
    end;

  procedure jJixFormTitleEvent.PressHalt(Sender: TObject);
  Begin
    TForm(TBCButton(Sender).Parent).Close;
  end;

  procedure jJixFormTitleEvent.PressMax(Sender: TObject);
  Begin
    With jJixForm(TBCButton(Sender).Parent) do
    Begin
      if WindowState = wsNormal then
        WindowState:= wsMaximized
      else
        WindowState:= wsNormal;
    end;
  end;

  procedure jJixFormTitleEvent.PressMinimize(Sender: TObject);
  Begin
    With jJixForm(TBCButton(Sender).Parent) do
      if Application.MainFormHandle = Handle then
        Application.Minimize
      else
        WindowState:= wsMinimized;
  end;

  procedure jJixFormTitleEvent.PressHelp(Sender: TObject);
  Begin
    if Assigned(jJixForm(TBCButton(Sender).Parent).WhenHelp) then
      jJixForm(TBCButton(Sender).Parent).WhenHelp(jJixForm(TBCButton(Sender).Parent));
  end;

  procedure jJixFormTitleEvent.LabelCaption_OnMouseMove(Sender: TObject; Shift: TShiftState; X,Y: objpas.Integer);
  Begin
    if ssLeft in Shift then
    Begin
      ReleaseCapture;
      SendMessage(jJixForm(TBCLabel(Sender).Parent).Handle, WM_SYSCOMMAND, 61458, 0);
    end;
  end;

  {%endregion}

constructor jJixForm_Title.Create(UpForm: TForm);
Begin
  OUpper:= UpForm;

  OCustom_Caption:= TBCLabel.Create(UpForm);
  With OCustom_Caption do
  Begin
    Name:= 'Custom_Caption';

    OnMouseMove:= jJixFormTitleEvent.LabelCaption_OnMouseMove;
    OnDblClick:= jJixFormTitleEvent.PressMax;

    Parent:= UpForm;
    Align:= alTop;

    FontEx.TextAlignment:= bcaLeftCenter;
    FontEx.PaddingLeft:= 15;
    FontEx.PaddingTop:= -10;
    FontEx.Height:= 20;
    FontEx.Shadow:= True;
    FontEx.ShadowColorOpacity:= 200;
    Background.Style:= bbsGradient;
    Background.Gradient1EndPercent:= 100;
    With Background.Gradient1 do
    Begin
      EndColorOpacity:= 0;
      StartColor:= GetThemeColor;
    end;
  end;

  OCustom_Halt:= TBCButton.Create(UpForm);
  With OCustom_Halt do
  Begin
    Name:= 'Custom_Halt';

    OnMouseMove:= jJixFormTitleEvent.LabelCaption_OnMouseMove;
    OnClick:= jJixFormTitleEvent.PressHalt;

    Caption:= 'X';
    Parent:= UpForm;

    Anchors:= [akTop, akRight];

    Width:= 30;
    Height:= 30;
    AnchorSideLeft.Control:= UpForm;
    AnchorSideLeft.Side:= asrLeft;
    AnchorSideRight.Control:= UpForm;
    AnchorSideRight.Side:= asrRight;
    AnchorSideTop.Control:= UpForm;
    AnchorSideTop.Side:= asrTop;

    Rounding.RoundX:= 0;
    Rounding.RoundY:= 0;
    StateNormal.Background.Style:= bbsClear;
    StateHover.Background.Style:= bbsColor;
    StateHover.Background.Color:= clRed;
    StateHover.Background.ColorOpacity:= 150;
    StateClicked.Background.Style:= bbsColor;
    StateClicked.Background.Color:= clRed;
    StateNormal.FontEx.Height:= 12;
    StateHover.FontEx.Height:= 12;
    StateClicked.FontEx.Height:= 12;
  end;

  OCustom_Maximize:= TBCButton.Create(UpForm);
  With OCustom_Maximize do
  Begin
    Name:= 'Custom_Maximize';

    OnMouseMove:= jJixFormTitleEvent.LabelCaption_OnMouseMove;
    OnClick:= jJixFormTitleEvent.PressMax;

    Caption:= '[]';
    Parent:= UpForm;

    Anchors:= [akTop, akRight];

    Width:= 30;
    Height:= 30;
    AnchorSideLeft.Control:= OCustom_Halt;
    AnchorSideLeft.Side:= asrRight;
    AnchorSideRight.Control:= OCustom_Halt;
    AnchorSideRight.Side:= asrLeft;
    AnchorSideTop.Control:= UpForm;
    AnchorSideTop.Side:= asrTop;

    Rounding.RoundX:= 0;
    Rounding.RoundY:= 0;
    StateNormal.Background.Style:= bbsClear;
    StateHover.Background.Style:= bbsColor;
    StateHover.Background.Color:= clGray;
    StateHover.Background.ColorOpacity:= 150;
    StateClicked.Background.Style:= bbsColor;
    StateClicked.Background.Color:= clGray;
    StateNormal.FontEx.Height:= 12;
    StateHover.FontEx.Height:= 12;
    StateClicked.FontEx.Height:= 12;
  end;

  OCustom_Minimize:= TBCButton.Create(UpForm);
  With OCustom_Minimize do
  Begin
    Name:= 'Custom_Minimize';

    OnMouseMove:= jJixFormTitleEvent.LabelCaption_OnMouseMove;
    OnClick:= jJixFormTitleEvent.PressMinimize;

    Caption:= '_';
    Parent:= UpForm;

    Anchors:= [akTop, akRight];

    Width:= 30;
    Height:= 30;
    AnchorSideLeft.Control:= OCustom_Maximize;
    AnchorSideLeft.Side:= asrRight;
    AnchorSideRight.Control:= OCustom_Maximize;
    AnchorSideRight.Side:= asrLeft;
    AnchorSideTop.Control:= UpForm;
    AnchorSideTop.Side:= asrTop;

    Rounding.RoundX:= 0;
    Rounding.RoundY:= 0;
    StateNormal.Background.Style:= bbsClear;
    StateHover.Background.Style:= bbsColor;
    StateHover.Background.Color:= clGray;
    StateHover.Background.ColorOpacity:= 150;
    StateClicked.Background.Style:= bbsColor;
    StateClicked.Background.Color:= clGray;
    StateNormal.FontEx.Height:= 12;
    StateHover.FontEx.Height:= 12;
    StateClicked.FontEx.Height:= 12;
  end;

  OCustom_Help:= TBCButton.Create(UpForm);
  With OCustom_Help do
  Begin
    Name:= 'Custom_Help';

    OnMouseMove:= jJixFormTitleEvent.LabelCaption_OnMouseMove;
    OnClick:= jJixFormTitleEvent.PressHelp;

    Caption:= '?';
    Parent:= UpForm;

    Anchors:= [akTop, akRight];

    Width:= 30;
    Height:= 30;
    AnchorSideLeft.Control:= OCustom_Minimize;
    AnchorSideLeft.Side:= asrRight;
    AnchorSideRight.Control:= OCustom_Minimize;
    AnchorSideRight.Side:= asrLeft;
    AnchorSideTop.Control:= UpForm;
    AnchorSideTop.Side:= asrTop;

    Rounding.RoundX:= 0;
    Rounding.RoundY:= 0;
    StateNormal.Background.Style:= bbsClear;
    StateHover.Background.Style:= bbsColor;
    StateHover.Background.Color:= clGray;
    StateHover.Background.ColorOpacity:= 150;
    StateClicked.Background.Style:= bbsColor;
    StateClicked.Background.Color:= clGray;
    StateNormal.FontEx.Height:= 12;
    StateHover.FontEx.Height:= 12;
    StateClicked.FontEx.Height:= 12;

    Visible:= False;
  end;

  //UpForm.Height:= UpForm.Height+ OCustom_Caption.Height;
end;

Procedure jJixForm_Title.SetTextPosition(Value: eHorzAlign);

  Function FindUsableRight: Integer;
  Begin
    if Custom_Help.Visible then
      Result:= Custom_Caption.Width -Custom_Help.Left

    else if Custom_Minimize.Visible then
      Result:= Custom_Caption.Width -Custom_Minimize.Left

    else if Custom_Maximize.Visible then
      Result:= Custom_Caption.Width -Custom_Maximize.Left

    else if Custom_Halt.Visible then
      Result:= Custom_Caption.Width -Custom_Halt.Left

    else
      Result:= 15;
  End;

  Function FindUsableLeft: Integer;
  Begin
    if Custom_Help.Visible then
      Result:= Custom_Help.BoundsRect.Right

    else if Custom_Minimize.Visible then
      Result:= Custom_Minimize.BoundsRect.Right

    else if Custom_Maximize.Visible then
      Result:= Custom_Maximize.BoundsRect.Right

    else if Custom_Halt.Visible then
      Result:= Custom_Halt.BoundsRect.Right

    else
      Result:= 15;
  End;

Begin
  OTextPosition:= Value;
  Custom_Caption.FontEx.PaddingLeft:= 0;
  Custom_Caption.FontEx.PaddingRight:= 0;

  Case Value of
    haCenter: Custom_Caption.FontEx.TextAlignment:= bcaCenter;

    haLeft:
      Begin
        if ButtonsPosition = haRight then
        Begin
          Custom_Caption.FontEx.PaddingLeft:= 15;
          Custom_Caption.FontEx.TextAlignment:= bcaLeftCenter
        End Else Begin
          Custom_Caption.FontEx.PaddingLeft:= FindUsableLeft;
          Custom_Caption.FontEx.TextAlignment:= bcaLeftCenter;
      End;

    End;

    haRight:
      Begin
        if ButtonsPosition = haLeft then
        Begin
          Custom_Caption.FontEx.PaddingRight:= 15;
          Custom_Caption.FontEx.TextAlignment:= bcaRightCenter
        End Else Begin
          Custom_Caption.FontEx.PaddingRight:= FindUsableRight;
          Custom_Caption.FontEx.TextAlignment:= bcaRightCenter;
      End;

    End;


  End;
End;

Procedure jJixForm_Title.SetButtonsPosition(Value: eHorzAlignNC);
Begin
  OButtonsPosition:= Value;

  Case Value of
    haRight:
      Begin
        Custom_Halt.Anchors:= [akTop, akRight];
        Custom_Maximize.Anchors:= [akTop, akRight];
        Custom_Minimize.Anchors:= [akTop, akRight];
        Custom_Help.Anchors:= [akTop, akRight];
      End;

    haLeft:
      Begin
        Custom_Halt.Anchors:= [akTop, akLeft];
        Custom_Maximize.Anchors:= [akTop, akLeft];
        Custom_Minimize.Anchors:= [akTop, akLeft];
        Custom_Help.Anchors:= [akTop, akLeft];
      End;
  End;

  TextPosition:= TextPosition;
End;

Function jJixForm_Title.GetCaption: TCaption;
Begin
  Result:= OCustom_Caption.Caption;
end;

Procedure jJixForm_Title.SetCaption(Value: TCaption);
Begin
  OCustom_Caption.Caption:= Value;
  //SetForeColor(FindFontColor( ActiveColor.Mix(OUpper.Color) ));
end;

Function jJixForm_Title.GetForeColor: jARGB;
Begin
  Result:= OCustom_Caption.FontEx.Color;
end;

Procedure jJixForm_Title.SetForeColor(Value: jARGB);
Begin
  OCustom_Caption.FontEx.Color:= Value;
  OCustom_Caption.FontEx.ShadowColor:= Value;
end;
{%endregion}

{%region CustomJixForm}

{
Procedure jCustomJixForm.WhenChangePPI(Sender: TObject; NewPPI, LegPPI: Integer);

  Procedure ResetButton(bButton: TBCButton);
  Begin
    bButton.GlyphScale:= Scaling(Self, 1);

    bButton.StateNormal.FontEx.Height:= Round(Scaling(Self, 14));
    bButton.StateHover.FontEx.Height:= Round(Scaling(Self, 14));
    bButton.StateClicked.FontEx.Height:= Round(Scaling(Self, 14));
  end;

Begin
  Title.Custom_Caption.FontEx.Height:= Round(PixelsPerInch/96*20);

  ResetButton(Title.OCustom_Halt);
  ResetButton(Title.OCustom_Max);
  ResetButton(Title.OCustom_Minimize);
  ResetButton(Title.OCustom_Mini);
  ResetButton(Title.OCustom_Help);
end;
}

Constructor jCustomJixForm.Create(TheOwner: TComponent);
Var
  Style: Longint;
  FontC: jARGB;
Begin
  inherited;

  ActiveTheme:= True;
  Title:= jJixForm_Title.Create(Self);
  Title.ButtonsPosition:= haRight;
  Title.TextPosition:= haLeft;
  Title.Caption:= Caption;

  if BorderStyle = bsNone then Exit;
  Style := GetWindowLong(Handle, GWL_STYLE);
  if (Style and WS_CAPTION) = WS_CAPTION then
  begin
    case BorderStyle of
      bsSingle,
      bsSizeable: SetWindowLong(Handle, GWL_STYLE, Style and
          (not (WS_CAPTION)) or WS_BORDER);
      bsDialog: SetWindowLong(Handle, GWL_STYLE, Style and
          (not (WS_CAPTION)) or DS_MODALFRAME or WS_DLGFRAME);
    end;
    Height := Height - GetSystemMetrics(SM_CYCAPTION);
    Refresh;
  end;
  Application.ProcessMessages;

  FontC:= FindFontColor(ColorToARGB(GetThemeColor).Mix(Color));

  if Assigned(WhenChangeTheme) then
    WhenChangeTheme(Self, ColorToARGB(GetThemeColor), GetDarkTheme, FontC);

  FormManager.AddForm(Self);
End;

{%endregion}

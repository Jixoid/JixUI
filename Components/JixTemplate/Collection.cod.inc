Constructor jTemplateCollectionItem.Create(ACollection: TCollection);
Begin
  Inherited;

  IgnoreRound:= False;
  DarkTheme:= dtAuto;
  CornerRound:= 12;

  FOnPaintTemplate:= Nil;

  FStateNormal:= TBCButtonState.Create(Nil);
  FStateHover:= TBCButtonState.Create(Nil);
  FStateClick:= TBCButtonState.Create(Nil);
End;

Destructor jTemplateCollectionItem.Destroy;
Begin
  FStateNormal.Free;
  FStateHover.Free;
  FStateClick.Free;

  Inherited;
End;

Procedure jTemplateCollectionItem.PaintTemplate(Color: jARGB; Dark: Boolean);

  Procedure ColorState(State: TBCButtonState);
  Begin
    State.Background.Color:= Color;
    State.Background.Gradient1.StartColor:= Color;
    State.Background.Gradient1.EndColor:= Color;
    State.Background.Gradient2.StartColor:= Color;
    State.Background.Gradient2.EndColor:= Color;

    State.Border.Color:= Color;

    State.FontEx.Color:= FindFontColor(ColorToARGB(BackgroundColor).Mix(Color, State.Background.ColorOpacity));
    State.FontEx.ShadowColor:= State.FontEx.Color;
  End;

Begin
  if not Assigned(OnPaintTemplate) then
  Begin
    Case DarkTheme of
      dtAuto: Begin

          if GetDarkTheme then
            BackgroundColor:= $000000
          else
            BackgroundColor:= $F5F5F5;
        End;

      dtDark: BackgroundColor:= $000000;

      dtLight: BackgroundColor:= $F5F5F5;
    End;

    ColorState(StateNormal);
    ColorState(StateHover);
    ColorState(StateClick);

  End Else
    OnPaintTemplate(Self, Color, Dark);

End;

Procedure jTemplateCollectionItem.SetDarkTheme(Value: eDarkTheme);
Begin
  if FDarkTheme = Value then Exit;

  FDarkTheme:= Value;

  Case DarkTheme of
    dtAuto: Begin

        if GetDarkTheme then
          BackgroundColor:= $000000
        else
          BackgroundColor:= $F5F5F5;
      End;

    dtDark: BackgroundColor:= $000000;

    dtLight: BackgroundColor:= $F5F5F5;
  End;
End;

Procedure jTemplateCollectionItem.Assign(Source: Tpersistent);
Var
  S: jTemplateCollectionItem;
Begin
  S:= Source as jTemplateCollectionItem;

  BackgroundColor:= S.BackgroundColor;
  CornerRound:= S.CornerRound;
  DarkTheme:= S.DarkTheme;
  IgnoreRound:= S.IgnoreRound;

  StateNormal.Assign(S.StateNormal);
  StateHover.Assign(S.StateHover);
  StateClick.Assign(S.StateClick);

  //ThemeName:= S.ThemeName;
End;

Procedure jTemplateCollectionItem.Assignto(Dest: Tpersistent);
Begin
  jTemplateCollectionItem(Dest).Assign(Self);
End;








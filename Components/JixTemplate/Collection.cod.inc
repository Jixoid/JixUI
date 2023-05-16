Constructor jTemplateCollectionItem.Create(ACollection: TCollection);
Begin
  Inherited;

  IgnoreRound:= False;
  DarkTheme:= dtAuto;

  OnPaintTemplate:= Nil;

  StateNormal:= jJixState.Create;
  StateHover:= jJixState.Create;
  StateClick:= jJixState.Create;

  StaticBackground:= False;
  StaticBorder:= False;
  StaticFont:= False;
  StaticRound:= False;
End;

Destructor jTemplateCollectionItem.Destroy;
Begin
  FStateNormal.Free;
  FStateHover.Free;
  FStateClick.Free;

  Inherited;
End;

Procedure jTemplateCollectionItem.PaintTemplate(Color: jARGB; Dark: Boolean);

  Procedure ColorState(State: jJixState);
  Begin
    State.Background.Color:= Color;
    State.Background.Point1.Color:= Color;
    State.Background.Point2.Color:= Color;

    State.Border.ExColor:= Color;
    State.Border.InColor:= Color;

    State.FontEx.Color:= FindFontColor(ColorToARGB(BackgroundColor).Mix(Color, State.Background.Opacity));
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








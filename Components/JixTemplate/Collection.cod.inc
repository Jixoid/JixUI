Constructor jTemplateCollectionItem.Create(ACollection: TCollection);
Begin
  Inherited;

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

    State.FontEx.Color:= FindFontColor(Color);
  End;

Begin
  if not Assigned(OnPaintTemplate) then
  Begin
    ColorState(StateNormal);
    ColorState(StateHover);
    ColorState(StateClick);

  End Else
    OnPaintTemplate(Self, Color, Dark);

End;



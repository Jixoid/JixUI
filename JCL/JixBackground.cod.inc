Procedure jJixBackground.DoCreate;
Begin
  Inherited;

  FColor:= clWhite;
  FOpacity:= 255;
  FStyle:= jbgsColor;
  FSinus:= False;

  Point1:= jGradientPoint.Create(Self);
  Point2:= jGradientPoint.Create(Self);
End;

Destructor jJixBackground.Destroy;
Begin
  Point1.Free;
  Point2.Free;

  Inherited;
End;

Procedure jJixBackground.Assign(Source: jJixProperty);
Var
  S: jJixBackground;
Begin
  if not Assigned(Source) then
    {$ifopt D-}
      Exit;
    {$Else}
       if
        MessageDlg(
          'Access Violation',
          'Source parameter isn''t assigned' +#13+#13+
          'Class: '+Self.ClassName +#13+
          'Method: '+'Assign' +#13+
          'Parameter: '+'Source' +#13,
          mtError, [mbIgnore, mbAbort], 0
        ) = mrAbort
      then
        Raise EAccessViolation.Create('Source parameter isn''t assigned');
    {$Endif}

  if Source is jJixBackground then
  Begin
    S:= Source as jJixBackground;

    FColor:= S.Color;
    FOpacity:= S.Opacity;
    FStyle:= S.Style;
    FGradientType:= S.GradientType;
    FSinus:= S.Sinus;

    Point1.Assign(S.Point1);
    Point2.Assign(S.Point2);

    DoChanged;
  End Else
    Inherited;
End;

Procedure jJixBackground.SetColor(Value: TColor);
Begin
  if FColor = Value then Exit;

  FColor:= Value;

  DoChanged;
End;

Procedure jJixBackground.SetOpacity(Value: Int8U);
Begin
  if FOpacity = Value then Exit;

  FOpacity:= Value;

  DoChanged;
End;

Procedure jJixBackground.SetStyle(Value: eJixBackgroundStyle);
Begin
  if FStyle = Value then Exit;

  FStyle:= Value;

  DoChanged;
End;

Procedure jJixBackground.SetGradientType(Value: TGradientType); Inline;
Begin
  if FGradientType = Value then Exit;

  FGradientType:= Value;

  DoChanged;
End;

Procedure jJixBackground.SetSinus(Value: Boolean);
Begin
  if FSinus = Value then Exit;

  FSinus:= Value;

  DoChanged;
End;

//Gradients
Procedure jGradientPoint.Assign(Source: jJixProperty);
Var
  S: jGradientPoint;
Begin
  if not Assigned(Source) then
    {$ifopt D-}
      Exit;
    {$Else}
       if
        MessageDlg(
          'Access Violation',
          'Source parameter isn''t assigned' +#13+#13+
          'Class: '+Self.ClassName +#13+
          'Method: '+'Assign' +#13+
          'Parameter: '+'Source' +#13,
          mtError, [mbIgnore, mbAbort], 0
        ) = mrAbort
      then
        Raise EAccessViolation.Create('Source parameter isn''t assigned');
    {$Endif}

  if Source is jGradientPoint then
  Begin
    S:= Source as jGradientPoint;

    FColor:= S.Color;
    FOpacity:= S.Opacity;
    FXPercent:= S.XPercent;
    FYPercent:= S.YPercent;

    DoChanged;
  End Else
    Inherited;
End;

Procedure jGradientPoint.DoCreate;
Begin
  Inherited;

  FColor:= clWhite;
  FOpacity:= 255;
  FXPercent:= 0;
  FYPercent:= 0;
End;

Procedure jGradientPoint.SetColor(Value: TColor);
Begin
  if FColor = Value then Exit;

  FColor:= Value;

  DoChanged;
End;

Procedure jGradientPoint.SetOpacity(Value: Int8U);
Begin
  if FOpacity = Value then Exit;

  FOpacity:= Value;

  DoChanged;
End;

Procedure jGradientPoint.SetXPercent(Value: Single);
Begin
  if FXPercent = Value then Exit;

  FXPercent:= Value;

  DoChanged;
End;

Procedure jGradientPoint.SetYPercent(Value: Single);
Begin
  if FYPercent = Value then Exit;

  FYPercent:= Value;

  DoChanged;
End;











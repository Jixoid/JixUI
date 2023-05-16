Procedure jJixBorder.DoCreate;
Begin
  Inherited;

  FExColor:= clWhite;
  FExOpacity:= 255;
  FExWidth:= 1;
  FInColor:= clWhite;
  FInOpacity:= 255;
  FInWidth:= 0;
  FStyle:= jbsSolid;
End;

Procedure jJixBorder.Assign(Source: jJixProperty);
Var
  S: jJixBorder;
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

  if Source is jJixBorder then
  Begin
    S:= Source as jJixBorder;

    FExColor:=   S.ExColor;
    FExOpacity:= S.ExOpacity;
    FExWidth:=   S.ExWidth;
    FInColor:=   S.InColor;
    FInOpacity:= S.InOpacity;
    FInWidth:=   S.InWidth;
    FStyle:=     S.Style;

    DoChanged;
  End Else
    Inherited;
End;

Procedure jJixBorder.SetExColor(Value: TColor);
Begin
  If FExColor = Value then Exit;

  FExColor:= Value;

  DoChanged;
End;

Procedure jJixBorder.SetExOpacity(Value: Int8U);
Begin
  If FExOpacity = Value then Exit;

  FExOpacity:= Value;

  DoChanged;
End;

Procedure jJixBorder.SetExWidth(Value: Int8U);
Begin
  If FExWidth = Value then Exit;

  FExWidth:= Value;

  DoChanged;
End;

Procedure jJixBorder.SetInColor(Value: TColor);
Begin
  If FInColor = Value then Exit;

  FInColor:= Value;

  DoChanged;
End;

Procedure jJixBorder.SetInOpacity(Value: Int8U);
Begin
  If FInOpacity = Value then Exit;

  FInOpacity:= Value;

  DoChanged;
End;

Procedure jJixBorder.SetInWidth(Value: Int8U);
Begin
  If FInWidth = Value then Exit;

  FInWidth:= Value;

  DoChanged;
End;

Procedure jJixBorder.SetStyle(Value: eJixBorderStyle);
Begin
  If FStyle = Value then Exit;

  FStyle:= Value;

  DoChanged;
End;

Function jJixBorder.GetExColor: jARGB;
Begin
  Result:= FExColor;
  Result.Alpha:= FExOpacity;
End;

Function jJixBorder.GetInColor: jARGB;
Begin
  Result:= FInColor;
  Result.Alpha:= FInOpacity;
End;



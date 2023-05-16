Procedure jJixRounding.DoCreate;
Begin
  Inherited;

  FRoundX:= 8;
  FRoundY:= 8;
  FOptions:= [];
End;

Procedure jJixRounding.Assign(Source: jJixProperty);
Var
  S: jJixRounding;
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

  if Source is jJixRounding then
  Begin
    S:= Source as jJixRounding;

    FRoundX:= S.RoundX;
    FRoundY:= S.RoundY;
    FOptions:= S.Options;

    DoChanged;
  End Else
    Inherited;
End;


Procedure jJixRounding.SetRoundX(Value: Int8U);
Begin
  if FRoundX = Value then Exit;

  FRoundX:= Value;

  DoChanged;
End;

Procedure jJixRounding.SetRoundY(Value: Int8U);
Begin
  if FRoundY = Value then Exit;

  FRoundY:= Value;

  DoChanged;
End;

Procedure jJixRounding.SetOptions(Value: TRoundRectangleOptions);
Begin
  if FOptions = Value then Exit;

  FOptions:= Value;

  DoChanged;
End;



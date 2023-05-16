Procedure jJixState.DoCreate;
Begin
  Inherited;

  Border:= jJixBorder.Create(Self);
  Background:= jJixBackground.Create(Self);
  FontEx:= jJixFontEx.Create(Self);
  Rounding:= jJixRounding.Create(Self);
End;

Destructor jJixState.Destroy;
Begin
  Border.Free;
  Background.Free;
  FontEx.Free;
  Rounding.Free;

  Inherited;
End;

Procedure jJixState.Assign(Source: jJixProperty);
Var
  S: jJixState;
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

  if Source is jJixState then
  Begin
    S:= Source as jJixState;

    Border.Assign(S.Border);
    Background.Assign(S.Background);
    FontEx.Assign(S.FontEx);
    Rounding.Assign(S.Rounding);

    DoChanged;
  End Else
    Inherited;
End;








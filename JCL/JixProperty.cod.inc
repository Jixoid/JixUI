Constructor jJixProperty.Create;
Begin
  MyParent:= Nil;

  OnChanged:= Nil;
  DoCreate;
End;

Constructor jJixProperty.Create(Parent: jJixProperty); Overload;
Begin
  MyParent:= Parent;

  OnChanged:= Nil;
  DoCreate;
End;

Procedure jJixProperty.Assign(Source: jJixProperty);
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

  DoChanged;
End;

Procedure jJixProperty.DoCreate;
Begin
  OnChanged:= Nil;
End;

Procedure jJixProperty.DoChanged; Inline;
Begin
  Try
    if Assigned(OnChanged) then
      OnChanged(Self);

    if Assigned(MyParent) then
      MyParent.DoChanged;
  Except
  End;
End;



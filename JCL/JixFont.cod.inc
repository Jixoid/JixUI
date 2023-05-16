Procedure jJixFontEx.DoCreate;
Begin
  Inherited;

  FColor:= clWhite;
  FDisColor:= clLtGray;
  FOpacity:= 255;
  FHeight:= 12;
  FName:= 'default';
  FPadL:= 0;
  FPadT:= 0;
  FPadR:= 0;
  FPadB:= 0;
  FSingleLine:= True;
  FStyle:= [];
  FWordBreak:= False;
  FAlignment:= taCenter;
  FLayout:= tlCenter;
End;

Procedure jJixFontEx.Assign(Source: jJixProperty);
Var
  S: jJixFontEx;
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

  if Source is jJixFontEx then
  Begin
    S:= Source as jJixFontEx;

    FColor:= S.Color;
    FDisColor:= S.DisabledColor;
    FOpacity:= S.Opacity;
    FHeight:= S.Height;
    FName:= S.Name;
    FPadL:= S.PaddingLeft;
    FPadT:= S.PaddingTop;
    FPadR:= S.PaddingRight;
    FPadB:= S.PaddingBottom;
    FSingleLine:= S.SingleLine;
    FStyle:= S.Style;
    FWordBreak:= S.WordBreak;
    FAlignment:= S.Alignment;
    FLayout:= S.Layout;

    DoChanged;
  End Else
    Inherited;
End;

Procedure jJixFontEx.SetColor(Value: TColor);
Begin
  if FColor = Value then Exit;

  FColor:= Value;

  DoChanged;
End;

Procedure jJixFontEx.SetDisColor(Value: TColor);
Begin
  if FDisColor = Value then Exit;

  FDisColor:= Value;

  DoChanged;
End;

Procedure jJixFontEx.SetOpacity(Value: Int8U);
Begin
  if FOpacity = Value then Exit;

  FOpacity:= Value;

  DoChanged;
End;

Procedure jJixFontEx.SetHeight(Value: Int16U);
Begin
  if FHeight = Value then Exit;

  FHeight:= Value;

  DoChanged;
End;

Procedure jJixFontEx.SetName(Value: String);
Begin
  if FName = Value then Exit;

  FName:= Value;

  DoChanged;
End;

Procedure jJixFontEx.SetPadL(Value: Int16S);
Begin
  if FPadL = Value then Exit;

  FPadL:= Value;

  DoChanged;
End;

Procedure jJixFontEx.SetPadT(Value: Int16S);
Begin
  if FPadT = Value then Exit;

  FPadT:= Value;

  DoChanged;
End;

Procedure jJixFontEx.SetPadR(Value: Int16S);
Begin
  if FPadR = Value then Exit;

  FPadR:= Value;

  DoChanged;
End;

Procedure jJixFontEx.SetPadB(Value: Int16S);
Begin
  if FPadB = Value then Exit;

  FPadB:= Value;

  DoChanged;
End;

Procedure jJixFontEx.SetSingleLine(Value: Boolean);
Begin
  if FSingleLine = Value then Exit;

  FSingleLine:= Value;

  DoChanged;
End;

Procedure jJixFontEx.SetStyle(Value: TFontStyles);
Begin
  if FStyle = Value then Exit;

  FStyle:= Value;

  DoChanged;
End;

Procedure jJixFontEx.SetWordBreak(Value: Boolean);
Begin
  if FWordBreak = Value then Exit;

  FWordBreak:= Value;

  DoChanged;
End;

Procedure jJixFontEx.SetAlignment(Value: TAlignment);
Begin
  if FAlignment = Value then Exit;

  FAlignment:= Value;

  DoChanged;
End;

Procedure jJixFontEx.SetLayout(Value: TTextLayout);
Begin
  if FLayout = Value then Exit;

  FLayout:= Value;

  DoChanged;
End;







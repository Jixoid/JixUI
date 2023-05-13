Constructor jFormManager.Create;
Begin
  FormList:= jList<TForm>.Create;
  FormItem:= jList<jItem>.Create;

  CompTemps:= jList<TJixTemplate>.Create;

  Controller:= TTimer.Create(Nil);
  Controller.Interval:= 100;
  Controller.OnTimer:= TimerEvent;
End;

Destructor jFormManager.Destroy;
Begin
  Controller.Free;
  FormList.Free;
  FormItem.Free;

  CompTemps.Free;

  Inherited;
End;

Procedure jFormManager.AddForm(Form: TForm);
Var
  Cac: jItem;
Begin
  if not Assigned(Form) then
    {$IfOpt D+}
      if
        MessageDlg(
          'Access Violation',
          'Form parameter isn''t assigned' +#13+#13+
          'Class: '+Self.ClassName +#13+
          'Method: '+'AddForm' +#13+
          'Parameter: '+'Form' +#13,
          mtError, [mbIgnore, mbAbort], 0
        ) = mrAbort
      then
        Raise EAccessViolation.Create('Form parameter isn''t assigned');
    {$Else}
      if
        MessageDlg(
          'Access Violation', '',
          mtError, [mbIgnore, mbAbort], 0
        ) = mrAbort
      then
        Halt;
    {$IfEnd}

  Cac:= jItem.Create;
  Cac.Color:= Form.Color;
  Cac.Focus:= Form.Active;
  Cac.BIcons:= [biSystemMenu, biMaximize, biMinimize];

  if not FormList.Exists(Form) then
  Begin
    FormList.Add(Form);
    FormItem.Add(Cac);
  End;
End;

Procedure jFormManager.DeleteForm(Form: TForm);
Var
  i: Integer;
Begin
  if not Assigned(Form) then
    {$IfOpt D+}
      if
        MessageDlg(
          'Access Violation',
          'Form parameter isn''t assigned' +#13+#13+
          'Class: '+Self.ClassName +#13+
          'Method: '+'DeleteForm' +#13+
          'Parameter: '+'Form' +#13,
          mtError, [mbIgnore, mbAbort], 0
        ) = mrAbort
      then
        Raise EAccessViolation.Create('Form parameter isn''t assigned');
    {$Else}
      if
        MessageDlg(
          'Access Violation', '',
          mtError, [mbIgnore, mbAbort], 0
        ) = mrAbort
      then
        Halt;
    {$IfEnd}

  i:= FormList.Find(Form);

  if i <> -1 then
  Begin
    FormList.Delete(i);
    FormItem[i].Free;
    FormItem.Delete(i);
  End;
End;

Procedure jFormManager.TimerEvent(Sender: TObject);

  Function FindTemplate(Text: String): jTemplateCollectionItem;
  Var
    i,j: Integer;
  Begin
    Result:= Nil;

    For i:= 0 to Templates.Count-1 do
      For j:= 0 to Templates.Items[i].Templates.Count-1 do
        if UpperCase(Text) = UpperCase(jTemplateCollectionItem(Templates.Items[i].Templates.Items[j]).ThemeName) then
        Begin

          Result:= Templates.Items[i].Templates.Items[j] as jTemplateCollectionItem;
          Exit;
        End;

  End;

  Function GetAllChild(Comp: TWinControl): TArray<TControl>;
  Var
    i: Integer;
  Begin
    Result:= [];

    if not Assigned(Comp) then Exit;

    For i:= 0 to Comp.ControlCount-1 do
    Begin
      if Comp.Controls[i] is jJixComponent then
        Result+= [Comp.Controls[i]];

      if Comp.Controls[i] is TWinControl then
        Result+= GetAllChild(Comp.Controls[i] as TWinControl);
    End;
  End;

  Procedure SetTheme(Form: TForm; FormIndex: Integer);
  Var
    Comp: TControl;

    This: jJixForm;
    SColor: jARGB;
    SFocus: Boolean;
    SBIcons: TBorderIcons;

    Temp: jTemplateCollectionItem;
  Begin
    With Form do begin

      SColor:= FindFormColor(Form);

      if Form is jJixForm then
        with Form as jJixForm do
        Begin
          This:= jJixForm(Form);

          Temp:= FindTemplate(This.ThemeName);

          SFocus:= Active;
          SBIcons:= BorderIcons;

          if Assigned(Temp) then
            Color:= Temp.BackgroundColor;

          Title.Custom_Caption.Background.Gradient1.StartColor:= FindFormTitleColor(This);
          Title.Custom_Caption.FontEx.Color:= FindFontColor(ColorToARGB(Title.Custom_Caption.Background.Gradient1.StartColor));
          Title.Custom_Caption.FontEx.ShadowColor:= Title.Custom_Caption.FontEx.Color;

          if (SBIcons <> FormItem[FormIndex].BIcons) then
          Begin
            FormItem[FormIndex].BIcons:= SBIcons;

            Title.Custom_Halt.Visible:= (biSystemMenu in BorderIcons);
            Title.Custom_Maximize.Visible:= (biMaximize in BorderIcons);
            Title.Custom_Minimize.Visible:= (biMinimize in BorderIcons);
            Title.Custom_Help.Visible:= (biHelp in BorderIcons);

            Title.TextPosition:= Title.TextPosition;
          End;

          if (SFocus  <> FormItem[FormIndex].Focus) then
          Begin
            FormItem[FormIndex].Focus:= SFocus;

            if Active then
            Begin
              if Assigned(WhenFocus) then WhenFocus(This);
            End Else
              if Assigned(WhenDeFocus) then WhenDeFocus(This);

          End;
        End;

      // Normal Theme

      if (SColor  <> FormItem[FormIndex].Color) then begin
        FormItem[FormIndex].Color:= SColor;

        For Comp in GetAllChild(Form) do // Scan All JixComponent
        Begin
          Temp:= FindTemplate((Comp as jJixComponent).ThemeName);

          if Assigned(Temp) then
          Begin
            Temp.PaintTemplate(SColor, DarkTheme);
            (Comp as jJixComponent).SetTemplate(Temp);
          End;
        End;

      End;

    End;
  End;

Var
  i: Integer;
Begin
  For i:= 0 to FormList.Count-1 do
    if Assigned(TForm(FormList[i])) then
      if TForm(FormList[i]).Showing then
        SetTheme(FormList[i], i);

  //For i:= 0 to Screen.FormCount-1 do
  //  if not FormList.Exists(Screen.Forms[i]) then
  //    FormList.Add(Screen.Forms[i]);

End;

Function jFormManager.FindFormTitleColor(Form: TForm): jARGB;
Begin
  if MiniJixoid_JCL.IsWindowFrameEnabled then
    if Form.Active then
      Result:= MiniJixoid_JCL.GetThemeColor
    Else
      Result:= MiniJixoid_JCL.GetThemeColorInactive

  Else
    Result:= clWhite;
End;

Function jFormManager.FindFormColor(Form: TForm): jARGB;
Begin
  if Form.Active then
    Result:= ActiveColor
  Else
    if InActiveColor <> clWhite then
      Result:= InActiveColor
    Else
      Result:= $00555555;
End;

Function jFormManager.GetActiveColor: jARGB;
Begin
  if Assigned(GetterActiveColor) then
    Result:= GetterActiveColor()
  Else
    Result:= MiniJixoid_JCL.GetThemeColor;
End;

Function jFormManager.GetInActiveColor: jARGB;
Begin
  if Assigned(GetterInActiveColor) then
    Result:= GetterInActiveColor()
  Else
    Result:= MiniJixoid_JCL.GetThemeColorInactive;
End;

Function jFormManager.GetDarkTheme: Boolean;
Begin
  if Assigned(GetterDarkTheme) then
    Result:= GetterDarkTheme()
  Else
    Result:= MiniJixoid_JCL.GetDarkTheme;
End;

Procedure jFormManager.AddTemplate(Templates: TJixTemplate);
Begin
  CompTemps.Add(Templates);
End;

Procedure jFormManager.ForceReSet(Form: TForm);
Var
  i: Integer;
Begin

  i:= FormList.Find(Form);

  if i <> -1 then
  With FormItem.Items[i] do
  Begin
    Color.Red:= Color.Red +1;
    Focus:= not Focus;

  End;
End;

Procedure jFormManager.ForceReSetAll;
Var
  i: Integer;
Begin

  For i:= 0 to FormList.Count-1 do
    With FormItem.Items[i] do
    Begin
      Color.Red:= Color.Red +1;
      Focus:= not Focus;

    End;
End;



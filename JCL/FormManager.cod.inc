Constructor jFormManager.Create;
Begin
  FormList:= TList.Create;
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

Procedure jFormManager.AddForm(Form: jCustomJixForm);
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

  FormList.Add(Form);
  FormItem.Add(Cac);
End;

Procedure jFormManager.DeleteForm(Form: jCustomJixForm);
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

  i:= FormList.IndexOf(Form);

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

  Function GetAllChild(Comp: TWinControl; IsParentOfProcess: Boolean = True): TArray<TControl>;
  Var
    i: Integer;
  Begin
    if IsParentOfProcess then
      Result:= [];

    For i:= 0 to Comp.ControlCount-1 do
    Begin
      Result+= [Comp.Controls[i]];

      if Comp.Controls[i] is TWinControl then
      Begin
        GetAllChild(Comp.Controls[i] as TWinControl, False);
      End;

    End;
  End;

Var
  Comp: TControl;

  i, j1,j2: Integer;
  This: jJixForm;
  SColor: jARGB;
  SFocus: Boolean;
  SBIcons: TBorderIcons;

  Temp: jTemplateCollectionItem;
Begin
  For i:= 0 to FormList.Count-1 do
    if Assigned(jJixForm(FormList[i])) then
    if jJixForm(FormList[i]).Showing then
    With jJixForm(FormList[i]) do
      if ActiveTheme then begin

        This:= jJixForm(FormList[i]);

        SColor:= FindFormColor(This);
        SFocus:= Active;
        SBIcons:= BorderIcons;

        Title.Custom_Caption.Background.Gradient1.StartColor:= FindFormTitleColor(FormList[i]);
        Title.Custom_Caption.FontEx.Color:= FindFontColor(ColorToARGB(Title.Custom_Caption.Background.Gradient1.StartColor));
        Title.Custom_Caption.FontEx.ShadowColor:= Title.Custom_Caption.FontEx.Color;

        if (SBIcons <> FormItem[i].BIcons) then begin

          FormItem[i].BIcons:= SBIcons;

          Title.Custom_Halt.Visible:= (biSystemMenu in BorderIcons);
          Title.Custom_Maximize.Visible:= (biMaximize in BorderIcons);
          Title.Custom_Minimize.Visible:= (biMinimize in BorderIcons);
          Title.Custom_Help.Visible:= (biHelp in BorderIcons);

          Title.TextPosition:= Title.TextPosition;
        End;

        if (SColor  <> FormItem[i].Color) then begin
          FormItem[i].Color:= SColor;

          For Comp in GetAllChild(This) do // Scan All Component
            if Comp is jBGRAComponent then begin // Is Jix Component

              if Comp is TJixButton then begin

                Temp:= FindTemplate(TJixButton(Comp).ThemeName);

                if Assigned(Temp) then
                Begin
                  Temp.PaintTemplate(SColor, True);
                  JixAPI.SetTheme(Comp as TJixButton, Temp);
                End;


              End;

            End;

        End;

        if (SFocus  <> FormItem[i].Focus) then begin

          FormItem[i].Focus:= SFocus;

          if Active then
          Begin
            if Assigned(WhenFocus) then WhenFocus(This);
          End Else
            if Assigned(WhenDeFocus) then WhenDeFocus(This);

        End;

      End;
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





unit MiniJixoid_JCL;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms, SysUtils, StrUtils, Controls, Registry, Graphics, BCButton,
  JixPas;

function Scaling(bForm: TCustomDesignControl; Value: Double): Double;
function ScalingBorder(bForm: TCustomDesignControl): Integer;

function GetDarkTheme: boolean;
function GetThemeColor: jARGB;
function GetThemeColorInactive: jARGB;
Function IsWindowFrameEnabled: Boolean;

Procedure ButtonRadialEffect(Button: TBCButton; X, Y: Int32S);

Function FindFontColor(bColor: jARGB): jARGB;

implementation

Procedure ButtonRadialEffect(Button: TBCButton; X, Y: Int32S);
Var
  r: Int8U;
  VarX,
  VarY: Int32S;
begin
  r:= Round(100/Button.Width*20);
  VarX:= Round(X/Button.Width*100);
  VarY:= Round(Y/Button.Height*100);

  Button.StateClicked.Background.Gradient1.Point2XPercent:= VarX-r;
  Button.StateClicked.Background.Gradient1.Point2YPercent:= VarY-r;

  Button.StateClicked.Background.Gradient1.Point1XPercent:= VarX;
  Button.StateClicked.Background.Gradient1.Point1YPercent:= VarY;
end;

Function GetDarkTheme: boolean;
const
  KEYPATH = '\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize';
  KEYNAME = 'AppsUseLightTheme';
var
  LightKey: boolean;
  Registry: TRegistry;
begin
  Result := True;
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKeyReadOnly(KEYPATH) then
      begin
        if Registry.ValueExists(KEYNAME) then
          LightKey := Registry.ReadBool(KEYNAME)
        else
          LightKey := False;
      end
    else
      LightKey := False;
    Result := not LightKey
  finally
    Registry.Free;
  end;
end;


function Scaling(bForm: TCustomDesignControl; Value: Double): Double;
Begin
  Result:= bForm.PixelsPerInch/96*Value;
end;

function ScalingBorder(bForm: TCustomDesignControl): Integer;
Begin
  Result:= Round(((bForm.PixelsPerInch/96)-1)*4);

  if Result <= 0 then Result:= 1;
end;

Function FindFontColor(bColor: jARGB): jARGB;
Var
  luminance: Double;
Begin
  luminance:= ((0.299 * bColor.Red) + (0.587 * bColor.Green) + (0.114 * bColor.Blue)) / 255;

  if (luminance > 0.5) then
     Result:= Colors.Black
  else
     Result:= Colors.White;

end;

function GetThemeColor: jARGB;
const
  KEYPATH = '\Software\Microsoft\Windows\DWM';
  KEYNAME = 'AccentColor';
var
  Cac: String;
  ColorKey: String;
  Registry: TRegistry;
begin
  //Result:= $555555;
  //Exit;

  Result := $003357FF;
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKeyReadOnly(KEYPATH) then
      begin
        if Registry.ValueExists(KEYNAME) then
          ColorKey := Registry.ReadInteger(KEYNAME).ToHexString
        else
          ColorKey := 'FF3357FF';
      end
    else
      ColorKey := 'FF3357FF';
  finally
    Registry.Free;
  end;

  Cac:= ReverseString(ColorKey);
  SetLength(Cac, Length(Cac)-2);
  Cac:= ReverseString(Cac+'00');

  Result:= StringToColor('$'+Cac);
end;

function GetThemeColorInactive: jARGB;
Const
  KEYPATH = '\Software\Microsoft\Windows\DWM';
  KEYNAME = 'AccentColorInactive';
Var
  Cac: String;
  ColorKey: String;
  Registry: TRegistry;
Begin
  Result := $00000000;
  Registry := TRegistry.Create;
  Try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKeyReadOnly(KEYPATH) then
    Begin
      if Registry.ValueExists(KEYNAME) then
        ColorKey := Registry.ReadInteger(KEYNAME).ToHexString
      Else
      Begin
        Result:= Colors.White;
        Exit;
      End;
    End Else
    Begin
      Result:= Colors.White;
      Exit;
    End;
  Finally
    Registry.Free;
  End;

  Cac:= ReverseString(ColorKey);
  SetLength(Cac, Length(Cac)-2);
  Cac:= ReverseString(Cac+'00');

  Result:= StringToColor('$'+Cac);
End;

Function IsWindowFrameEnabled: Boolean;
const
  KEYPATH = '\Software\Microsoft\Windows\DWM';
  KEYNAME = 'ColorPrevalence';
var
  Prevalence: boolean;
  Registry: TRegistry;
begin
  Result := True;
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKeyReadOnly(KEYPATH) then
      begin
        if Registry.ValueExists(KEYNAME) then
          Prevalence := Registry.ReadBool(KEYNAME)
        else
          Prevalence := False;
      end
    else
      Prevalence := False;
    Result := Prevalence
  finally
    Registry.Free;
  end;
end;

end.


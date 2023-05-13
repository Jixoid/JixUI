{
Supporting
  -32bit
  -64bit
  -128bit
  -256bit
}

unit JixPas;

{$Mode ObjFpc}{$H+}{$M+}{$ModeSwitch TypeHelpers}

interface

Uses
  Classes, Graphics, StrUtils, SysUtils, Math;

{%Region Ints}
Type
  IntPer = {0..100}0..100;
  pIntPer = {Pointer of IntPer}^IntPer;

  Int8U = {0..255}Byte;
  pInt8U = {Pointer of Int8U}^Int8U;

  Int8S = {-128..127}ShortInt;
  pInt8S = {Pointer of Int8S}^Int8S;

  Int16U = {0..65535}Word;
  pInt16U = {Pointer of Int16U}^Int16U;

  Int16S = {-32768..32767}SmallInt;
  pInt16S = {Pointer of Int16S}^Int16S;

  Int32U = {0..4294967295}LongWord;
  pInt32U = {Pointer of Int32U}^Int32U;

  Int32S = {-2147483648..2147483647}LongInt;
  pInt32S = {Pointer of Int32S}^Int32S;

  Int64U = {0..18446744073709551615}QWord;
  pInt64U = {Pointer of Int64U}^Int64U;

  Int64S = {-9223372036854775808..9223372036854775807}Int64;
  pInt64S = {Pointer of tInt64S}^Int64S;

  Int128U = {0..340282366920938463463374607431768211455}
    {$if defined(CPU64)}
      Array[0..1] Of Int64U;
    {$else}
      Array[0..3] Of Int32U;
    {$ifend}
  pInt128U = {Pointer of Int128U}^Int128U;

  Int128S = {-170141183460469231731687303715884105727..170141183460469231731687303715884105727}
    {$if defined(CPU64)}
      Array[0..1] Of Int64S;
    {$else}
      Array[0..3] Of Int32S;
    {$ifend}
  pInt128S = {Pointer of Int128S}^Int128S;

  Int256U = {0..115792089237316195423570985008687907853269984665640564039457584007913129639935}
    {$if defined(CPU128)}
      Array[0..1] of Int128U;
    {$elseif defined(CPU64)}
      Array[0..3] of Int64U;
    {$else}
      Array[0..7] of Int32U;
    {$ifend}
  pInt256U = {Pointer of Int256U}^Int256U;

  Int256S = {-57896044618658097711785492504343953926634992332820282019728792003956564819968..57896044618658097711785492504343953926634992332820282019728792003956564819967}
    {$if defined(CPU128)}
      Array[0..1] of Int128S;
    {$elseif defined(CPU64)}
      Array[0..3] of Int64S;
    {$else}
      Array[0..7] of Int32S;
    {$ifend}
  pInt256S = {Pointer of Int256S}^Int256S;

  //Integer =
  //  {$if     defined(CPU256)} Int256S
  //  {$elseif defined(CPU128)} Int128S
  //  {$elseif defined(CPU64)}  Int64S
  //  {$else} Int32S
  //  {$ifend};
  //
  SavInteger =
    {$if     defined(CPU256)} Int128S
    {$elseif defined(CPU128)} Int64S
    {$elseif defined(CPU64)}  Int32S
    {$else} Int16S
    {$ifend};

{%endregion}
  
{%Region Colors}
Type
  jARGB = Packed Record
   Red,
   Green,
   Blue,
   Alpha: Int8U;
 end;

 hARGB = type Helper for jARGB
   Public
     Function ReSetAlpha(NewAlpha: Int8U): jARGB;
     Function Mix(Color: jARGB): jARGB;
     Function Mix(Color: jARGB; Percent: Byte): jARGB;
 end;

Operator := (in1: jARGB): TColor;
Operator := (in1: TColor): jARGB;
Operator =  (in1:jARGB; in2: jARGB): Boolean;
Operator =  (in1:jARGB; in2: TColor): Boolean;
Operator =  (in1:TColor; in2: jARGB): Boolean;
Operator <> (in1:jARGB; in2: jARGB): Boolean;
Operator <> (in1:jARGB; in2: TColor): Boolean;
Operator <> (in1:TColor; in2: jARGB): Boolean;

Function ARGBToColor(Color: jARGB): TColor;
Function ColorToARGB(Color: TColor; IgnoreAlpha: Boolean = True): jARGB;

Function CreateColor(R,G,B: Byte; A: Byte = 00): jARGB;



Type
  jColors = record
    Black,
    Maroon,
    Green,
    Olive,
    Navy,
    Purple,
    Teal,
    Gray,
    Silver,
    Red,
    Lime,
    Yellow,
    Blue,
    Fuchsia,
    Aqua,
    LtGray,
    DkGray,
    White,

    // extended colors
    MoneyGreen,
    SkyBlue,
    Cream,
    MedGray,

    // special colors
    None,
    Default: jARGB;
  end;

Var
  Colors: jColors;

{%endregion}

{%Region Helper}
{
Type
  hDouble = Type Helper For Double
    Public
      Function  Round: Int64S;
      Function  RoundTo(Digits: TRoundToRange): Double;

      Function  ToString: String;
      Function  ToString(FormatSettings: TFormatSettings): String;

      Function  isPositive: Boolean;
      Function  isNegative: Boolean;

      Function  isNil: Boolean;
      Procedure doNil;
  End;
}
{%EndRegion}

Type
  mBasicProcedure = Procedure of object;

  mBasicString = Function: String of object;
  mBasicUnicodeString = Function: UnicodeString of object;

  mBasicInt8U = Function: Int8U of object;
  mBasicInt8S = Function: Int8S of object;
  mBasicInt16U = Function: Int16U of object;
  mBasicInt16S = Function: Int16S of object;
  mBasicInt32U = Function: Int32U of object;
  mBasicInt32S = Function: Int32S of object;
  mBasicInt64U = Function: Int64U of object;
  mBasicInt64S = Function: Int64S of object;
  mBasicInt128U = Function: Int128U of object;
  mBasicInt128S = Function: Int128S of object;
  mBasicInt256U = Function: Int256U of object;
  mBasicInt256S = Function: Int256S of object;
  mBasicInteger = Function: Integer of object;

  mBasicBoolean = Function: Boolean of object;
  mBasicColor = Function: TColor of object;
  mBasicARGB = Function: jARGB of object;

  mBasicClass = Function: TClass of object;
  mBasicObject = Function: TObject of object;

implementation

{%Region Colors}
Function hARGB.ReSetAlpha(NewAlpha: Int8U): jARGB;
Begin
  Result:= Self;
  Result.Alpha:= NewAlpha;
End;

Function hARGB.Mix(Color: jARGB): jARGB;
Begin
  Result.Alpha:= Round((Self.Alpha+Color.Alpha)/2);
  Result.Red:=   Round((Self.Red+Color.Red)/2);
  Result.Green:= Round((Self.Green+Color.Green)/2);
  Result.Blue:=  Round((Self.Blue+Color.Blue)/2);
End;

Function hARGB.Mix(Color: jARGB; Percent: Byte): jARGB;
Begin
  Result.Alpha:= Round(((Color.Alpha *Percent) /255) +((Self.Alpha *(255 -Percent)) /255));
  Result.Red:=   Round(((Color.Red   *Percent) /255) +((Self.Red   *(255 -Percent)) /255));
  Result.Green:= Round(((Color.Green *Percent) /255) +((Self.Green *(255 -Percent)) /255));
  Result.Blue:=  Round(((Color.Blue  *Percent) /255) +((Self.Blue  *(255 -Percent)) /255));
End;

Operator := (in1: jARGB): TColor;
Begin
  Result:= ARGBToColor(in1);
end;

Operator := (in1: TColor): jARGB;
Begin
  Result:= ColorToARGB(in1);
end;

Operator = (in1:jARGB; in2: jARGB): Boolean;
Begin
  Result:=
    (in1.Red   = in2.Red) and
    (in1.Green = in2.Green) and
    (in1.Blue  = in2.Blue) and
    (in1.Alpha = in2.Alpha);
end;

Operator = (in1:jARGB; in2: TColor): Boolean;
Var
  Cac: jARGB;
Begin
  Cac:= ColorToARGB(in2);

  Result:=
    (in1.Red   = Cac.Red) and
    (in1.Green = Cac.Green) and
    (in1.Blue  = Cac.Blue) and
    (in1.Alpha = Cac.Alpha);
end;

Operator = (in1:TColor; in2: jARGB): Boolean;
Begin
  Result:=
    (ColorToARGB(in1).Red   = in2.Red) and
    (ColorToARGB(in1).Green = in2.Green) and
    (ColorToARGB(in1).Blue  = in2.Blue) and
    (ColorToARGB(in1).Alpha = in2.Alpha);
end;

Operator <> (in1:jARGB; in2: jARGB): Boolean;
Begin
  Result:= not(in1 = in2);
End;

Operator <> (in1:jARGB; in2: TColor): Boolean;
Begin
  Result:= not(in1 = in2);
end;

Operator <> (in1:TColor; in2: jARGB): Boolean;
Begin
  Result:= not(in1 = in2);
end;

Function ARGBToColor(Color: jARGB): TColor;
Begin
  Result:= RGBToColor(
    Color.Red,
    Color.Green,
    Color.Blue
  );
end;

Function ColorToARGB(Color: TColor; IgnoreAlpha: Boolean = True): jARGB;
Var
  Cac,
  C: String;
Begin
  // Prepare
  Cac:= IntToHex(ColorToRGB(Color));

  // ===> Alpha
  C:= Cac;
  SetLength(C, 2);
  Result.Alpha:= Hex2Dec(C);
  Cac:= ReverseString(Cac);
  SetLength(Cac, Length(Cac)-2);
  Cac:= ReverseString(Cac);

  // ===> Blue
  C:= Cac;
  SetLength(C, 2);
  Result.Blue:= Hex2Dec(C);

  // ===> Green
  C:= ReverseString(Cac);
  SetLength(C, Length(C)-2);
  C:= ReverseString(C);
  SetLength(C, Length(C)-2);
  Result.Green:= Hex2Dec(C);

  // ===> Red
  C:= ReverseString(Cac);
  SetLength(C, Length(C)-4);
  C:= ReverseString(C);
  Result.Red:= Hex2Dec(C);

  if IgnoreAlpha then
    Result.Alpha:= $FF;
end;

Function CreateColor(R,G,B: Byte; A: Byte = 00): jARGB;
Begin
  Result.Red:= R;
  Result.Green:= G;
  Result.Blue:= B;
  Result.Alpha:= A;
end;

Procedure ColorInit;
Begin
  {%region Colors}
  With Colors do
  Begin
    Black   := ColorToARGB(clBlack);
    Maroon  := ColorToARGB(clMaroon);
    Green   := ColorToARGB(clGreen);
    Olive   := ColorToARGB(clOlive);
    Navy    := ColorToARGB(clNavy);
    Purple  := ColorToARGB(clPurple);
    Teal    := ColorToARGB(clTeal);
    Gray    := ColorToARGB(clGray);
    Silver  := ColorToARGB(clSilver);
    Red     := ColorToARGB(clRed);
    Lime    := ColorToARGB(clLime);
    Yellow  := ColorToARGB(clYellow);
    Blue    := ColorToARGB(clBlue);
    Fuchsia := ColorToARGB(clFuchsia);
    Aqua    := ColorToARGB(clAqua);
    LtGray  := ColorToARGB(clLtGray);
    DkGray  := ColorToARGB(clDkGray);
    White   := ColorToARGB(clWhite);


    // extended colors
    MoneyGreen := ColorToARGB(clMoneyGreen);
    SkyBlue    := ColorToARGB(clSkyBlue);
    Cream      := ColorToARGB(clCream);
    MedGray    := ColorToARGB(clMedGray);

    // special colors
    None    := ColorToARGB($FFFFFFFF);
  End;
  {%endregion}
End;

{%EndRegion}

{%Region Helper}
{

Function  hDouble.Round: Int64S;
Begin
  Result:= System.Round(Self);
End;

Function  hDouble.RoundTo(Digits: TRoundToRange): Double;
Begin
  Result:= Math.RoundTo(Self, Digits);
end;

Function  hDouble.ToString: String;
Begin
  Result:= FloatToStr(Self);
end;

Function  hDouble.ToString(FormatSettings: TFormatSettings): String;
Begin
  Result:= FloatToStr(Self, FormatSettings);
end;

Function  hDouble.isPositive: Boolean;
Begin
  Result:= (Self > 0);
end;

Function  hDouble.isNegative: Boolean;
Begin
  Result:= (Self < 0);
end;

Function  hDouble.isNil: Boolean;
Begin
  Result:= (Self = 0);
end;

Procedure hDouble.doNil;
Begin
  Self:= 0;
end;

}
{%EndRegion}

initialization
Begin
  ColorInit;
End;

end.


{  $Id$  }
{
JixPas
|
|- Ver: 1.8
|- Status: Beta
|- Meaning: Jixoid Helper Types
}
unit JCL.Utils;

{$Mode ObjFpc}{$H+}{$M+}{$Inline On}{$Macro On}{$ModeSwitch TypeHelpers}

interface

Uses
  Classes, SysUtils, Graphics, Controls, Forms, StrUtils,
  JCL.Generics;

{%Region Ints}
Type
  IntPer  = {0..100}0..100;
  pIntPer = {Pointer of IntPer}^IntPer;
  aIntPer = {Array of IntPer}Array Of IntPer;
  lIntPer = {List of IntPer}Specialize jList<IntPer>;

  Int4U  = {0..15}0..15;
  pInt4U = {Pointer of Int4U}^Int4U;
  aInt4U = {Array of Int4U}Array Of Int4U;
  lInt4U = {List of Int4U}Specialize jList<Int4U>;

  Int4S  = {-8..7}-8..7;
  pInt4S = {Pointer of Int4S}^Int4S;
  aInt4S = {Array of Int4S}Array Of Int4S;
  lInt4S = {List of Int4S}Specialize jList<Int4S>;

  Int8U  = {0..255}Type Byte;
  pInt8U = {Pointer of Int8U}^Int8U;
  aInt8U = {Array of Int8U}Array Of Int8U;
  lInt8U = {List of Int8U}Specialize jList<Int8U>;
  bInt8U = {Bit Index of Int8U}0..7;
  hInt8U = Type Helper for Int8U
  Public
    Const MaxValue = 255;
    Const MinValue = 0;
  Public
    Class Function Parse(AString: String): Int8U; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(AValue: Int8U): string; inline; static;
    Class Function TryParse(AString: string; out AValue: Int8U): Boolean; inline; static;
  Public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString: String;
    Function ToHexString(AMinDigits: Integer): string; inline;
    Function ToHexString: String; inline;
    Function ToSingle: Single; inline;
    Function ToString: String; inline;
    Function SetBit(Index: bInt8U): Int8U; inline;
    Function ClearBit(Index: bInt8U): Int8U; inline;
    Function ToggleBit(Index: bInt8U): Int8U; inline;
    Function TestBit(Index: bInt8U): Boolean; inline;
  end;

  Int8S  = {-128..127}Type ShortInt;
  pInt8S = {Pointer of Int8S}^Int8S;
  aInt8S = {Array of Int8S}Array Of Int8S;
  lInt8S = {List of Int8S}Specialize jList<Int8S>;
  bInt8S = {Bit Index of Int8S}0..7;
  hInt8S = Type Helper for Int8S
  public
    const
      MaxValue = 127;
      MinValue = -128;
  public
    Class Function Parse(AString: string): Int8S; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(AValue: Int8S): string; inline; static;
    Class Function TryParse(AString: string; out AValue: Int8S): Boolean; inline; static;
  public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString: string; inline;
    Function ToHexString(AMinDigits: Integer): string; inline;
    Function ToHexString: string; inline;
    Function ToSingle: Single; inline;
    Function ToString: string; inline;
    Function SetBit(Index: bInt8S): Int8S; inline;
    Function ClearBit(Index: bInt8S): Int8S; inline;
    Function ToggleBit(Index: bInt8S): Int8S; inline;
    Function TestBit(Index: bInt8S): Boolean;
  end;

  Int16U  = {0..65535}Type Word;
  pInt16U = {Pointer of Int16U}^Int16U;
  aInt16U = {Array of Int16U}Array Of Int16U;
  lInt16U = {List of Int16U}Specialize jList<Int16U>;
  bInt16U = {Bit Index of Int16U}0..15;
  hInt16U = Type Helper for Int16U
  public
    const
      MaxValue = 65535;
      MinValue = 0;
  Public
    Class Function Parse(AString: string): Int16U; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(AValue: Int16U): string; inline; static;
    Class Function TryParse(AString: string; out AValue: Int16U): Boolean; inline; static;
  Public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(AMinDigits: Integer): string; inline;
    Function ToHexString: string; inline;
    Function ToSingle: Single; inline;
    Function ToString: string; inline;
    Function SetBit(Index: bInt16U) : Int16U; inline;
    Function ClearBit(Index: bInt16U) : Int16U; inline;
    Function ToggleBit(Index: bInt16U) : Int16U; inline;
    Function TestBit(Index: bInt16U):Boolean; inline;
  end;

  Int16S  = {-32768..32767}Type SmallInt;
  pInt16S = {Pointer of Int16S}^Int16S;
  aInt16S = {Array of Int16S}Array Of Int16S;
  lInt16S = {List of Int16S}Specialize jList<Int16S>;
  bInt16S = {Bit Index of Int16S}0..15;
  hInt16S = Type Helper for Int16S
  public
    const
      MaxValue = 32767;
      MinValue = -32768;
  public
    Class Function Parse(AString: string): Int16S; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(AValue: Int16S): string; inline; static;
    Class Function TryParse(AString: string; out AValue: Int16S): Boolean; inline; static;
  public
    Function ToString: string; inline;
    Function ToBoolean: Boolean; inline;
    Function ToBinString:string; inline;
    Function ToHexString: string; inline;
    Function ToHexString(AMinDigits: Integer): string; inline;
    Function ToSingle: Single; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function SetBit(Index: bInt16S) : Int16S; inline;
    Function ClearBit(Index: bInt16S) : Int16S; inline;
    Function ToggleBit(Index: bInt16S) : Int16S; inline;
    Function TestBit(Index: bInt16S):Boolean;
  end;

  Int32U  = {0..4294967295}Type LongWord;
  pInt32U = {Pointer of Int32U}^Int32U;
  aInt32U = {Array of Int32U}Array Of Int32U;
  lInt32U = {List of Int32U}Specialize jList<Int32U>;
  bInt32U = {Bit Index of Int32U}0..31;
  hInt32U = Type Helper for Int32U
  public
    const
      MaxValue = 4294967295;
      MinValue = 0;
  Public
    Class Function Parse(AString: string): Int32U; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(AValue: Int32U): string; inline; static;
    Class Function TryParse(AString: string; out AValue: Int32U): Boolean; inline; static;
  Public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(AMinDigits: Integer): string; inline;
    Function ToHexString: string; inline;
    Function ToSingle: Single; inline;
    Function ToString: string; inline;
    Function SetBit(Index: bInt32U): Int32U; inline;
    Function ClearBit(Index: bInt32U): Int32U; inline;
    Function ToggleBit(Index: bInt32U): Int32U; inline;
    Function TestBit(Index: bInt32U):Boolean; inline;
  end;

  Int32S  = {-2147483648..2147483647}Type LongInt;
  pInt32S = {Pointer of Int32S}^Int32S;
  aInt32S = {Array of Int32S}Array Of Int32S;
  lInt32S = {List of Int32S}Specialize jList<Int32S>;
  bInt32S = {Bit Index of Int32S}0..31;
  hInt32S = Type Helper for Int32S
  public
    const
      MaxValue = 2147483647;
      MinValue = -2147483648;
  Public
    Class Function Size: Integer; inline; static;
    Class Function ToString(AValue: Int32S): string; inline; static;
    Class Function Parse(AString: string): Int32S; inline; static;
    Class Function TryParse(AString: string; out AValue: Int32S): Boolean; inline; static;
  Public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(AMinDigits: Integer): string; inline;
    Function ToHexString: string; inline;
    Function ToSingle: Single; inline;
    Function ToString: string; inline;
    Function SetBit(Index: bInt32S): Int32S; inline;
    Function ClearBit(Index: bInt32S): Int32S; inline;
    Function ToggleBit(Index: bInt32S): Int32S; inline;
    Function TestBit(Index: bInt32S): Boolean; inline;
  end;

  Int64U  = {0..18446744073709551615}Type QWord;
  pInt64U = {Pointer of Int64U}^Int64U;
  aInt64U = {Array of Int64U}Array Of Int64U;
  lInt64U = {List of Int64U}Specialize jList<Int64U>;
  bInt64U = {Bit Index of Int64U}0..63;
  hInt64U = Type Helper for Int64U
  public
    const
      MaxValue = 18446744073709551615;
      MinValue = 0;
  Public
    Class Function Parse(AString: string): Int64U; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(AValue: Int64U): string; inline; static;
    Class Function TryParse(AString: string; out AValue: Int64U): Boolean; inline; static;
  Public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(AMinDigits: Integer): string; inline;
    Function ToHexString: string; inline;
    Function ToSingle: Single; inline;
    Function ToString: string; inline;
    Function SetBit(Index: bInt64U): Int64U; inline;
    Function ClearBit(Index: bInt64U): Int64U; inline;
    Function ToggleBit(Index: bInt64U): Int64U; inline;
    Function TestBit(Index: bInt64U): Boolean; inline;
  end;

  Int64S  = {-9223372036854775808..9223372036854775807}Type Int64;
  pInt64S = {Pointer of tInt64S}^Int64S;
  aInt64S = {Array of Int64S}Array Of Int64S;
  lInt64S = {List of Int64S}Specialize jList<Int64S>;
  bInt64S = {Bit Index of Int64S}0..63;
  hInt64S = Type Helper for Int64S
  public
    const
      MaxValue = 9223372036854775807;
      MinValue = -9223372036854775808;
  Public
    Class Function Parse(AString: string): Int64S; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(AValue: Int64S): string; inline; static;
    Class Function TryParse(AString: string; out AValue: Int64S): Boolean; inline; static;
  Public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(AMinDigits: Integer): string; inline;
    Function ToHexString: string; inline;
    Function ToSingle: Single; inline;
    Function ToString: string; inline;
    Function SetBit(Index: bInt64S): Int64S; inline;
    Function ClearBit(Index: bInt64S): Int64S; inline;
    Function ToggleBit(Index: bInt64S): Int64S; inline;
    Function TestBit(Index: bInt64S): Boolean; inline;
  end;

  Int128U = {0..340282366920938463463374607431768211455}
    Packed Record
      Case Integer of
      {$ifdef FPC_LITTLE_ENDIAN}
        0: (Lo,Hi: Int64U);
      {$else}
        0: (Hi,Lo: Int64U);
      {$endif}
        1: (Int32Us: Array[0..3] of Int32U);
        2: (Int16Us: Array[0..7] of Int16U);
        3: (Int8Us: Array[0..15] of Int8U);
    End;
  pInt128U = {Pointer of Int128U}^Int128U;
  aInt128U = {Array of Int128U}Array Of Int128U;
  //lInt128U = {List of Int128U}Specialize jList<Int128U>;

  Int128S = {-170141183460469231731687303715884105727..170141183460469231731687303715884105727}
    Packed Record
      Case Integer of
      {$ifdef FPC_LITTLE_ENDIAN}
        0: (Lo,Hi: Int64S);
      {$else}
        0: (Hi,Lo: Int64S);
      {$endif}
        1: (Int32Ss: Array[0..3] of Int32S);
        2: (Int16Ss: Array[0..7] of Int16S);
        3: (Int8Ss: Array[0..15] of Int8S);
    End;
  pInt128S = {Pointer of Int128S}^Int128S;
  aInt128S = {Array of Int128S}Array Of Int128S;
  //lInt128S = {List of Int128S}Specialize jList<Int128S>;

  Int256U = {0..115792089237316195423570985008687907853269984665640564039457584007913129639935}
    Packed Record
      Case Integer of
      {$ifdef FPC_LITTLE_ENDIAN}
        0: (Lo,Hi: Int128U);
      {$else}
        0: (Hi,Lo: Int128U);
      {$endif}
        1: (Int64Us: Array[0..3] of Int64U);
        2: (Int32Us: Array[0..7] of Int32U);
        3: (Int16Us: Array[0..15] of Int16U);
        4: (Int8Us: Array[0..31] of Int8U);
    End;
  pInt256U = {Pointer of Int256U}^Int256U;
  aInt256U = {Array of Int256U}Array Of Int256U;
  //lInt256U = {List of Int256U}Specialize jList<Int256U>;

  Int256S = {-57896044618658097711785492504343953926634992332820282019728792003956564819968..57896044618658097711785492504343953926634992332820282019728792003956564819967}
    Packed Record
      Case Integer of
      {$ifdef FPC_LITTLE_ENDIAN}
        0: (Lo,Hi: Int128S);
      {$else}
        0: (Hi,Lo: Int128S);
      {$endif}
        1: (Int64Ss: Array[0..3] of Int64S);
        2: (Int32Ss: Array[0..7] of Int32S);
        3: (Int16Ss: Array[0..15] of Int16S);
        4: (Int8Ss: Array[0..31] of Int8S);
    End;
  pInt256S = {Pointer of Int256S}^Int256S;
  aInt256S = {Array of Int256S}Array Of Int256S;
  //lInt256S = {List of Int256S}Specialize jList<Int256S>;
{%endregion}

{%Region Strings}
Type
  StrS  = {Static String, Max 255 Char} ShortString;
  pStrS = {Pointer of StrS}^StrS;
  aStrS = {Array of StrS}Array Of StrS;
  lStrS = {List of StrS}Specialize jList<StrS>;

  StrA  = {Ansi String} AnsiString;
  pStrA = {Pointer of StrA}^StrA;
  aStrA = {Array of StrA}Array Of StrA;
  lStrA = {List of StrA}Specialize jList<StrA>;

  StrU  = {Unicode String} UnicodeString;
  pStrU = {Pointer of StrU}^StrU;
  aStrU = {Array of StrU}Array Of StrU;
  lStrU = {List of StrU}Specialize jList<StrU>;

  Str8  = {Utf8 String} type AnsiString(CP_UTF8);
  pStr8 = {Pointer of Str8}^Str8;
  aStr8 = {Array of Str8}Array Of Str8;
  lStr8 = {List of Str8}Specialize jList<Str8>;

  Str16  = {Utf16 String} type AnsiString(CP_UTF16);
  pStr16 = {Pointer of Str16}^Str16;
  aStr16 = {Array of Str16}Array Of Str16;
  lStr16 = {List of Str16}Specialize jList<Str16>;

  StrR  = {Raw String} type AnsiString(CP_NONE);
  pStrR = {Pointer of StrR}^StrR;
  aStrR = {Array of StrR}Array Of StrR;
  lStrR = {List of StrR}Specialize jList<StrR>;

  Operator in(in1: aStrA; in2: StrA): Boolean;
  Function FindTextStart(AText: String): Int32U;
  Function FindTextFinish(AText: String): Int32U;
  Function ClearText(AText: String): String;

  Function Match(in1,in2: StrA; IgnoreCase: Boolean = False): Boolean;

  Function UpCase(S: StrA): StrA; Inline;
  Function DownCase(S: StrA): StrA; Inline;
{%EndRegion}

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

Operator not (in1: jARGB): jARGB;

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
Type
  haStrA = Type Helper for aStrA
    Public
      Procedure Add(Line: StrA);
      Procedure Add(Lines: TStringList);
  End;
{%EndRegion}

Procedure Delay(dt: Int64U); Inline;
Procedure Delay(dt: Int64U; From: TControl); Inline;

Procedure FreeIfAssigned(Var Obj); //Deprecated 'Use FIA instead';
Procedure FIA(Var Obj);
Procedure Swap(Var Obj1, Obj2);

Type
  mNotify = Procedure(Sender: TObject) of Object;
  mNotifyB = procedure(Sender: TObject);

  mProcedureB = Procedure;
  mProcedureO = Procedure of object;

Implementation
End.

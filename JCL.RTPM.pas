{  $Id$  }
{
Jix RTTM
|
|- Ver: 0.3
|- Status: Alpha
|- Meaning: Jixoid Run Time Property Manager
}
Unit JCL.RTPM;

{$mode ObjFPC}{$H+}

Interface

Uses
  // FCL(FreePascal Component Library)
  Classes, SysUtils, TypInfo,

  // LCL(Lazarus Component Library)
  Dialogs,

  // JCL(Jixoid Component Library, JixUI)
  JCL.Generics,
  JCL.Utils,
  JCL.Dialogs;

Type
  eTypeKind = (
    // Integers
    tkInt8U, tkInt8S, tkInt16U, tkInt16S, tkInt32U, tkInt32S, tkInt64U, tkInt64S,

    // Boolean
    tkBoolean,

    // Strings's
    tkStrS, tkStrA,

    // Enum
    tkEnum, tkEnumSet,

    // Class and such's
    tkRecord, tkClass
  );


  eAccessLimit = (alRead, alWrite);
  sAccessLimit = Set Of eAccessLimit;

  mPointerF = Function: Pointer of object;
  mPointerP = Procedure(Value: Pointer) of object;

  mStrAF = Function: StrA of object;
  mStrAP = Procedure(Value: StrA) of object;

  mInt8UF = Function: Int8U of object;
  mInt8UP = Procedure(Value: Int8U) of object;

  mInt8SF = Function: Int8S of object;
  mInt8SP = Procedure(Value: Int8S) of object;

  mInt16UF = Function: Int16U of object;
  mInt16UP = Procedure(Value: Int16U) of object;

  mInt16SF = Function: Int16S of object;
  mInt16SP = Procedure(Value: Int16S) of object;

  mInt32UF = Function: Int32U of object;
  mInt32UP = Procedure(Value: Int32U) of object;

  mInt32SF = Function: Int32S of object;
  mInt32SP = Procedure(Value: Int32S) of object;

  mInt64UF = Function: Int64U of object;
  mInt64UP = Procedure(Value: Int64U) of object;

  mInt64SF = Function: Int64S of object;
  mInt64SP = Procedure(Value: Int64S) of object;

  mBooleanF = Function: Boolean of object;
  mBooleanP = Procedure(Value: Boolean) of object;

  eSetGetType = (sgtMethod, sgtSpecializedMethod, sgtVariable, sgtNone);

  RTTMError = Class(Exception)

  End;

  // Fudge
  jRTTIProperty = Class;
  jRTTI = Class;

  pRTTI = ^jRTTI;
  lRTTI = Specialize jList<jRTTI>;

  pRTTIProperty = ^jRTTIProperty;
  lRTTIProperty = Specialize jList<jRTTIProperty>;

  jRTTIProperty = Class
    Private
      Var FName: StrS;
      Var FGroup: StrS;
      Var FTypeKind: eTypeKind;
      Var FAccessLimit: sAccessLimit;
      Var FEnumList: aStrA;
      Var FEnumType: PTypeInfo;

      // Get
      Var FGetType: eSetGetType;
      Var FGetterString: pStrA;
      Var FGetterInteger: pInt64S;
      Var FGetterBoolean: pBoolean;
      Var FGetterMethod: mPointerF;
      Var FGetterSpecializedStrA: mStrAF;
      Var FGetterSpecializedInt8S: mInt8SF;
      Var FGetterSpecializedInt8U: mInt8UF;
      Var FGetterSpecializedInt16S: mInt16SF;
      Var FGetterSpecializedInt16U: mInt16UF;
      Var FGetterSpecializedInt32S: mInt32SF;
      Var FGetterSpecializedInt32U: mInt32UF;
      Var FGetterSpecializedInt64S: mInt64SF;
      Var FGetterSpecializedInt64U: mInt64UF;
      Var FGetterSpecializedBoolean: mBooleanF;

      // Set
      Var FSetType: eSetGetType;
      Var FSetterString: pStrA;
      Var FSetterInteger: pInt64S;
      Var FSetterBoolean: pBoolean;
      Var FSetterMethod: mPointerP;
      Var FSetterSpecializedStrA: mStrAP;
      Var FSetterSpecializedInt8S: mInt8SP;
      Var FSetterSpecializedInt8U: mInt8UP;
      Var FSetterSpecializedInt16S: mInt16SP;
      Var FSetterSpecializedInt16U: mInt16UP;
      Var FSetterSpecializedInt32S: mInt32SP;
      Var FSetterSpecializedInt32U: mInt32UP;
      Var FSetterSpecializedInt64S: mInt64SP;
      Var FSetterSpecializedInt64U: mInt64UP;
      Var FSetterSpecializedBoolean: mBooleanP;

    Public
      Property Name: StrS                Read FName;
      Property Group: StrS               Read FGroup;
      Property TypeKind: eTypeKind       Read FTypeKind;
      Property AccessLimit: sAccessLimit Read FAccessLimit;
      Property EnumList: aStrA           Read FEnumList;

      Property GetType: eSetGetType      Read FGetType;
      Property GetterMethod: mPointerF   Read FGetterMethod;
      Property GetterString: pStrA       Read FGetterString;
      Property GetterInteger: pInt64S    Read FGetterInteger;
      Property Getterboolean: pBoolean   Read FGetterBoolean;
      Property GetterSpecializedStrA: mStrAF       Read FGetterSpecializedStrA;
      Property GetterSpecializedInt8S: mInt8SF     Read FGetterSpecializedInt8S;
      Property GetterSpecializedInt8U: mInt8UF     Read FGetterSpecializedInt8U;
      Property GetterSpecializedInt16S: mInt16SF   Read FGetterSpecializedInt16S;
      Property GetterSpecializedInt16U: mInt16UF   Read FGetterSpecializedInt16U;
      Property GetterSpecializedInt32S: mInt32SF   Read FGetterSpecializedInt32S;
      Property GetterSpecializedInt32U: mInt32UF   Read FGetterSpecializedInt32U;
      Property GetterSpecializedInt64S: mInt64SF   Read FGetterSpecializedInt64S;
      Property GetterSpecializedInt64U: mInt64UF   Read FGetterSpecializedInt64U;
      Property GetterSpecializedBoolean: mBooleanF Read FGetterSpecializedBoolean;

      Property SetType: eSetGetType      Read FSetType;
      Property SetterMethod: mPointerP   Read FSetterMethod;
      Property SetterString: pStrA       Read FSetterString;
      Property SetterInteger: pInt64S    Read FSetterInteger;
      Property Setterboolean: pBoolean   Read FSetterBoolean;
      Property SetterSpecializedStrA: mStrAP       Read FSetterSpecializedStrA;
      Property SetterSpecializedInt8S: mInt8SP     Read FSetterSpecializedInt8S;
      Property SetterSpecializedInt8U: mInt8UP     Read FSetterSpecializedInt8U;
      Property SetterSpecializedInt16S: mInt16SP   Read FSetterSpecializedInt16S;
      Property SetterSpecializedInt16U: mInt16UP   Read FSetterSpecializedInt16U;
      Property SetterSpecializedInt32S: mInt32SP   Read FSetterSpecializedInt32S;
      Property SetterSpecializedInt32U: mInt32UP   Read FSetterSpecializedInt32U;
      Property SetterSpecializedInt64S: mInt64SP   Read FSetterSpecializedInt64S;
      Property SetterSpecializedInt64U: mInt64UP   Read FSetterSpecializedInt64U;
      Property SetterSpecializedBoolean: mBooleanP Read FSetterSpecializedBoolean;
  End;

  jRTTI = Class(jRTTIProperty)
    Private
      Var FClassType: TClass;
      Var FClassName: StrS;
      Var FPointer: Pointer;

      Var FSubProperties: lRTTIProperty;

    Public
      Property AClassName: StrS             Read FClassName;
      Property AClassType: TClass           Read FClassType;
      Property APointer: Pointer         Read FPointer;

      Property SubProperties: lRTTIProperty Read FSubProperties;
  End;

  jRTTManager = Class
    Private
      Var RTTICollection: lRTTI;

    Public
      Constructor Create;
      Destructor Destroy; Override;

      Procedure RegisterClass(AClass: TObject);
      Function GetClass(AClass: TObject): jRTTI;

      Procedure RegisterProperty(AClass: TObject; Name: StrS; TypeKind: eTypeKind; Group: StrS = 'None');
      Procedure UnRegisterProperty(AClass: TObject; Name: StrS);

      Procedure RegisterPropertyEnum(AClass: TObject; Name: StrS; EnumType: PTypeInfo);

      Procedure RegisterPropertyGetter(AClass: TObject; Name: StrS; Getter: mPointerF);
      Procedure RegisterPropertyGetter(AClass: TObject; Name: StrS; Getter: mStrAF);
      Procedure RegisterPropertyGetter(AClass: TObject; Name: StrS; Getter: mInt8UF);
      Procedure RegisterPropertyGetter(AClass: TObject; Name: StrS; Getter: mInt8SF);
      Procedure RegisterPropertyGetter(AClass: TObject; Name: StrS; Getter: mInt16UF);
      Procedure RegisterPropertyGetter(AClass: TObject; Name: StrS; Getter: mInt16SF);
      Procedure RegisterPropertyGetter(AClass: TObject; Name: StrS; Getter: mInt32UF);
      Procedure RegisterPropertyGetter(AClass: TObject; Name: StrS; Getter: mInt32SF);
      Procedure RegisterPropertyGetter(AClass: TObject; Name: StrS; Getter: mInt64UF);
      Procedure RegisterPropertyGetter(AClass: TObject; Name: StrS; Getter: mInt64SF);
      Procedure RegisterPropertyGetter(AClass: TObject; Name: StrS; Getter: mBooleanF);
      Procedure RegisterPropertyGetter(AClass: TObject; Name: StrS; Getter: Pointer);
      Procedure UnRegisterPropertyGetter(AClass: TObject; Name: StrS);

      Procedure RegisterPropertySetter(AClass: TObject; Name: StrS; Setter: mPointerP);
      Procedure RegisterPropertySetter(AClass: TObject; Name: StrS; Setter: mStrAP);
      Procedure RegisterPropertySetter(AClass: TObject; Name: StrS; Setter: mInt8UP);
      Procedure RegisterPropertySetter(AClass: TObject; Name: StrS; Setter: mInt8SP);
      Procedure RegisterPropertySetter(AClass: TObject; Name: StrS; Setter: mInt16UP);
      Procedure RegisterPropertySetter(AClass: TObject; Name: StrS; Setter: mInt16SP);
      Procedure RegisterPropertySetter(AClass: TObject; Name: StrS; Setter: mInt32UP);
      Procedure RegisterPropertySetter(AClass: TObject; Name: StrS; Setter: mInt32SP);
      Procedure RegisterPropertySetter(AClass: TObject; Name: StrS; Setter: mInt64UP);
      Procedure RegisterPropertySetter(AClass: TObject; Name: StrS; Setter: mInt64SP);
      Procedure RegisterPropertySetter(AClass: TObject; Name: StrS; Setter: mBooleanP);
      Procedure RegisterPropertySetter(AClass: TObject; Name: StrS; Setter: Pointer);
      Procedure UnRegisterPropertySetter(AClass: TObject; Name: StrS);

      Function GetPropertyValueString(AProperty: jRTTIProperty): StrA;
      Function GetPropertyValueIntegerU(AProperty: jRTTIProperty): Int64U;
      Function GetPropertyValueIntegerS(AProperty: jRTTIProperty): Int64S;
      Function GetPropertyValueBoolean(AProperty: jRTTIProperty): Boolean;

      Procedure SetPropertyValueString(AProperty: jRTTIProperty; Value: StrA);
      Procedure SetPropertyValueIntegerU(AProperty: jRTTIProperty; Value: Int64U);
      Procedure SetPropertyValueIntegerS(AProperty: jRTTIProperty; Value: Int64S);
      Procedure SetPropertyValueBoolean(AProperty: jRTTIProperty; Value: Boolean);
  End;

Var
  JRTTM: jRTTManager;

Implementation
End.

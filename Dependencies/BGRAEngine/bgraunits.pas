// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAUnits;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, BGRABitmapTypes;

type
  TSVGNumber = single;//double
  ArrayOfTSVGNumber = array of TSVGNumber;

  TCSSUnit = (cuCustom, cuPixel,
              cuCentimeter, cuMillimeter,
              cuInch, cuPica, cuPoint,
              cuFontEmHeight, cuFontXHeight, cuPercent);
  TFloatWithCSSUnit = record
    value: single;
    CSSUnit: TCSSUnit;
  end;

  ArrayOfTFloatWithCSSUnit = array of TFloatWithCSSUnit;


function FloatWithCSSUnit(AValue: single; AUnit: TCSSUnit): TFloatWithCSSUnit;

const
  CSSUnitShortName: array[TCSSUnit] of string =
        ('','px',
         'cm','mm',
         'in','pc','pt',
         'em','ex','%');

type
  { TCSSUnitConverter }

  TCSSUnitConverter = class
  protected
    FViewBoxHeight: TFloatWithCSSUnit;
    FViewBoxWidth: TFloatWithCSSUnit;
    FViewBoxHeightInUnit: array[TCSSUnit] of single;
    FViewBoxWidthInUnit: array[TCSSUnit] of single;
    FCurrentFontEmHeight: TFloatWithCSSUnit;
    function GetRootFontEmHeight: TFloatWithCSSUnit;
    function GetDefaultUnitHeight: TFloatWithCSSUnit; virtual;
    function GetDefaultUnitWidth: TFloatWithCSSUnit; virtual;
    function GetDpiX: single; virtual;
    function GetDpiY: single; virtual;
    function GetFontEmHeight: TFloatWithCSSUnit; virtual;
    function GetFontXHeight: TFloatWithCSSUnit; virtual;
    procedure SetViewBoxHeight(AValue: TFloatWithCSSUnit);
    procedure SetViewBoxWidth(AValue: TFloatWithCSSUnit);
    property FontEmHeight: TFloatWithCSSUnit read GetFontEmHeight;
    property FontXHeight: TFloatWithCSSUnit read GetFontXHeight;
    property DefaultUnitWidth: TFloatWithCSSUnit read GetDefaultUnitWidth;
    property DefaultUnitHeight: TFloatWithCSSUnit read GetDefaultUnitHeight;
  public
    constructor Create;
    function ConvertOrtho(xy: single; sourceUnit, destUnit: TCSSUnit): single; overload;
    function ConvertOrtho(AValue: TFloatWithCSSUnit; destUnit: TCSSUnit): TFloatWithCSSUnit; overload;
    function ConvertWidth(x: single; sourceUnit, destUnit: TCSSUnit): single; overload;
    function ConvertHeight(y: single; sourceUnit, destUnit: TCSSUnit): single; overload;
    function ConvertWidth(AValue: TFloatWithCSSUnit; destUnit: TCSSUnit): TFloatWithCSSUnit; overload;
    function ConvertHeight(AValue: TFloatWithCSSUnit; destUnit: TCSSUnit): TFloatWithCSSUnit; overload;
    function ConvertWidth(AValue: ArrayOfTFloatWithCSSUnit; destUnit: TCSSUnit): ArrayOfTFloatWithCSSUnit; overload;
    function ConvertHeight(AValue: ArrayOfTFloatWithCSSUnit; destUnit: TCSSUnit): ArrayOfTFloatWithCSSUnit; overload;
    function ConvertCoord(pt: TPointF; sourceUnit, destUnit: TCSSUnit): TPointF; overload;
    function GetConversionMatrix(AFromUnit, AToUnit: TCSSUnit): TAffineMatrix; overload;
    function Convert(xy: single; sourceUnit, destUnit: TCSSUnit; dpi: single; containerSize: single = 0): single;
    function ConvertOrtho(AValue: TFloatWithCSSUnit; destUnit: TCSSUnit; containerWidth: single; containerHeight: single): TFloatWithCSSUnit; overload;
    function ConvertWidth(x: single; sourceUnit, destUnit: TCSSUnit; containerWidth: single): single; overload;
    function ConvertHeight(y: single; sourceUnit, destUnit: TCSSUnit; containerHeight: single): single; overload;
    function ConvertWidth(AValue: TFloatWithCSSUnit; destUnit: TCSSUnit; containerWidth: single): TFloatWithCSSUnit; overload;
    function ConvertHeight(AValue: TFloatWithCSSUnit; destUnit: TCSSUnit; containerHeight: single): TFloatWithCSSUnit; overload;
    function ConvertWidth(AValue: ArrayOfTFloatWithCSSUnit; destUnit: TCSSUnit; containerWidth: single): ArrayOfTFloatWithCSSUnit; overload;
    function ConvertHeight(AValue: ArrayOfTFloatWithCSSUnit; destUnit: TCSSUnit; containerHeight: single): ArrayOfTFloatWithCSSUnit; overload;
    function ConvertCoord(pt: TPointF; sourceUnit, destUnit: TCSSUnit; containerWidth: single; containerHeight: single): TPointF; overload;
    function GetConversionMatrix(AFromUnit, AToUnit: TCSSUnit; containerWidth: single; containerHeight: single): TAffineMatrix; overload;
    class function parseValue(AValue: string; ADefault: TFloatWithCSSUnit): TFloatWithCSSUnit; overload; static;
    class function parseValue(AValue: string; ADefault: single): single; overload; static;
    class function parseArrayOfNumbers(AValue: string): ArrayOfTSVGNumber; overload; static;
    class function parseArrayOfValuesWithUnit(AValue: string): ArrayOfTFloatWithCSSUnit; overload; static;
    class function formatValue(AValue: TFloatWithCSSUnit; APrecision: integer = 7): string; overload; static;
    class function formatValue(AValue: single; APrecision: integer = 7): string; overload; static;
    class function formatValue(AValue: ArrayOfTSVGNumber; APrecision: integer = 7): string; overload; static;
    class function formatValue(AValue: ArrayOfTFloatWithCSSUnit; APrecision: integer = 7): string; overload; static;
    property ViewBoxWidth: TFloatWithCSSUnit read FViewBoxWidth write SetViewBoxWidth;
    property ViewBoxHeight: TFloatWithCSSUnit read FViewBoxHeight write SetViewBoxHeight;
    property DpiX: single read GetDpiX;
    property DpiY: single read GetDpiY;
    property CurrentFontEmHeight: TFloatWithCSSUnit read FCurrentFontEmHeight write FCurrentFontEmHeight;
    property RootFontEmHeight: TFloatWithCSSUnit read GetRootFontEmHeight;
  end;

Implementation
End.

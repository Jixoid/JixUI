// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAArrow;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, BGRABitmapTypes, BGRAGraphics;

type
  { TBGRAArrow }

  TBGRAArrow = class(TBGRACustomArrow)
  private
    FLineCap: TPenEndCap;
    FWidth : single;
    FStart : ArrayOfTPointF;
    FStartComputed: boolean;
    FStartStyle: TBGRAArrowStyle;
    FStartSizeFactor: TPointF;
    FStartTipStyle: TPenJoinStyle;
    FStartOffsetX: single;
    FStartRepeatCount: integer;
    FStartRelativePenWidth: single;
    FStartTriangleBackOffset: single;
    FEnd : ArrayOfTPointF;
    FEndComputed: boolean;
    FEndStyle: TBGRAArrowStyle;
    FEndSizeFactor: TPointF;
    FEndTipStyle: TPenJoinStyle;
    FEndOffsetX: single;
    FEndRepeatCount: integer;
    FEndRelativePenWidth: single;
    FEndTriangleBackOffset: single;
    function ComputeAnyAt(const AData: ArrayOfTPointF; const APosition, ADirection: TPointF): ArrayOfTPointF;
    function ComputeData(AStyle: TBGRAArrowStyle; const ASizeFactor: TPointF;
        ATipStyle: TPenJoinStyle; ALineCap: TPenEndCap; const AWidth: single; AOffsetX: single;
        ARepeatCount: integer; ARelativePenWidth: single; ATriangleBackOffset: single): ArrayOfTPointF;
    procedure SetWidth(AValue: single);
  protected
    function GetEndRepeatCount: integer; override;
    function GetEndSizeFactor: TPointF; override;
    function GetIsEndDefined: boolean; override;
    function GetIsStartDefined: boolean; override;
    function GetEndOffsetX: single; override;
    function GetStartOffsetX: single; override;
    function GetStartRepeatCount: integer; override;
    function GetStartSizeFactor: TPointF; override;
    procedure SetEndOffsetX(AValue: single); override;
    procedure SetEndRepeatCount(AValue: integer); override;
    procedure SetEndSizeFactor(AValue: TPointF); override;
    procedure SetStartOffsetX(AValue: single); override;
    procedure SetStartRepeatCount(AValue: integer); override;
    procedure SetStartSizeFactor(AValue: TPointF); override;
    function GetLineCap: TPenEndCap; override;
    procedure SetLineCap(AValue: TPenEndCap); override;
    procedure SetStart(AStyle: TBGRAArrowStyle; ATipStyle: TPenJoinStyle = pjsMiter; ARelativePenWidth: single = 1; ATriangleBackOffset: single = 0);
    procedure SetEnd(AStyle: TBGRAArrowStyle; ATipStyle: TPenJoinStyle = pjsMiter; ARelativePenWidth: single = 1; ATriangleBackOffset: single = 0);
  public
    constructor Create;
    procedure StartAsNone; override;
    procedure StartAsClassic(AFlipped: boolean = false; ACut: boolean = false; ARelativePenWidth: single = 1); override;
    procedure StartAsTriangle(ABackOffset: single = 0; ARounded: boolean = false; AHollow: boolean = false; AHollowPenWidth: single = 0.5); override;
    procedure StartAsTail; override;
    procedure EndAsNone; override;
    procedure EndAsClassic(AFlipped: boolean = false; ACut: boolean = false; ARelativePenWidth: single = 1); override;
    procedure EndAsTriangle(ABackOffset: single = 0; ARounded: boolean = false; AHollow: boolean = false; AHollowPenWidth: single = 0.5); override;
    procedure EndAsTail; override;
    function ComputeStartAt(const APosition: TPointF; const ADirection: TPointF; const AWidth: single; const ACurrentPos: single): ArrayOfTPointF; override;
    function ComputeEndAt(const APosition: TPointF; const ADirection: TPointF; const AWidth: single; const ACurrentPos: single): ArrayOfTPointF; override;

  end;

Implementation
End.

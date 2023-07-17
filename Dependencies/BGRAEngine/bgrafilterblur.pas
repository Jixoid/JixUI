// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAFilterBlur;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, BGRABitmapTypes, BGRAFilterType;

type
  { TCustomBlurTask }

  TCustomBlurTask = class(TFilterTask)
  private
    FBounds: TRect;
    FMask: TCustomUniversalBitmap;
    FMaskOwned: boolean;
  public
    constructor Create(bmp: TBGRACustomBitmap; ABounds: TRect; AMask: TCustomUniversalBitmap; AMaskIsThreadSafe: boolean = false);
    destructor Destroy; override;
  protected
    procedure DoExecute; override;
  end;

  { TRadialBlurTask }

  TRadialBlurTask = class(TFilterTask)
  private
    FBounds: TRect;
    FRadiusX,FRadiusY: single;
    FBlurType: TRadialBlurType;
  public
    constructor Create(bmp: TBGRACustomBitmap; ABounds: TRect; radius: single;
                       blurType: TRadialBlurType); overload;
    constructor Create(bmp: TBGRACustomBitmap; ABounds: TRect; radiusX,radiusY: single;
                       blurType: TRadialBlurType); overload;
  protected
    procedure DoExecute; override;
  end;

  { TMotionBlurTask }

  TMotionBlurTask = class(TFilterTask)
  private
    FBounds: TRect;
    FDistance,FAngle: single;
    FOriented: boolean;
  public
    constructor Create(ABmp: TBGRACustomBitmap; ABounds: TRect; ADistance, AAngle: single; AOriented: boolean);
  protected
    procedure DoExecute; override;
  end;

procedure FilterBlurCustom(bmp: TCustomUniversalBitmap; ABounds: TRect;
   blurMask: TCustomUniversalBitmap; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc);
procedure FilterBlurMotion(bmp: TCustomUniversalBitmap; ABounds: TRect; distance: single;
  angle: single; oriented: boolean; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc);
procedure FilterBlurRadial(bmp: TCustomUniversalBitmap; ABounds: TRect; radiusX,radiusY: single;
  blurType: TRadialBlurType; ADestination: TCustomUniversalBitmap; ACheckShouldStop: TCheckShouldStopFunc);

Implementation
End.

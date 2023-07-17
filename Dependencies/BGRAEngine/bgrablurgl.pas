// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRABlurGL;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, BGRAOpenGL3D, BGRABitmapTypes, BGRACanvasGL, BGRAOpenGLType;

type

  { TBGLBlurShader }

  TBGLBlurShader = class(TBGLShader3D)
  private
    function GetDirection: TPointF;
    function GetImageIndex: integer;
    function GetRadius: Single;
    function GetTextureSize: TPoint;
    procedure SetDirection(AValue: TPointF);
    procedure SetImageIndex(AValue: integer);
    procedure SetRadius(AValue: Single);
    procedure SetTextureSize(AValue: TPoint);
  protected
    FTextureSize: TUniformVariablePoint;
    FImageIndex: TUniformVariableInteger;
    FDirection: TUniformVariablePointF;
    FRadius: TUniformVariableSingle;
    FBlurType: TRadialBlurType;
    procedure StartUse; override;
  public
    constructor Create(ACanvas: TBGLCustomCanvas; ABlurType: TRadialBlurType);
    function FilterBlurMotion(ATexture: IBGLTexture): IBGLTexture; overload;
    function FilterBlurMotion(ATexture: IBGLTexture; ADirection: TPointF): IBGLTexture; overload;
    function FilterBlurRadial(ATexture: IBGLTexture): IBGLTexture;
    property ImageIndex: integer read GetImageIndex write SetImageIndex;
    property TextureSize: TPoint read GetTextureSize write SetTextureSize;
    property Direction: TPointF read GetDirection write SetDirection;
    property Radius: Single read GetRadius write SetRadius;
    property BlurType: TRadialBlurType read FBlurType;
  end;

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ A Graphic Button Control that uses SVG images as the button states
  for Normal,Hover and DOWN states.

  originally written in 2018 by User Josh on Lazarus Forum.

  You can use the SVGDOWNXML property to enter the SVG XML code to create the
  image or You can enter the full svg image file and pathname into the properties
  FileNameDown; it will then read in the File Information and place it in the
  SVGDownXML Property.

  This Component uses the BGRABITMAP and BGRACONTROLS Framework to implement
  the Button's Functionality
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCSVGButton;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  {$IFDEF FPC}LResources, lazutils,{$ENDIF}
  {$IFNDEF FPC}Windows, Messages, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCSVGViewer;

type

  SVGButtonState = (MouseIn, MouseOut, Pressed);

  TBCSVGButton = class(TBCSVGViewer)
  private
    fsvgnormal:tstrings;
    fsvghover:tstrings;
    fsvgdown:tstrings;
    fdown:boolean;
    FState:SVGButtonState;
    FOwner: TComponent;
    FFileNameHover: String;
    FFileNameNormal: String;
    FFileNameDown: String;
    FPosition: Integer;
    FMax: Integer;
    FInfo1: String;
    FInfo2: String;
  //  property OnPositionChange;
    procedure setdown(AValue: boolean);
    procedure ReadSVGFileAndSetString(fn:String;itm:Integer);
    procedure GenerateCompletedSVGImage(AValue: string);
  protected
    FOnPositionChange: TNotifyEvent;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      MX, MY: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; MX, MY: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure setsvghoverxml(const AValue: tstrings);
    procedure setsvgnormalxml(const AValue: tstrings);
    procedure setsvgdownxml(const AValue: tstrings);
    procedure setFFileNameDown(const AValue: string);
    procedure setFFileNameHover(const AValue: string);
    procedure setFFileNameNormal(const AValue: string);
    procedure SetInfo1(const AValue:String);
    procedure SetInfo2(const AValue:String);
    procedure Setposition(const AValue:Integer);
    procedure SetMax(const AValue:Integer);
    procedure RedrawBitmapContent; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure paint; override;
  published
    property BorderSpacing;
    property Constraints;
    Property FileNameDown : String Read FFileNameDown Write setFFileNameDown;
    Property FileNameHover : String Read FFileNameHover Write setFFileNameHover;
    Property FileNameNormal : String Read FFileNameNormal Write setFFileNameNormal;
    property SVGNormalXML:tstrings read fsvgnormal write setsvgnormalxml;
    property SVGHoverXML:tstrings read fsvghover write setsvghoverxml;
    property SVGDownXML:tstrings read fsvgdown write setsvgdownxml;
    property Down:boolean read fdown write setdown default false;
    property Information1:string read FInfo1 write SetInfo1;
    property Information2:string read FInfo2 write SetInfo2;
    property Position:integer read fposition write SetPosition;
    property Maximum:integer read fmax write SetMax;
    property OnPositionChange: TNotifyEvent read FOnPositionChange write FOnPositionChange;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Styles form manager

  ------------------------------------------------------------------------------
  originally written in 2012 by Krzysztof Dibowski dibowski at interia.pl
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCStylesForm;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}FileUtil, ComponentEditors, PropEdits,{$ELSE}
  Windows, DesignIntf, DesignEditors, PropertyCategories,
  ToolIntf, ExptIntf, DesignWindows,
  {$ENDIF}
  Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ActnList, ComCtrls, Buttons,
  bcbasectrls;

type

  { TBCfrmStyle }

  TBCfrmStyle = class(TForm)
    ActionRefresh: TAction;
    ActionNewFromFile: TAction;
    ActionDelete: TAction;
    ActionNewFromCtrl: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    gboxPreview: TGroupBox;
    gboxStyles: TGroupBox;
    lvFiles: TListView;
    memoLogs: TMemo;
    OpenDialog1: TOpenDialog;
    pnlBottom: TPanel;
    Splitter1: TSplitter;
    sptrLog: TSplitter;
    ToolBar1: TToolBar;
    btnDelete: TToolButton;
    btnNewFromCtrl: TToolButton;
    ToolButton1: TToolButton;
    btnNewFromFile: TToolButton;
    btnRefresh: TToolButton;
    procedure ActionDeleteExecute({%H-}Sender: TObject);
    procedure ActionNewFromCtrlExecute({%H-}Sender: TObject);
    procedure ActionNewFromFileExecute({%H-}Sender: TObject);
    procedure ActionRefreshExecute({%H-}Sender: TObject);
    procedure FormCloseQuery({%H-}Sender: TObject; var CanClose: boolean);
    procedure lvFilesSelectItem({%H-}Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    { private declarations }
    FControl: TControl;
    FPreviewControl: TControl;
    FStyleExt: String;
    procedure AddLog(const AText: String; AClear: Boolean = True);
    procedure CreatePreviewControl;
    function GetFileName: String;
    function GetStylesDir: String;
  public
    { public declarations }
    constructor {%H-}Create(AControl: TControl; const AFileExt: String);

    property FileName: String read GetFileName;
  end;

  { TBCStyleComponentEditor }

  TBCStyleComponentEditor = class(TComponentEditor)
  protected
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetStyleExtension: String;
    procedure DoShowEditor;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function  GetVerb({%H-}Index: Integer): String; override;
    function  GetVerbCount: Integer; override;
  end;

  { TBCSylePropertyEditor }

  TBCSylePropertyEditor = class({$IFDEF FPC}TClassPropertyEditor{$ELSE}TPropertyEditor{$ENDIF})
  private
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetStyleExtension: String;
    procedure DoShowEditor;
  public
    procedure Edit; Override;
    function  GetAttributes: TPropertyAttributes; Override;
  end;

Implementation
End.

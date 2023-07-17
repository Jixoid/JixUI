unit bgrasvgimagelistform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, BCSVGViewer,
  BGRASVGImageList, ComponentEditors, Types, Math, LCLType, ComCtrls;

type

  { TfrmBGRASVGImageListEditor }

  TfrmBGRASVGImageListEditor = class(TForm)
    BCSVGViewerPreview: TBCSVGViewer;
    btnAdd: TButton;
    btnRemove: TButton;
    btnUp: TButton;
    btnDown: TButton;
    btnReplace: TButton;
    CheckBox_UseSVGAlignment: TCheckBox;
    ImageList1: TImageList;
    ListBox1: TListBox;
    OpenDialog1: TOpenDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton_AlignTop: TToolButton;
    ToolButton_AlignLeft: TToolButton;
    ToolButton_AlignCenter: TToolButton;
    ToolButton_AlignRight: TToolButton;
    ToolButton_AlignVCenter: TToolButton;
    ToolButton_AlignBottom: TToolButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure CheckBox_UseSVGAlignmentChange(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
    procedure ToolButton_AlignBottomClick(Sender: TObject);
    procedure ToolButton_AlignCenterClick(Sender: TObject);
    procedure ToolButton_AlignLeftClick(Sender: TObject);
    procedure ToolButton_AlignRightClick(Sender: TObject);
    procedure ToolButton_AlignTopClick(Sender: TObject);
    procedure ToolButton_AlignVCenterClick(Sender: TObject);
  private
    FComponent: TComponent;
    function GetImageList: TBGRASVGImageList;
    procedure UpdateListBox;
    procedure UpdateButtons;
    procedure UpdateToolButtonsAlign;
  public
    constructor {%H-}Create(AComponent: TComponent);
    property ImageList: TBGRASVGImageList read GetImageList;
  end;

  { TBGRASVGImageListEditor }

  TBGRASVGImageListEditor = class(TComponentEditor)
  protected
    procedure DoShowEditor;
  public
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb({%H-}Index: integer): string; override;
    function GetVerbCount: integer; override;
  end;

var
  frmBGRASVGImageListEditor: TfrmBGRASVGImageListEditor;

Implementation
End.

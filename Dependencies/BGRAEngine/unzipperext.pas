// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit UnzipperExt;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, zipper;

type

  { TUnzipperStreamUtf8 }

  TUnzipperStreamUtf8 = class(TUnZipper)
    private
      FCustomOutputStream: TStream;
      FCustomInputStream: TStream;
      procedure SetInputStream(AValue: TStream);
    protected
      Procedure CustomOpenInput(Sender: TObject; var AStream: TStream);
      procedure CustomCloseInput(Sender: TObject; var AStream: TStream);
      procedure CustomCreateOutput(Sender : TObject; var AStream : TStream; {%H-}AItem : TFullZipFileEntry);
      procedure CustomCloseOutput(Sender : TObject; var AStream : TStream; {%H-}AItem : TFullZipFileEntry);
    public
      function UnzipFileToStream(AFilename: string; AStream: TStream; ACaseSensitive: boolean= true): boolean;
      function UnzipFileToString(AFilename:string): string;
      constructor Create;
      property InputStream: TStream read FCustomInputStream write SetInputStream;
  end;

Implementation
End.

// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Useful tools for RTTI. Functions are used expecialy for save/load styles.
  Styles has construction similar to INI files:

  [Header]
  Author=Krzysztof Dibowski
  Description=My test style
  ControlClass=TBCButton

  [Properties]
  State.Border.Width=2
  .....

  But instead of IniFiles unit, we have own functions for read and write styles.

  ------------------------------------------------------------------------------
  originally written in 2012 by Krzysztof Dibowski dibowski at interia.pl
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCRTTI;

{$I bgracontrols.map.inc}

interface

uses
  Classes;

type
  PBCStyleHeader = ^TBCStyleHeader;
  TBCStyleHeader = record
    Author: String;
    ControlClass: String;
    Description: String;
  end;

// Function return data of specified section (header, properties, etc).
// This is smart function, because it doesn't read whole file but read file
// line by line and return only needed section. So it should fastest for reading
// header info instead of TIniFile object which read, parse and index all file.
function GetSectionData(const AFileName, ASectionName: String): TStrings;
// Methods which read header from list or file and parse it into pascal record
procedure GetStyleHeader(const AFileName: String; AOutHeader: PBCStyleHeader);
// Function check if specified name is on ignored list
function IsPropIgnored(const AName: String): Boolean;
// Method load style saved by SaveStyle method
procedure LoadStyle(AControl: TObject; const AFileName: String; ALogs: TStrings = nil);
// Method save all (which are not on ignored list or readonly) public propertys to
// the output string list. This method have support for property
// tree (Propert1.Subpropert1.Color = 543467). Values are represented as "human readable"
// (e.g. Align = alClient). Header info is save too.
procedure SaveStyle(AControl: TObject; const AAuthor, ADescription: String;
  ATargetList: TStrings);

Implementation
End.

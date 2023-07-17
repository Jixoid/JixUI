// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAStreamLayers;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  BGRAClasses, SysUtils, BGRALayers, BGRABitmap, BGRALzpCommon, BGRAMemDirectory;

function CheckStreamForLayers(AStream: TStream): boolean;
function LoadLayersFromStream(AStream: TStream; out ASelectedLayerIndex: integer; ALoadLayerUniqueIds: boolean = false;
         ADestination: TBGRALayeredBitmap = nil; AProgress: boolean = false): TBGRALayeredBitmap;
procedure SaveLayersToStream(AStream: TStream; ALayers: TBGRACustomLayeredBitmap; ASelectedLayerIndex: integer;
         ACompression: TLzpCompression = lzpZStream; AProgress: boolean = false);
procedure SaveLayerBitmapToStream(AStream: TStream; ABitmap: TBGRABitmap; ACaption: string; ACompression: TLzpCompression = lzpZStream);
function LoadLayerBitmapFromStream(AStream: TStream; ACompression: TLzpCompression = lzpZStream) : TBGRABitmap;
procedure RegisterStreamLayers;

Implementation
End.

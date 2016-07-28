unit miscfunc;
{$mode objfpc}{$H+}
interface

uses
  SysUtils, Classes, base64, fpg_main, fpg_base, fpg_imgfmt_png;

type
  TArray = array of string;

function explode(cDelimiter,  sValue : string; iCount : integer) : TArray;  
procedure DecodeImageData(s: String; var img: TfpgImage);

{$I imgdata.inc}
{$I version.inc}

implementation

function explode(cDelimiter,  sValue : string; iCount : integer) : TArray;
var
  s : string;
  i,p : integer;
begin
  s := sValue; i := 0;
  while length(s) > 0 do
  begin
    inc(i);
    SetLength(result, i);
    p := pos(cDelimiter,s);
    if ( p > 0 ) and ( ( i < iCount ) OR ( iCount = 0) ) then
    begin
      result[i - 1] := copy(s,0,p-1);
      s := copy(s,p + length(cDelimiter),length(s));
    end else
    begin
      result[i - 1] := s;
      s :=  '';
    end;
  end;
end;

procedure DecodeImageData(s: String; var img: TfpgImage);
var
  decoder: TBase64DecodingStream;
  ss: TStringStream;
  m: TMemoryStream;
begin
  ss := TStringStream.Create(s);
  decoder := TBase64DecodingStream.Create(ss);
  m := TMemoryStream.Create;
  m.CopyFrom(decoder,decoder.Size);
  img := LoadImage_PNG(m);
  decoder.Free;
  ss.Free;
  m.Free;
end;

end.

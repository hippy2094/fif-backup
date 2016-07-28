unit miscfunc;
{$mode objfpc}{$H+}
interface

uses
  SysUtils, Classes, base64, fpg_main, fpg_base, fpg_imgfmt_png, 
  fphttpclient{$IFDEF MSWINDOWS}, Windows{$ENDIF};

type
  TArray = array of string;

function explode(cDelimiter,  sValue : string; iCount : integer) : TArray;  
procedure DecodeImageData(s: String; var img: TfpgImage);
{$IFDEF MSWINDOWS}function getWinVer: String;{$ENDIF}
function httpGet(URL: String): String;

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

{$IFDEF MSWINDOWS}
function getWinVer: String;
var
  VerInfo: TOSVersioninfo;
  nt: String;
begin
  nt := '';
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(VerInfo);
  if VerInfo.dwPlatformId = VER_PLATFORM_WIN32_NT then nt := 'NT ';
  Result := 'Windows '+nt+IntToStr(VerInfo.dwMajorVersion) + '.' + IntToStr(VerInfo.dwMinorVersion);
end;
{$ENDIF}

function httpGet(URL: String): String;
var
  HTTP: TFPHttpClient;
  OS: String;
begin
  {$ifdef Windows}
  OS := getWinVer;
  {$endif}
  {$ifdef Linux}
  OS := 'Linux';
  {$endif}
  {$ifdef FreeBSD}
  OS := 'FreeBSD';
  {$endif}
  {$ifdef Darwin}
  OS := 'OSX';
  {$endif}
  Result := '';
  HTTP := TFPHttpClient.Create(nil);
  HTTP.RequestHeaders.Add('User-Agent: Mozilla/5.0 (compatible; '+OS+'; '+APPNAME+' '+APPVER+' ('+IntToStr(CURRVER)+'))');
  Result := HTTP.Get(URL);
  HTTP.Free;
end;

end.

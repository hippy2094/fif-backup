program fif;
{mode objfpc}{$H+}

uses SysUtils, Classes, fpg_base, fpg_main, fpg_stylemanager, fpg_style_win8, main;

const
  APPNAME = 'easyfinder';
  APPVER = '0.1';
  CURRVER = 20160727;

procedure main;
var
  frm: TfrmMain;
  sfimg: TfpgImage;
  progicon: TfpgImage;
begin
  fpgApplication.Initialize;
  if fpgStyleManager.SetStyle('win8') then fpgStyle := fpgStyleManager.Style;
  frm := TfrmMain.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  main;
end.

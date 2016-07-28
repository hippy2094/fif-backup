program fif;
{mode objfpc}{$H+}

uses SysUtils, Classes, fpg_base, fpg_main, fpg_stylemanager, miscfunc,
  fpg_style_win8, main;

procedure main;
var
  frm: TfrmMain;
  sfimg: TfpgImage;
  progicon: TfpgImage;
begin
  fpgApplication.Initialize;
  if fpgStyleManager.SetStyle('Carbon') then fpgStyle := fpgStyleManager.Style;
  sfimg := TfpgImage.Create;
  DecodeImageData(sflogo,sfimg);
  progicon := TfpgImage.Create;
  DecodeImageData(appicon,progicon);
  fpgImages.AddImage('sflogo',sfimg);
  fpgImages.AddImage('appicon',progicon);  
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

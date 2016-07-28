unit about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, LclIntf, ExtCtrls;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnOK: TButton;
    imgLaz: TImage;
    imgTwitter: TImage;
    sfImage: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure imgLazClick(Sender: TObject);
    procedure imgTwitterClick(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure sfImageClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    apptitle: String;
  end; 

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  Label1.Width := frmAbout.ClientWidth;
  Label2.Width := frmAbout.ClientWidth;
  Label3.Width := frmAbout.ClientWidth;
  Label4.Width := frmAbout.ClientWidth;
  Label5.Width := frmAbout.ClientWidth;
end;

procedure TfrmAbout.imgLazClick(Sender: TObject);
begin
  OpenURL('http://www.lazarus-ide.org');
end;

procedure TfrmAbout.imgTwitterClick(Sender: TObject);
begin
  OpenURL('https://twitter.com/hippy2094');
end;

procedure TfrmAbout.Label4Click(Sender: TObject);
begin
  OpenURL('http://www.matthewhipkin.co.uk');
end;

procedure TfrmAbout.sfImageClick(Sender: TObject);
begin
  OpenURL('https://sourceforge.net/p/fif/');
end;

end.


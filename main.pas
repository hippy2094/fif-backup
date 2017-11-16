unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, EditBtn, LCLIntF, miscfunc, scanthread;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnGo: TButton;
    btnAbout: TButton;
    checkRecurse: TCheckBox;
    checkMatchCase: TCheckBox;
    checkWholeWords: TCheckBox;
    Label4: TLabel;
    Panel2: TPanel;
    textSearch: TComboBox;
    Label3: TLabel;
    textExt: TComboBox;
    textDir: TDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    listResults: TListView;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    updateTimer: TTimer;
    procedure btnAboutClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure updateTimerTimer(Sender: TObject);
  private
    { private declarations }
    stop: Boolean;
    appdir: String;
    scanner: TScanThread;
    procedure LoadHistory;
    procedure SaveHistory;
    procedure scannerOnTextFound(r: TScanResult);
    procedure scannerOnTerminate(Sender: TObject);
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses about;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.LoadHistory;
var
  f: TStrings;
  ext: TArray;
  i: integer;
begin
  f := TStringList.Create;
  if FileExists(appdir + 'history.txt') then
  begin
    f.LoadFromFile(appdir + 'history.txt');
    // Line 0 = file extensions
    ext := explode(':',f[0],0);
    for i := 0 to High(ext) do
    begin
      textExt.Items.Add(ext[i]);
    end;
    for i := 1 to f.Count -1 do
    begin
      textSearch.Items.Add(f[i]);
    end;
  end;
  f.Free;
end;

procedure TfrmMain.SaveHistory;
var
  f: TStrings;
  ext: TArray;
  i: integer;
begin
  f := TStringList.Create;
  SetLength(ext,textExt.Items.Count);
  for i := 0 to textExt.Items.Count -1 do
  begin
    ext[i] := textExt.Items[i];
  end;
  f.Add(implode(':',ext));
  f.AddStrings(textSearch.Items);
  f.SaveToFile(appdir + 'history.txt');
  f.Free;
end;

procedure TfrmMain.scannerOnTextFound(r: TScanResult);
var
  Item: TListItem;
begin
  Item := listResults.Items.Add;
  Item.Caption := ExtractFileName(r.FileName);
  Item.SubItems.Add(ExtractFilePath(r.FileName));
  Item.SubItems.Add(IntToStr(r.LineNumber));
  Item.SubItems.Add(r.LineText);
end;

procedure TfrmMain.scannerOnTerminate(Sender: TObject);
begin
  showmessage('Done');
  StatusBar1.SimpleText := IntToStr(listResults.Items.Count) + ' results';
  textExt.Items.Add(textExt.Text);
  textSearch.Items.Add(textSearch.Text);
  SaveHistory;
  scanner.Free;
end;

procedure TfrmMain.btnGoClick(Sender: TObject);
var
  r: TScanOptions;
begin
  StatusBar1.SimpleText := '';
  scanner := TScanThread.Create(true);
  scanner.FreeOnTerminate := false;
  scanner.AddDirectory(textDir.Directory);
  scanner.AddFileType(textExt.Text);
  r.Recursive := checkRecurse.Checked;
  r.SearchText := textSearch.Text;
  r.MatchCase := checkMatchCase.Checked;
  scanner.SetOptions(r);
  scanner.OnTextFound := @scannerOnTextFound;
  scanner.OnTerminate := @scannerOnTerminate;
  scanner.Start;
end;

procedure TfrmMain.btnAboutClick(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  frmMain.Caption := APPNAME + ' ' + APPVER;
  Application.Title := APPNAME;
  appdir :=  GetUserDir + '.fif' + PathDelim;
  textExt.Text := '';
  textSearch.Text := '';
  if not DirectoryExists(appdir) then mkdir(appdir);
  LoadHistory;
end;

procedure TfrmMain.Label4Click(Sender: TObject);
begin
  OpenURL('http://www.matthewhipkin.co.uk');
end;

procedure TfrmMain.updateTimerTimer(Sender: TObject);
var
  response: String;
  newVer: Boolean;
begin
  updateTimer.Enabled := false;
  newVer := false;
  try
    response := httpGet('http://www.matthewhipkin.co.uk/fif.txt');
    response := trim(response);
    if CURRVER < StrToInt(response) then newVer := true;
  except
    newVer := false;
  end;
  if newVer then
  begin
    Panel2.Visible := true;
  end;
end;

end.


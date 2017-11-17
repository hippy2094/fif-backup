unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, EditBtn, LCLIntF, StrUtils, MD_Label, miscfunc, scanthread;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnGo: TButton;
    btnAbout: TButton;
    btnStop: TButton;
    checkRecurse: TCheckBox;
    checkMatchCase: TCheckBox;
    checkWholeWords: TCheckBox;
    Label4: TLabel;
    labelCurrentDir: TLabel;
    labelFileCount: TLabel;
    labelResultCount: TLabel;
    Panel2: TPanel;
    panelProgress: TPanel;
    ScrollBox1: TScrollBox;
    textSearch: TComboBox;
    Label3: TLabel;
    textExt: TComboBox;
    textDir: TDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    updateTimer: TTimer;
    procedure btnAboutClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure ResultLabelMouseLeave(Sender: TObject);
    procedure ResultLabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure updateTimerTimer(Sender: TObject);
  private
    { private declarations }
    appdir: String;
    scanner: TScanThread;
    procedure LoadHistory;
    procedure SaveHistory;
    procedure scannerOnTextFound(r: TScanResult);
    procedure scannerOnTerminate(Sender: TObject);
    procedure scannerOnUpdateCount(fc: Integer; rc: Integer);
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
  l: TMDLabel;
  html: TStrings;
  mt: String;
begin
  html := TStringList.Create;
  html.Add('<fs:10><fs:12><t>'+ExtractFilename(r.FileName)+'</fs><t:300>Line: '+IntToStr(r.LineNumber)+'<br>');
  html.Add('<t>'+ExtractFilePath(r.FileName)+'<br><br>');
  mt := r.LineText;
  mt.Replace('&','&amp;');
  mt.Replace('<','&lt;');
  mt.Replace('>','&gt;');
  mt := AnsiReplaceStr(mt,r.MatchedText,'<b>' + r.MatchedText + '</b>');
  html.Add('<t>'+mt+'<br></fs>');
  l := TMDLabel.Create(Self);
  l.ParentFont := true;
  l.Color := clWhite;
  l.Parent := ScrollBox1;
  l.AutoSizeHeight:=true;
  l.AutoSizeWidth:=false;
  l.Width := ScrollBox1.ClientWidth;
  l.Top := 0;
  l.Left := 0;
  l.BorderStyle := bsNone;
  l.BorderWidth := 0;
  l.CompressSpaces := false;
  l.Caption := html.Text;
  l.Visible := true;
  l.Align := alTop;
  l.BorderStyle := bsSingle;
  l.BorderWidth := 1;
  l.OnMouseMove := @ResultLabelMouseMove;
  l.OnMouseLeave := @ResultLabelMouseLeave;
  html.Free;
end;

procedure TfrmMain.scannerOnUpdateCount(fc: Integer; rc: Integer);
begin
  labelCurrentDir.Caption := scanner.CurrentPath;
  labelFileCount.Caption := Format('%d files scanned',[fc]);
  labelResultCount.Caption := Format('%d results found',[rc]);
  //StatusBar1.SimpleText := Format('Scanning %s: %d files scanned, %d results found',[scanner.CurrentPath, fc, rc]);
  Application.ProcessMessages;
end;

procedure TfrmMain.scannerOnTerminate(Sender: TObject);
begin
//  showmessage('Done');
//  StatusBar1.SimpleText := IntToStr(listResults.Items.Count) + ' results';
  panelProgress.Visible := true;
  textExt.Items.Add(textExt.Text);
  textSearch.Items.Add(textSearch.Text);
  SaveHistory;
  scanner.Free;
end;

procedure TfrmMain.ResultLabelMouseLeave(Sender: TObject);
begin
  if Sender is TMDLabel then
  begin
    if (Sender as TMDLabel).Color = clActiveCaption then
      (Sender as TMDLabel).Color := clWhite;
  end;
end;

procedure TfrmMain.ResultLabelMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Sender is TMDLabel then
  begin
    if (Sender as TMDLabel).Color = clWhite then
      (Sender as TMDLabel).Color := clActiveCaption;
  end;
end;


procedure TfrmMain.btnGoClick(Sender: TObject);
var
  r: TScanOptions;
begin
  StatusBar1.SimpleText := '';
  panelProgress.Visible := true;
  Application.ProcessMessages;
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
  scanner.OnUpdateCount := @scannerOnUpdateCount;
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


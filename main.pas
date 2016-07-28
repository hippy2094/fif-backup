unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, EditBtn, miscfunc;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnGo: TButton;
    checkRecurse: TCheckBox;
    checkMatchCase: TCheckBox;
    checkWholeWords: TCheckBox;
    textSearch: TComboBox;
    Label3: TLabel;
    textExt: TComboBox;
    textDir: TDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    listResults: TListView;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    procedure btnGoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    stop: Boolean;
    fileList: TStrings;
    procedure createFileList(mask: String; path: String; recurse: Boolean);
    procedure scanFiles;
    procedure addFind(line: integer; filename: string; content: string; matchedword: String);
    procedure LoadHistory;
    procedure SaveHistory;
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.LoadHistory;
var
  f: TStrings;
begin
  // Line 0 = file extensions
  f := TStringList.Create;
  f.Free;
end;

procedure TfrmMain.SaveHistory;
var
  f: TStrings;
begin
  f := TStringList.Create;
  f.Free;
end;

procedure TfrmMain.createFileList(mask: String; path: String; recurse: Boolean);
var
  s: TSearchRec;
  searchStr: string;
  fp: string;
  masks: TArray;
  i: integer;
begin
  masks := explode(';',mask,0);
  fp := IncludeTrailingPathDelimiter(path);
  searchStr := fp + '*';
  if FindFirst(searchStr, faAnyFile, s) = 0 then
  begin
    repeat
      if stop = true then exit;
      if (s.Attr and faDirectory) <> faDirectory then
      begin
        for i := Low(masks) to High(masks) do
        begin
          if ExtractFileExt(s.Name) = ExtractFileExt(trim(masks[i])) then
	      begin
	        fileList.Add(fp + s.Name);
          end;
        end;
      end
      else
      begin
        if (s.Name <> '.') and (s.Name <> '..') and (recurse = true) then
          createFileList(mask,fp + s.Name, true);
      end;
    until FindNext(s) <> 0;
  end;
end;

procedure TfrmMain.scanFiles;
var
  i,j,x: integer;
  c: integer;
  F: TextFile;
  line: String;
  words: TArray;
  sWords: TArray;
begin
  sWords := explode(' ',textSearch.Text,0);
  for i := 0 to fileList.Count -1 do
  begin
    AssignFile(F,fileList[i]);
    Reset(F);
    c := 0;
    while not eof(F) do
    begin
      Readln(F,line);
      inc(c);
      if checkWholeWords.Checked then
      begin
	    words := explode(' ',line,0);
	    for j := Low(words) to High(words) do
	    begin
	      for x := Low(sWords) to High(sWords) do
	      begin
	        if checkMatchCase.Checked = true then
	        begin
	          if sWords[x] = words[j] then addFind(c,fileList[i],line,sWords[x]);
	        end
	        else
	        begin
	          if Lowercase(sWords[x]) = Lowercase(words[j]) then addFind(c,fileList[i],line,sWords[x]);
	        end;
	      end;
	    end;
      end
      else
      begin
        if checkMatchCase.Checked = true then
        begin
          for x := Low(sWords) to High(sWords) do
          begin
            if AnsiPos(sWords[x],line) > 0 then addFind(c,fileList[i],line,sWords[x]);
          end;
        end
        else
        begin
          for x := Low(sWords) to High(sWords) do
          begin
            if AnsiPos(Lowercase(sWords[x]),Lowercase(line)) > 0 then addFind(c,fileList[i],line,sWords[x]);
          end;
        end;
      end;
    end;
    CloseFile(F);
  end;
end;

procedure TfrmMain.addFind(line: integer; filename: string; content: string; matchedword: String);
var
  Item: TListItem;
begin
  Item := listResults.Items.Add;
  Item.Caption := ExtractFileName(filename);
  Item.SubItems.Add(ExtractFilePath(filename));
  Item.SubItems.Add(IntToStr(line));
  Item.SubItems.Add(content);
end;

procedure TfrmMain.btnGoClick(Sender: TObject);
begin
  fileList.Clear;
  createFileList(textExt.Text,textDir.Directory,checkRecurse.Checked);
  if fileList.Count > 0 then scanFiles;
  showmessage('Done');
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  fileList := TStringList.Create;
end;

end.


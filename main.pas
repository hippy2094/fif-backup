unit main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_button, fpg_listview, 
  fpg_editbtn, fpg_checkbox, fpg_editcombo, fpg_panel, fpg_edit, fpg_label;

type
  TFileEntry = record
    path: String;
    filename: String;
    line: integer;
    content: String;
  end;
  TfrmMain = class(TfpgForm)
  private
    textDir: TfpgDirectoryEdit;
    checkRecurse: TfpgCheckBox;
    checkWholeWords: TfpgCheckBox;
    checkMatchCase: TfpgCheckBox;
    btnGo: TfpgButton;
    listResults: TfpgListView;
    textExt: TfpgEditCombo;
    textSearch: TfpgEdit;
    panelTop: TfpgPanel;
    Label1: TfpgLabel;
    Label2: TfpgLabel;
    Label3: TfpgLabel;
    stop: Boolean;
    procedure AfterCreate; override;
    procedure btnGoClick(Sender: TObject);
  public
    fileList: TStrings;
    procedure createFileList(mask: String; path: String; recurse: Boolean);
    procedure scanFiles;
  end;

implementation

procedure TfrmMain.createFileList(mask: String; path: String; recurse: Boolean);
var
  s: TSearchRec;
  searchStr: string;
  fp: string;
begin
  // TODO: Check mask for multiple extensions
  fp := IncludeTrailingPathDelimiter(path);
  searchStr := fp + '*';
  if FindFirst(searchStr, faAnyFile, s) = 0 then
  begin
    repeat
      if stop = true then exit;
      if (s.Attr and faDirectory) <> faDirectory then
      begin
        if ExtractFileExt(s.Name) = ExtractFileExt(mask) then
	begin
	  fileList.Add(fp + s.Name);
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
  i: integer;
  c: integer;
  F: TextFile;
  line: String;
begin
  for i := 0 to fileList.Count -1 do
  begin
    AssignFile(F,fileList[i]);
    Reset(F);
    c := 1;
    while not eof(F) do
    begin
      Readln(F,line);
    end;
    CloseFile(F);
  end;
end;

procedure TfrmMain.btnGoClick(Sender: TObject);
begin
  fileList.Clear;
  createFileList(textExt.Text,textDir.Directory,checkRecurse.Checked);
  //writeln(fileList.Count);
  
end;

procedure TfrmMain.AfterCreate;
var
  LVColumn: TfpgLVColumn;
begin
  Name := 'frmMain';
  SetPosition(422, 153, 567, 331);
  WindowTitle := 'frmMain';
  Hint := '';
  IconName := '';

  panelTop := TfpgPanel.Create(self);
  with panelTop do
  begin
    SetPosition(0,0,400,100);
    Align := alTop;
    Text := '';
  end;

  Label1 := TfpgLabel.Create(panelTop);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(8, 12, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Directory';
  end;

  textDir := TfpgDirectoryEdit.Create(panelTop);
  with textDir do
  begin
    Name := 'textDir';
    SetPosition(8, 32, 140, 24);
    Directory := '';
    ExtraHint := '';
    RootDirectory := '';
    TabOrder := 2;
  end;

  checkRecurse := TfpgCheckBox.Create(panelTop);
  with checkRecurse do
  begin
    Name := 'checkRecurse';
    SetPosition(8, 64, 120, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 3;
    Text := 'Recurse';
  end;

  checkWholeWords := TfpgCheckBox.Create(panelTop);
  with checkWholeWords do
  begin
    Name := 'checkWholeWords';
    SetPosition(284, 64, 150, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 5;
    Text := 'Match Whole Words';
  end;

  checkMatchCase := TfpgCheckBox.Create(panelTop);
  with checkMatchCase do
  begin
    Name := 'checkMatchCase';
    SetPosition(152, 64, 120, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 4;
    Text := 'Match Case';
  end;

  textExt := TfpgEditCombo.Create(panelTop);
  with textExt do
  begin
    Name := 'textExt';
    SetPosition(156, 32, 120, 24);
    AutoCompletion := true;
    AllowNew := anYes;
  end;

  Label2 := TfpgLabel.Create(panelTop);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(156, 12, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'File Types';
  end;

  textSearch := TfpgEdit.Create(panelTop);
  with textSearch do
  begin
    Name := 'textSearch';
    SetPosition(284, 32, 272, 24);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 8;
    Text := '';
  end;

  Label3 := TfpgLabel.Create(panelTop);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(284, 12, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Search Text';
  end;

  btnGo := TfpgButton.Create(panelTop);
  with btnGo do
  begin
    Name := 'btnGo';
    SetPosition(480, 64, 80, 24);
    Text := 'Go';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 10;
    OnClick := @btnGoClick;
  end;
  
  listResults := TfpgListView.Create(self);
  with listResults do
  begin
    Name := 'listResults';
    SetPosition(0, 83, 567, 248);
    Align := alClient;
    Hint := '';
    MultiSelect := False;
    ShowHeaders := True;
    ShowHeaders := True;
    TabOrder := 4;
  end;

  LVColumn := TfpgLVColumn.Create(listResults.Columns);
  LVColumn.Caption := 'Filename';
  LVColumn.Width := 150;
  LVColumn.Height := 50;
  LVColumn.Resizable := True;
  LVColumn.Alignment := taLeftJustify;
  LVColumn.ColumnIndex := 0;
  listResults.Columns.Add(LVColumn);

  LVColumn := TfpgLVColumn.Create(listResults.Columns);
  LVColumn.Caption := 'Path';
  LVColumn.Width := 150;
  LVColumn.Height := 50;
  LVColumn.Resizable := True;
  LVColumn.Alignment := taLeftJustify;
  LVColumn.ColumnIndex := 1;
  listResults.Columns.Add(LVColumn);

  LVColumn := TfpgLVColumn.Create(listResults.Columns);
  LVColumn.Caption := 'Line Number';
  LVColumn.Width := 150;
  LVColumn.Height := 50;
  LVColumn.Resizable := True;
  LVColumn.Alignment := taLeftJustify;
  LVColumn.ColumnIndex := 2;
  listResults.Columns.Add(LVColumn);

  LVColumn := TfpgLVColumn.Create(listResults.Columns);
  LVColumn.Caption := 'Line Content';
  LVColumn.Width := 200;
  LVColumn.AutoSize := true;
  LVColumn.Height := 50;
  LVColumn.Resizable := True;
  LVColumn.Alignment := taLeftJustify;
  LVColumn.ColumnIndex := 3;
  listResults.Columns.Add(LVColumn);

  stop := false;
  fileList := TStringList.Create;
end;

end.

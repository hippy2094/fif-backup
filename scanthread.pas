unit scanthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, miscfunc;

type
  TScanResult = class(TObject)
    FileName: String;
    LineNumber: Integer;
    MatchedText: String;
  end;
  TWordMode = (wrmNone, wrmWholeWords, wrmExactMatch);
  TScanOptions = record
    SearchText: String;
    Recursive: Boolean;
    MatchCase: Boolean;
    WordMode: TWordMode;
  end;
  TUpdateCountEvent = procedure(c: Integer) of Object;
  TResponseComplete = procedure(c: Integer) of Object;
  TScanThread = class(TThread)
    private
      FCurrentIndex: Integer;
      FOnUpdateCount: TUpdateCountEvent;
      FResponseComplete: TResponseComplete;
      FScanOptions: TScanOptions;
      FScanDirectories: TStrings;
      FFileTypes: TStrings;
      FFiles: TStrings;
      procedure PerformScan;
      procedure UpdateCount;
      procedure ResponseComplete;
      procedure CreateFileList(mask: String; path: String);
    protected
      procedure Execute; override;
    public
      HasFinished: Boolean;
      stop: Boolean;
      constructor Create(CreateSuspended: boolean);
      procedure AddDirectory(dir: String);
      procedure AddFileType(f: String);
      property ScanOptions: TScanOptions read FScanOptions write FScanOptions;
      property OnUpdateCount: TUpdateCountEvent read FOnUpdateCount write FOnUpdateCount;
      property OnResponseComplete: TResponseComplete read FResponseComplete write FResponseComplete;
  end;

implementation

{ TScanThread }
constructor TScanThread.Create(CreateSuspended: Boolean);
begin
  FCurrentIndex := 0;
  FScanDirectories := TStringList.Create;
  FFileTypes := TStringList.Create;
  FFiles := TStringList.Create;
  HasFinished := false;
  stop := false;
  inherited Create(CreateSuspended);
end;

procedure TScanThread.AddDirectory(dir: String);
begin
  if not InTStrings(dir, FScanDirectories) then
  begin
    FScanDirectories.Add(dir);
  end;
end;

procedure TScanThread.AddFileType(f: String);
begin
  if not InTStrings(f, FFileTypes) then
  begin
    FFileTypes.Add(f);
  end;
end;

procedure TScanThread.CreateFileList(mask: String; path: String);
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
	    FFiles.Add(fp + s.Name);
          end;
        end;
      end
      else
      begin
        if (s.Name <> '.') and (s.Name <> '..') and (FScanOptions.Recursive) then
          createFileList(mask,fp + s.Name);
      end;
    until FindNext(s) <> 0;
  end;
end;

procedure TScanThread.PerformScan;
var
  i, j: integer;
begin
  FFiles.Clear;
  for i := 0 to FScanDirectories.Count -1 do
  begin
    for j := 0 to FFileTypes.Count -1 do
    begin
      CreateFileList(FFileTypes[j],FScanDirectories[i]);
    end;
  end;
  HasFinished := true;
end;

procedure TScanThread.UpdateCount;
begin
  if Assigned(FOnUpdateCount) then
  begin
    FOnUpdateCount(FCurrentIndex);
  end;
end;

procedure TScanThread.ResponseComplete;
begin
  if Assigned(FResponseComplete) then
  begin
    FResponseComplete(FCurrentIndex);
  end;
end;

procedure TScanThread.Execute;
begin
  PerformScan;
end;

end.


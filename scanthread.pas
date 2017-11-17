unit scanthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, miscfunc;

type
  TWordMode = (wrmNone, wrmWholeWords, wrmExactMatch);
  TScanResult = record
    FileName: String;
    LineNumber: Integer;
    MatchedText: String;
    LineText: String;
  end;
  TScanOptions = record
    SearchText: String;
    Recursive: Boolean;
    MatchCase: Boolean;
    WordMode: TWordMode;
  end;
  TUpdateCountEvent = procedure(fc: Integer; rc: Integer) of Object;
  TTextFoundEvent = procedure(r: TScanResult) of Object;
  TScanThread = class(TThread)
    private
      FOnUpdateCount: TUpdateCountEvent;
      FOnTextFound: TTextFoundEvent;
      FScanOptions: TScanOptions;
      FScanDirectories: TStrings;
      FFileTypes: TStrings;
      FLastFileResult: TScanResult;
      FFilesCounted: Integer;
      FResultsFound: Integer;
      FCurrentPath: String;
      procedure PerformScan;
      procedure UpdateCount;
      procedure TextFound;
      procedure CreateFileList(mask: String; path: String);
      procedure AddFind(line: integer; filename: string; content: string;
        matchedword: String);
      procedure ScanFile(filename: String);
    protected
      procedure Execute; override;
    public
      HasFinished: Boolean;
      stop: Boolean;
      constructor Create(CreateSuspended: boolean);
      procedure AddDirectory(dir: String);
      procedure AddFileType(f: String);
      procedure SetOptions(o: TScanOptions);
      property CurrentPath: String read FCurrentPath;
      property ScanOptions: TScanOptions read FScanOptions write FScanOptions;
      property OnUpdateCount: TUpdateCountEvent read FOnUpdateCount write FOnUpdateCount;
      property OnTextFound: TTextFoundEvent read FOnTextFound write FOnTextFound;
  end;

implementation

{ TScanThread }
constructor TScanThread.Create(CreateSuspended: Boolean);
begin
  FFilesCounted := 0;
  FResultsFound := 0;
  FScanDirectories := TStringList.Create;
  FFileTypes := TStringList.Create;
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
  FCurrentPath := path;
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
            inc(FFilesCounted);
	    ScanFile(fp + s.Name);
          end;
        end;
      end
      else
      begin
        if (s.Name <> '.') and (s.Name <> '..') and (FScanOptions.Recursive) then
          createFileList(mask,fp + s.Name);
      end;
      Synchronize(@UpdateCount);
    until FindNext(s) <> 0;
  end;
end;

procedure TScanThread.AddFind(line: integer; filename: string; content: string;
  matchedword: String);
begin
  inc(FResultsFound);
  FLastFileResult.FileName := filename;
  FLastFileResult.LineNumber := line;
  FLastFileResult.MatchedText := matchedword;
  FLastFileResult.LineText := content;
  Synchronize(@TextFound);
end;

procedure TScanThread.ScanFile(filename: String);
var
  i,j,x: integer;
  c: integer;
  F: TextFile;
  line: String;
  words: TArray;
  sWords: TArray;
begin
  sWords := explode(' ',FScanOptions.SearchText,0);
  AssignFile(F,filename);
  Reset(F);
  c := 0;
  while not eof(F) do
  begin
    Readln(F,line);
    inc(c);
    if FScanOptions.WordMode = wrmWholeWords then
    begin
      words := explode(' ',line,0);
      for j := Low(words) to High(words) do
      begin
        for x := Low(sWords) to High(sWords) do
        begin
          if FScanOptions.MatchCase then
          begin
            if sWords[x] = words[j] then AddFind(c,filename,line,sWords[x]);
          end
          else
          begin
            if Lowercase(sWords[x]) = Lowercase(words[j]) then AddFind(c,filename,line,sWords[x]);
          end;
        end;
      end;
    end
    else if FScanOptions.WordMode = wrmExactMatch then
    begin
      if FScanOptions.MatchCase then
      begin
        if AnsiPos(FScanOptions.SearchText,line) > 0 then
        begin
          AddFind(c,filename,line,FScanOptions.SearchText);
        end;
      end
      else
      begin
        if AnsiPos(Lowercase(FScanOptions.SearchText),Lowercase(line)) > 0 then
        begin
          AddFind(c,filename,line,FScanOptions.SearchText);
        end;
      end;
    end
    else
    begin
      if FScanOptions.MatchCase then
      begin
        for x := Low(sWords) to High(sWords) do
        begin
          if AnsiPos(sWords[x],line) > 0 then AddFind(c,filename,line,sWords[x]);
        end;
      end
      else
      begin
        for x := Low(sWords) to High(sWords) do
        begin
          if AnsiPos(Lowercase(sWords[x]),Lowercase(line)) > 0 then AddFind(c,filename,line,sWords[x]);
        end;
      end;
    end;
  end;
  CloseFile(F);
end;

procedure TScanThread.PerformScan;
var
  i, j: integer;
begin
  for i := 0 to FScanDirectories.Count -1 do
  begin
    for j := 0 to FFileTypes.Count -1 do
    begin
      CreateFileList(FFileTypes[j],FScanDirectories[i]);
    end;
  end;
  HasFinished := true;
end;

procedure TScanThread.SetOptions(o: TScanOptions);
begin
  FScanOptions := o;
end;

procedure TScanThread.UpdateCount;
begin
  if Assigned(FOnUpdateCount) then
  begin
    FOnUpdateCount(FFilesCounted, FResultsFound);
  end;
end;

procedure TScanThread.TextFound;
begin
  if Assigned(FOnTextFound) then
  begin
    FOnTextFound(FLastFileResult);
  end;
end;

procedure TScanThread.Execute;
begin
  PerformScan;
end;

end.


unit TimestampMetadata;

{Per-directory metadata file for storing file timestamps.
	Tab-separated format: filename<TAB>local_mtime<TAB>cloud_mtime
	Mirrors TDescription pattern but stores timestamp pairs instead of text comments.}

interface

uses
	System.SysUtils,
	System.Classes,
	Generics.Collections,
	TimestampEntry,
	FileSystem;

type
	TTimestampMetadata = class
	private
		FItems: TDictionary<WideString, TTimestampEntry>;
		FFileName: WideString;
		FFileSystem: IFileSystem;
	public
		constructor Create(const FileName: WideString; FileSystem: IFileSystem);
		destructor Destroy; override;

		{Read metadata from file. Returns 0 on success, -1 on error.}
		function Read: Integer;

		{Write metadata to file. Returns 0 on success, -1 on error.
			Deletes file if no entries remain.}
		function Write(const TargetFileName: WideString = ''): Integer;

		function GetEntry(const FileName: WideString): TTimestampEntry;
		procedure SetEntry(const FileName: WideString; const Entry: TTimestampEntry);
		function DeleteEntry(const FileName: WideString): Boolean;
		function RenameEntry(const OldName, NewName: WideString): Boolean;
		function HasEntry(const FileName: WideString): Boolean;

		procedure Clear;

		property MetadataFileName: WideString read FFileName;
	end;

implementation

constructor TTimestampMetadata.Create(const FileName: WideString; FileSystem: IFileSystem);
begin
	inherited Create;
	FItems := TDictionary<WideString, TTimestampEntry>.Create;
	FFileName := FileName;
	FFileSystem := FileSystem;

	if not FFileSystem.FileExists(FileName) then
		FFileSystem.CreateEmptyFile(FileName);
end;

destructor TTimestampMetadata.Destroy;
begin
	FItems.Free;
	FFileSystem := nil;
	inherited;
end;

function TTimestampMetadata.Read: Integer;
var
	Reader: TStreamReader;
	Line, FileName: WideString;
	Entry: TTimestampEntry;
	Parts: TArray<string>;
begin
	Result := 0;
	Clear;

	Reader := FFileSystem.OpenTextReader(FFileName, TEncoding.UTF8);
	try
		try
			while not Reader.EndOfStream do
			begin
				Line := Reader.ReadLine;
				if Line = '' then
					Continue;

				Parts := string(Line).Split([#9]); {TAB-separated}
				if Length(Parts) < 3 then
					Continue;

				FileName := Parts[0];
				Entry.LocalMTime := StrToInt64Def(Parts[1], 0);
				Entry.CloudMTime := StrToInt64Def(Parts[2], 0);

				FItems.AddOrSetValue(FileName, Entry);
			end;
		except
			Result := -1;
		end;
	finally
		Reader.Free;
	end;
end;

function TTimestampMetadata.Write(const TargetFileName: WideString): Integer;
var
	Lines: TStringList;
	Key: WideString;
	Entry: TTimestampEntry;
	OutputPath: WideString;
begin
	if TargetFileName <> '' then
		OutputPath := TargetFileName
	else
		OutputPath := FFileName;

	Result := 0;
	FFileSystem.DeleteFile(OutputPath);
	if FItems.Count = 0 then
		Exit;

	Lines := TStringList.Create;
	try
		try
			for Key in FItems.Keys do
			begin
				FItems.TryGetValue(Key, Entry);
				Lines.Add(Key + #9 + IntToStr(Entry.LocalMTime) + #9 + IntToStr(Entry.CloudMTime));
			end;

			FFileSystem.WriteAllLines(OutputPath, Lines, TEncoding.UTF8);
		except
			Result := -1;
		end;
	finally
		Lines.Free;
	end;
end;

function TTimestampMetadata.GetEntry(const FileName: WideString): TTimestampEntry;
begin
	if not FItems.TryGetValue(FileName, Result) then
		Result := TTimestampEntry.Empty;
end;

procedure TTimestampMetadata.SetEntry(const FileName: WideString; const Entry: TTimestampEntry);
begin
	if Entry.IsEmpty then
		DeleteEntry(FileName)
	else
		FItems.AddOrSetValue(FileName, Entry);
end;

function TTimestampMetadata.DeleteEntry(const FileName: WideString): Boolean;
begin
	Result := FItems.ContainsKey(FileName);
	FItems.Remove(FileName);
end;

function TTimestampMetadata.RenameEntry(const OldName, NewName: WideString): Boolean;
var
	Entry: TTimestampEntry;
begin
	if not FItems.TryGetValue(OldName, Entry) then
		Exit(False);

	FItems.AddOrSetValue(NewName, Entry);
	FItems.Remove(OldName);
	Result := True;
end;

function TTimestampMetadata.HasEntry(const FileName: WideString): Boolean;
begin
	Result := FItems.ContainsKey(FileName);
end;

procedure TTimestampMetadata.Clear;
begin
	FItems.Clear;
end;

end.

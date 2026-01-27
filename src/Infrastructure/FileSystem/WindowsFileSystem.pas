unit WindowsFileSystem;

{Abstraction for file system operations, enabling testability without actual disk I/O.
	Used primarily by TDescription for description file handling.}

interface

uses
	System.SysUtils,
	System.Classes,
	Windows;

type
	{TStreamReader wrapper that owns its underlying stream.
		Needed because TStreamReader.Create(Stream) doesn't take ownership.}
	TOwningStreamReader = class(TStreamReader)
	private
		FOwnedStream: TStream;
	public
		constructor Create(AStream: TStream; Encoding: TEncoding);
		destructor Destroy; override;
	end;

	{Interface for file system operations}
	IFileSystem = interface
		['{8A8F24F8-A1B3-4FB0-A328-14DD231E453A}']

		{Checks if a file exists at the specified path}
		function FileExists(const Path: WideString): Boolean;

		{Returns file size in bytes, -1 if file doesn't exist or error}
		function GetFileSize(const Path: WideString): Int64;

		{Creates an empty file at the specified path, overwrites if exists}
		procedure CreateEmptyFile(const Path: WideString);

		{Deletes the file at the specified path, no error if not exists}
		procedure DeleteFile(const Path: WideString);

		{Reads first N bytes from file for BOM detection, returns empty if file not readable}
		function ReadFileHeader(const Path: WideString; ByteCount: Integer): TBytes;

		{Reads entire file content as string with specified encoding}
		function ReadAllText(const Path: WideString; Encoding: TEncoding): WideString;

		{Reads file content line by line with specified encoding}
		function ReadAllLines(const Path: WideString; Encoding: TEncoding): TStringList;

		{Writes content to file with specified encoding, creates or overwrites}
		procedure WriteAllText(const Path: WideString; const Content: WideString; Encoding: TEncoding);

		{Writes lines to file with specified encoding, creates or overwrites}
		procedure WriteAllLines(const Path: WideString; Lines: TStrings; Encoding: TEncoding);

		{Opens a text reader for streaming line-by-line access. Caller must free the returned reader.}
		function OpenTextReader(const Path: WideString; Encoding: TEncoding): TStreamReader;
	end;

	{Null implementation for testing - simulates empty/non-existent files}
	TNullFileSystem = class(TInterfacedObject, IFileSystem)
	public
		function FileExists(const Path: WideString): Boolean;
		function GetFileSize(const Path: WideString): Int64;
		procedure CreateEmptyFile(const Path: WideString);
		procedure DeleteFile(const Path: WideString);
		function ReadFileHeader(const Path: WideString; ByteCount: Integer): TBytes;
		function ReadAllText(const Path: WideString; Encoding: TEncoding): WideString;
		function ReadAllLines(const Path: WideString; Encoding: TEncoding): TStringList;
		procedure WriteAllText(const Path: WideString; const Content: WideString; Encoding: TEncoding);
		procedure WriteAllLines(const Path: WideString; Lines: TStrings; Encoding: TEncoding);
		function OpenTextReader(const Path: WideString; Encoding: TEncoding): TStreamReader;
	end;

	{In-memory file system for testing - stores files in dictionary}
	TMemoryFileSystem = class(TInterfacedObject, IFileSystem)
	private
		FFiles: TStringList; {Path -> Content mapping}
	public
		constructor Create;
		destructor Destroy; override;

		function FileExists(const Path: WideString): Boolean;
		function GetFileSize(const Path: WideString): Int64;
		procedure CreateEmptyFile(const Path: WideString);
		procedure DeleteFile(const Path: WideString);
		function ReadFileHeader(const Path: WideString; ByteCount: Integer): TBytes;
		function ReadAllText(const Path: WideString; Encoding: TEncoding): WideString;
		function ReadAllLines(const Path: WideString; Encoding: TEncoding): TStringList;
		procedure WriteAllText(const Path: WideString; const Content: WideString; Encoding: TEncoding);
		procedure WriteAllLines(const Path: WideString; Lines: TStrings; Encoding: TEncoding);
		function OpenTextReader(const Path: WideString; Encoding: TEncoding): TStreamReader;

		{Test helper: set file content directly}
		procedure SetFileContent(const Path: WideString; const Content: WideString);
		{Test helper: get file content}
		function GetFileContent(const Path: WideString): WideString;
		{Test helper: clear all files}
		procedure Clear;
	end;

	{Windows implementation of IFileSystem using standard RTL and Windows API}
	TWindowsFileSystem = class(TInterfacedObject, IFileSystem)
	public
		function FileExists(const Path: WideString): Boolean;
		function GetFileSize(const Path: WideString): Int64;
		procedure CreateEmptyFile(const Path: WideString);
		procedure DeleteFile(const Path: WideString);
		function ReadFileHeader(const Path: WideString; ByteCount: Integer): TBytes;
		function ReadAllText(const Path: WideString; Encoding: TEncoding): WideString;
		function ReadAllLines(const Path: WideString; Encoding: TEncoding): TStringList;
		procedure WriteAllText(const Path: WideString; const Content: WideString; Encoding: TEncoding);
		procedure WriteAllLines(const Path: WideString; Lines: TStrings; Encoding: TEncoding);
		function OpenTextReader(const Path: WideString; Encoding: TEncoding): TStreamReader;
	end;

implementation

{TOwningStreamReader}

constructor TOwningStreamReader.Create(AStream: TStream; Encoding: TEncoding);
begin
	inherited Create(AStream, Encoding, False);
	FOwnedStream := AStream;
end;

destructor TOwningStreamReader.Destroy;
begin
	inherited Destroy;
	FOwnedStream.Free;
end;

{TNullFileSystem}

function TNullFileSystem.FileExists(const Path: WideString): Boolean;
begin
	Result := False;
end;

function TNullFileSystem.GetFileSize(const Path: WideString): Int64;
begin
	Result := -1; {File doesn't exist in null implementation}
end;

procedure TNullFileSystem.CreateEmptyFile(const Path: WideString);
begin
	{No-op}
end;

procedure TNullFileSystem.DeleteFile(const Path: WideString);
begin
	{No-op}
end;

function TNullFileSystem.ReadFileHeader(const Path: WideString; ByteCount: Integer): TBytes;
begin
	SetLength(Result, 0);
end;

function TNullFileSystem.ReadAllText(const Path: WideString; Encoding: TEncoding): WideString;
begin
	Result := '';
end;

function TNullFileSystem.ReadAllLines(const Path: WideString; Encoding: TEncoding): TStringList;
begin
	Result := TStringList.Create;
end;

procedure TNullFileSystem.WriteAllText(const Path: WideString; const Content: WideString; Encoding: TEncoding);
begin
	{No-op}
end;

procedure TNullFileSystem.WriteAllLines(const Path: WideString; Lines: TStrings; Encoding: TEncoding);
begin
	{No-op}
end;

function TNullFileSystem.OpenTextReader(const Path: WideString; Encoding: TEncoding): TStreamReader;
var
	Stream: TStringStream;
begin
	{Return reader over empty stream, reader owns the stream}
	Stream := TStringStream.Create('', Encoding);
	try
		Result := TOwningStreamReader.Create(Stream, Encoding);
	except
		Stream.Free;
		raise;
	end;
end;

{TMemoryFileSystem}

constructor TMemoryFileSystem.Create;
begin
	inherited Create;
	FFiles := TStringList.Create;
	FFiles.CaseSensitive := False; {Windows-like behavior}
end;

destructor TMemoryFileSystem.Destroy;
begin
	FFiles.Free;
	inherited;
end;

function TMemoryFileSystem.FileExists(const Path: WideString): Boolean;
begin
	Result := FFiles.IndexOfName(Path) >= 0;
end;

function TMemoryFileSystem.GetFileSize(const Path: WideString): Int64;
var
	Content: WideString;
begin
	if not FileExists(Path) then
		Exit(-1);
	Content := GetFileContent(Path);
	Result := Length(TEncoding.UTF8.GetBytes(Content));
end;

procedure TMemoryFileSystem.CreateEmptyFile(const Path: WideString);
begin
	SetFileContent(Path, '');
end;

procedure TMemoryFileSystem.DeleteFile(const Path: WideString);
var
	Index: Integer;
begin
	Index := FFiles.IndexOfName(Path);
	if Index >= 0 then
		FFiles.Delete(Index);
end;

function TMemoryFileSystem.ReadFileHeader(const Path: WideString; ByteCount: Integer): TBytes;
var
	Content: WideString;
	ContentBytes: TBytes;
begin
	Content := GetFileContent(Path);
	if Content = '' then
	begin
		SetLength(Result, 0);
		Exit;
	end;

	ContentBytes := TEncoding.UTF8.GetBytes(Content);
	if Length(ContentBytes) <= ByteCount then
		Result := ContentBytes
	else
	begin
		SetLength(Result, ByteCount);
		Move(ContentBytes[0], Result[0], ByteCount);
	end;
end;

function TMemoryFileSystem.ReadAllText(const Path: WideString; Encoding: TEncoding): WideString;
begin
	Result := GetFileContent(Path);
end;

function TMemoryFileSystem.ReadAllLines(const Path: WideString; Encoding: TEncoding): TStringList;
begin
	Result := TStringList.Create;
	Result.Text := GetFileContent(Path);
end;

procedure TMemoryFileSystem.WriteAllText(const Path: WideString; const Content: WideString; Encoding: TEncoding);
begin
	SetFileContent(Path, Content);
end;

procedure TMemoryFileSystem.WriteAllLines(const Path: WideString; Lines: TStrings; Encoding: TEncoding);
begin
	SetFileContent(Path, Lines.Text);
end;

procedure TMemoryFileSystem.SetFileContent(const Path: WideString; const Content: WideString);
var
	Index: Integer;
begin
	Index := FFiles.IndexOfName(Path);
	if Index >= 0 then
		FFiles.ValueFromIndex[Index] := Content
	else
		FFiles.AddPair(Path, Content);
end;

function TMemoryFileSystem.GetFileContent(const Path: WideString): WideString;
var
	Index: Integer;
begin
	Index := FFiles.IndexOfName(Path);
	if Index >= 0 then
		Result := FFiles.ValueFromIndex[Index]
	else
		Result := '';
end;

procedure TMemoryFileSystem.Clear;
begin
	FFiles.Clear;
end;

function TMemoryFileSystem.OpenTextReader(const Path: WideString; Encoding: TEncoding): TStreamReader;
var
	Stream: TStringStream;
	Content: WideString;
begin
	Content := GetFileContent(Path);
	Stream := TStringStream.Create(Content, Encoding);
	try
		Result := TOwningStreamReader.Create(Stream, Encoding);
	except
		Stream.Free;
		raise;
	end;
end;

{TWindowsFileSystem}

function TWindowsFileSystem.FileExists(const Path: WideString): Boolean;
begin
	Result := System.SysUtils.FileExists(Path);
end;

function TWindowsFileSystem.GetFileSize(const Path: WideString): Int64;
var
	Handle: THandle;
begin
	Handle := CreateFileW(PWideChar(Path), 0, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	if Handle = INVALID_HANDLE_VALUE then
		Exit(-1);
	try
		Int64Rec(Result).Lo := Windows.GetFileSize(Handle, @Int64Rec(Result).Hi);
	finally
		CloseHandle(Handle);
	end;
end;

procedure TWindowsFileSystem.CreateEmptyFile(const Path: WideString);
var
	Handle: THandle;
begin
	Handle := CreateFileW(PWideChar(Path), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
	if Handle <> INVALID_HANDLE_VALUE then
		CloseHandle(Handle);
end;

procedure TWindowsFileSystem.DeleteFile(const Path: WideString);
begin
	DeleteFileW(PWideChar(Path));
end;

function TWindowsFileSystem.ReadFileHeader(const Path: WideString; ByteCount: Integer): TBytes;
var
	F: File;
	BytesRead: Integer;
begin
	SetLength(Result, ByteCount);
	FillChar(Result[0], ByteCount, 0);

	if not System.SysUtils.FileExists(Path) then
	begin
		SetLength(Result, 0);
		Exit;
	end;

	AssignFile(F, Path);
	try
		Reset(F, 1);
		try
			BlockRead(F, Result[0], ByteCount, BytesRead);
			if BytesRead < ByteCount then
				SetLength(Result, BytesRead);
		except
			SetLength(Result, 0);
		end;
	finally
		CloseFile(F);
	end;
end;

function TWindowsFileSystem.ReadAllText(const Path: WideString; Encoding: TEncoding): WideString;
var
	Stream: TStreamReader;
begin
	Result := '';
	if not System.SysUtils.FileExists(Path) then
		Exit;

	Stream := TStreamReader.Create(Path, Encoding, False);
	try
		Result := Stream.ReadToEnd;
	finally
		Stream.Free;
	end;
end;

function TWindowsFileSystem.ReadAllLines(const Path: WideString; Encoding: TEncoding): TStringList;
var
	Stream: TStreamReader;
begin
	Result := TStringList.Create;
	if not System.SysUtils.FileExists(Path) then
		Exit;

	Stream := TStreamReader.Create(Path, Encoding, False);
	try
		while not Stream.EndOfStream do
			Result.Add(Stream.ReadLine);
	finally
		Stream.Free;
	end;
end;

procedure TWindowsFileSystem.WriteAllText(const Path: WideString; const Content: WideString; Encoding: TEncoding);
var
	Stream: TStreamWriter;
begin
	Stream := TStreamWriter.Create(Path, False, Encoding);
	try
		Stream.Write(Content);
		Stream.Flush;
	finally
		Stream.Free;
	end;
end;

procedure TWindowsFileSystem.WriteAllLines(const Path: WideString; Lines: TStrings; Encoding: TEncoding);
var
	Stream: TStreamWriter;
	I: Integer;
begin
	Stream := TStreamWriter.Create(Path, False, Encoding);
	try
		for I := 0 to Lines.Count - 1 do
			Stream.WriteLine(Lines[I]);
		Stream.Flush;
	finally
		Stream.Free;
	end;
end;

function TWindowsFileSystem.OpenTextReader(const Path: WideString; Encoding: TEncoding): TStreamReader;
begin
	Result := TStreamReader.Create(Path, Encoding, False);
end;

end.

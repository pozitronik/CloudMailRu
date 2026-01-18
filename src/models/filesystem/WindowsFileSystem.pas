unit WindowsFileSystem;

{Windows implementation of IFileSystem using standard RTL and Windows API}

interface

uses
	IFileSystemInterface,
	System.SysUtils,
	System.Classes,
	Windows;

type
	TWindowsFileSystem = class(TInterfacedObject, IFileSystem)
	public
		function FileExists(const Path: WideString): Boolean;
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

{TWindowsFileSystem}

function TWindowsFileSystem.FileExists(const Path: WideString): Boolean;
begin
	Result := System.SysUtils.FileExists(Path);
end;

procedure TWindowsFileSystem.CreateEmptyFile(const Path: WideString);
var
	Handle: THandle;
begin
	Handle := CreateFileW(
		PWideChar(Path),
		GENERIC_WRITE,
		0,
		nil,
		CREATE_ALWAYS,
		FILE_ATTRIBUTE_NORMAL,
		0
	);
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
		if Assigned(Lines) then
		begin
			for I := 0 to Lines.Count - 1 do
				Stream.WriteLine(Lines[I]);
		end;
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

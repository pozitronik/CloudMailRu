unit RealSizeFileSystem;

{Real file system for GetFileSize - needed because TFileSplitInfo
	and TChunkedFileStream operate on real files. All other methods are
	stubs. Shared across upload-related test fixtures.}

interface

uses
	System.Classes,
	System.SysUtils,
	Winapi.Windows,
	FileSystem;

type
	TRealSizeFileSystem = class(TInterfacedObject, IFileSystem)
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
		function GetTmpFileName(const Prefix: WideString = ''): WideString;
		procedure SetFileTime(const Path: WideString; const FileTime: TFileTime);
		function FindFiles(const Pattern: WideString): TStringList;
		function GetFileModTime(const Path: WideString): Int64;
	end;

implementation

function TRealSizeFileSystem.FileExists(const Path: WideString): Boolean;
begin
	Result := System.SysUtils.FileExists(Path);
end;

function TRealSizeFileSystem.GetFileSize(const Path: WideString): Int64;
var
	Handle: THandle;
begin
	Handle := CreateFile(PChar(string(Path)), 0, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	if Handle = INVALID_HANDLE_VALUE then
		Result := -1
	else
		try
			Int64Rec(Result).Lo := Winapi.Windows.GetFileSize(Handle, @Int64Rec(Result).Hi);
		finally
			CloseHandle(Handle);
		end;
end;

procedure TRealSizeFileSystem.CreateEmptyFile(const Path: WideString);
begin
end;

procedure TRealSizeFileSystem.DeleteFile(const Path: WideString);
begin
end;

function TRealSizeFileSystem.ReadFileHeader(const Path: WideString; ByteCount: Integer): TBytes;
begin
	SetLength(Result, 0);
end;

function TRealSizeFileSystem.ReadAllText(const Path: WideString; Encoding: TEncoding): WideString;
begin
	Result := '';
end;

function TRealSizeFileSystem.ReadAllLines(const Path: WideString; Encoding: TEncoding): TStringList;
begin
	Result := TStringList.Create;
end;

procedure TRealSizeFileSystem.WriteAllText(const Path: WideString; const Content: WideString; Encoding: TEncoding);
begin
end;

procedure TRealSizeFileSystem.WriteAllLines(const Path: WideString; Lines: TStrings; Encoding: TEncoding);
begin
end;

function TRealSizeFileSystem.OpenTextReader(const Path: WideString; Encoding: TEncoding): TStreamReader;
var
	Stream: TStringStream;
begin
	Stream := TStringStream.Create('', Encoding);
	Result := TStreamReader.Create(Stream, Encoding, True);
end;

function TRealSizeFileSystem.GetTmpFileName(const Prefix: WideString): WideString;
begin
	Result := '';
end;

procedure TRealSizeFileSystem.SetFileTime(const Path: WideString; const FileTime: TFileTime);
begin
end;

function TRealSizeFileSystem.FindFiles(const Pattern: WideString): TStringList;
begin
	Result := TStringList.Create;
end;

function TRealSizeFileSystem.GetFileModTime(const Path: WideString): Int64;
begin
	Result := 0;
end;

end.

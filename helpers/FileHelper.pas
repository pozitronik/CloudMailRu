unit FileHelper;

{Files operations helper methods}
interface

uses
	PathHelper,
	SysUtils,
	Windows;

function IsWriteable(const DirName: WideString; FileName: WideString = 'delete.me'; CleanFile: boolean = true): boolean;
procedure SetAllFileTime(const FileName: string; const FileTime: TFileTime);
function SizeOfFile(const FileName: String): Int64;

implementation

function IsWriteable(const DirName: WideString; FileName: WideString = 'delete.me'; CleanFile: boolean = true): boolean;
var
	NewName: WideString;
	H: thandle;
begin
	NewName := IncludeTrailingPathDelimiter(DirName) + FileName;
	if CleanFile then
	begin
		H := CreateFile(PChar(NewName), GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0);
	end else begin //
		H := CreateFile(PChar(NewName), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING or CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
	end;
	Result := H <> INVALID_HANDLE_VALUE;
	if Result then
		CloseHandle(H);
end;

procedure SetAllFileTime(const FileName: string; const FileTime: TFileTime);
var
	Handle: thandle;
begin
	Handle := INVALID_HANDLE_VALUE;
	try
		Handle := CreateFileW(PWideChar(GetUNCFilePath(FileName)), FILE_WRITE_ATTRIBUTES, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
		if Handle = INVALID_HANDLE_VALUE then
		begin
			CloseHandle(Handle);
			exit;
		end;

		SetFileTime(Handle, @FileTime, @FileTime, @FileTime);
	finally
		CloseHandle(Handle);
	end;
end;

function SizeOfFile(const FileName: String): Int64;
var
	fHandle: DWORD;
begin
	fHandle := CreateFile(PChar(FileName), 0, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	if fHandle = INVALID_HANDLE_VALUE then
		Result := -1
	else
		try
			Int64Rec(Result).Lo := GetFileSize(fHandle, @Int64Rec(Result).Hi);
		finally
			CloseHandle(fHandle);
		end;
end;

end.

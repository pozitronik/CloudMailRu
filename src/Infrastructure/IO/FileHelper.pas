unit FileHelper;

{Files operations helper methods}
interface

uses
	PathHelper,
	SysUtils,
	Windows;

procedure SetAllFileTime(const FileName: string; const FileTime: TFileTime);
implementation

procedure SetAllFileTime(const FileName: string; const FileTime: TFileTime);
var
	Handle: Thandle;
begin
	Handle := INVALID_HANDLE_VALUE;
	try
		Handle := CreateFileW(PWideChar(GetUNCFilePath(FileName)), FILE_WRITE_ATTRIBUTES, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
		if Handle = INVALID_HANDLE_VALUE then
			Exit;

		SetFileTime(Handle, @FileTime, @FileTime, @FileTime);
	finally
		if Handle <> INVALID_HANDLE_VALUE then
			CloseHandle(Handle);
	end;
end;

end.

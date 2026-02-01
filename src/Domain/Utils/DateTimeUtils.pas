unit DateTimeUtils;

{Date/time conversion utilities for domain-level use}
interface

uses
	Windows;

function DateTimeToFileTime(FileTime: TDateTime): TFileTime;

implementation

uses
	SysUtils;

function DateTimeToFileTime(FileTime: TDateTime): TFileTime;
var
	LocalFileTime, Ft: TFileTime;
	SystemTime: TSystemTime;
begin
	Result.dwLowDateTime := 0;
	Result.dwHighDateTime := 0;
	DateTimeToSystemTime(FileTime, SystemTime);
	SystemTimeToFileTime(SystemTime, LocalFileTime);
	LocalFileTimeToFileTime(LocalFileTime, Ft);
	Result := Ft;
end;

end.

unit WindowsEnvironment;

{Windows implementation of IEnvironment using real system calls}

interface

uses
	IEnvironmentInterface,
	System.SysUtils,
	Windows;

type
	TWindowsEnvironment = class(TInterfacedObject, IEnvironment)
	public
		function GetEnvironmentVariable(const Name: WideString): WideString;
		function GetModulePath: WideString;
		function FileExists(const Path: WideString): Boolean;
		function DirectoryExists(const Path: WideString): Boolean;
		function IsDirectoryWriteable(const Path: WideString): Boolean;
		procedure CreateDirectory(const Path: WideString);
	end;

implementation

uses
	FileHelper,
	PathHelper;

{TWindowsEnvironment}

function TWindowsEnvironment.GetEnvironmentVariable(const Name: WideString): WideString;
begin
	Result := System.SysUtils.GetEnvironmentVariable(Name);
end;

function TWindowsEnvironment.GetModulePath: WideString;
begin
	Result := IncludeTrailingBackslash(ExtractFilePath(GetModuleName(hInstance)));
end;

function TWindowsEnvironment.FileExists(const Path: WideString): Boolean;
begin
	Result := System.SysUtils.FileExists(GetUNCFilePath(Path));
end;

function TWindowsEnvironment.DirectoryExists(const Path: WideString): Boolean;
begin
	Result := System.SysUtils.DirectoryExists(GetUNCFilePath(Path));
end;

function TWindowsEnvironment.IsDirectoryWriteable(const Path: WideString): Boolean;
begin
	Result := FileHelper.IsWriteable(Path);
end;

procedure TWindowsEnvironment.CreateDirectory(const Path: WideString);
begin
	System.SysUtils.CreateDir(GetUNCFilePath(Path));
end;

end.

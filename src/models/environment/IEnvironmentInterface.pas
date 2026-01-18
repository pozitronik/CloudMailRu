unit IEnvironmentInterface;

{Abstraction for environment and directory operations, enabling testability
 of path detection logic without real file system access.
 Used primarily by PluginSettingsManager for config file location detection.}

interface

uses
	System.SysUtils,
	System.Classes;

type
	{Interface for environment and directory operations}
	IEnvironment = interface
		['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']

		{Returns the value of an environment variable, empty string if not found}
		function GetEnvironmentVariable(const Name: WideString): WideString;

		{Returns the directory containing the current module (DLL/EXE)}
		function GetModulePath: WideString;

		{Checks if a file exists at the specified path}
		function FileExists(const Path: WideString): Boolean;

		{Checks if a directory exists at the specified path}
		function DirectoryExists(const Path: WideString): Boolean;

		{Checks if a directory is writable}
		function IsDirectoryWriteable(const Path: WideString): Boolean;

		{Creates a directory at the specified path}
		procedure CreateDirectory(const Path: WideString);
	end;

	{Null implementation for testing - simulates non-existent paths}
	TNullEnvironment = class(TInterfacedObject, IEnvironment)
	public
		function GetEnvironmentVariable(const Name: WideString): WideString;
		function GetModulePath: WideString;
		function FileExists(const Path: WideString): Boolean;
		function DirectoryExists(const Path: WideString): Boolean;
		function IsDirectoryWriteable(const Path: WideString): Boolean;
		procedure CreateDirectory(const Path: WideString);
	end;

	{In-memory environment for testing - allows configuring environment state}
	TMemoryEnvironment = class(TInterfacedObject, IEnvironment)
	private
		FEnvVars: TStringList;
		FModulePath: WideString;
		FExistingFiles: TStringList;
		FExistingDirs: TStringList;
		FWriteableDirs: TStringList;
	public
		constructor Create;
		destructor Destroy; override;

		function GetEnvironmentVariable(const Name: WideString): WideString;
		function GetModulePath: WideString;
		function FileExists(const Path: WideString): Boolean;
		function DirectoryExists(const Path: WideString): Boolean;
		function IsDirectoryWriteable(const Path: WideString): Boolean;
		procedure CreateDirectory(const Path: WideString);

		{Test helpers: configure environment state}
		procedure SetEnvironmentVariable(const Name, Value: WideString);
		procedure SetModulePath(const Path: WideString);
		procedure AddExistingFile(const Path: WideString);
		procedure AddExistingDirectory(const Path: WideString);
		procedure AddWriteableDirectory(const Path: WideString);
		procedure Clear;
	end;

implementation

{TNullEnvironment}

function TNullEnvironment.GetEnvironmentVariable(const Name: WideString): WideString;
begin
	Result := '';
end;

function TNullEnvironment.GetModulePath: WideString;
begin
	Result := '';
end;

function TNullEnvironment.FileExists(const Path: WideString): Boolean;
begin
	Result := False;
end;

function TNullEnvironment.DirectoryExists(const Path: WideString): Boolean;
begin
	Result := False;
end;

function TNullEnvironment.IsDirectoryWriteable(const Path: WideString): Boolean;
begin
	Result := False;
end;

procedure TNullEnvironment.CreateDirectory(const Path: WideString);
begin
	{No-op}
end;

{TMemoryEnvironment}

constructor TMemoryEnvironment.Create;
begin
	inherited Create;
	FEnvVars := TStringList.Create;
	FExistingFiles := TStringList.Create;
	FExistingFiles.CaseSensitive := False;
	FExistingDirs := TStringList.Create;
	FExistingDirs.CaseSensitive := False;
	FWriteableDirs := TStringList.Create;
	FWriteableDirs.CaseSensitive := False;
	FModulePath := '';
end;

destructor TMemoryEnvironment.Destroy;
begin
	FEnvVars.Free;
	FExistingFiles.Free;
	FExistingDirs.Free;
	FWriteableDirs.Free;
	inherited;
end;

function TMemoryEnvironment.GetEnvironmentVariable(const Name: WideString): WideString;
var
	Index: Integer;
begin
	Index := FEnvVars.IndexOfName(Name);
	if Index >= 0 then
		Result := FEnvVars.ValueFromIndex[Index]
	else
		Result := '';
end;

function TMemoryEnvironment.GetModulePath: WideString;
begin
	Result := FModulePath;
end;

function TMemoryEnvironment.FileExists(const Path: WideString): Boolean;
begin
	Result := FExistingFiles.IndexOf(Path) >= 0;
end;

function TMemoryEnvironment.DirectoryExists(const Path: WideString): Boolean;
begin
	Result := FExistingDirs.IndexOf(Path) >= 0;
end;

function TMemoryEnvironment.IsDirectoryWriteable(const Path: WideString): Boolean;
begin
	Result := FWriteableDirs.IndexOf(Path) >= 0;
end;

procedure TMemoryEnvironment.CreateDirectory(const Path: WideString);
begin
	if FExistingDirs.IndexOf(Path) < 0 then
		FExistingDirs.Add(Path);
end;

procedure TMemoryEnvironment.SetEnvironmentVariable(const Name, Value: WideString);
var
	Index: Integer;
begin
	Index := FEnvVars.IndexOfName(Name);
	if Index >= 0 then
		FEnvVars.ValueFromIndex[Index] := Value
	else
		FEnvVars.AddPair(Name, Value);
end;

procedure TMemoryEnvironment.SetModulePath(const Path: WideString);
begin
	FModulePath := Path;
end;

procedure TMemoryEnvironment.AddExistingFile(const Path: WideString);
begin
	if FExistingFiles.IndexOf(Path) < 0 then
		FExistingFiles.Add(Path);
end;

procedure TMemoryEnvironment.AddExistingDirectory(const Path: WideString);
begin
	if FExistingDirs.IndexOf(Path) < 0 then
		FExistingDirs.Add(Path);
end;

procedure TMemoryEnvironment.AddWriteableDirectory(const Path: WideString);
begin
	if FWriteableDirs.IndexOf(Path) < 0 then
		FWriteableDirs.Add(Path);
end;

procedure TMemoryEnvironment.Clear;
begin
	FEnvVars.Clear;
	FExistingFiles.Clear;
	FExistingDirs.Clear;
	FWriteableDirs.Clear;
	FModulePath := '';
end;

end.

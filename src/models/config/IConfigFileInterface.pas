unit IConfigFileInterface;

{Abstraction for INI-style configuration file operations, enabling testability
 without actual disk I/O. Used by AccountsManager and PluginSettingsManager.}

interface

uses
	System.SysUtils,
	System.Classes,
	System.Generics.Collections;

type
	{Interface for INI-style configuration file operations}
	IConfigFile = interface
		['{BAC4C620-1BF1-460B-8742-026C9E45E89F}']

		{Returns the file path this config was loaded from, empty for in-memory configs}
		function GetFilePath: string;

		{Read operations}
		function ReadString(const Section, Ident, Default: string): string;
		function ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
		function ReadInteger(const Section, Ident: string; Default: Integer): Integer;
		function ReadInt64(const Section, Ident: string; Default: Int64): Int64;

		{Write operations}
		procedure WriteString(const Section, Ident, Value: string);
		procedure WriteBool(const Section, Ident: string; Value: Boolean);
		procedure WriteInteger(const Section, Ident: string; Value: Integer);
		procedure WriteInt64(const Section, Ident: string; Value: Int64);

		{Write-if-not-default operations - writes value only if different from default,
		 otherwise deletes the key to keep INI files clean}
		procedure WriteStringIfNotDefault(const Section, Ident, Value, Default: string);
		procedure WriteBoolIfNotDefault(const Section, Ident: string; Value, Default: Boolean);
		procedure WriteIntegerIfNotDefault(const Section, Ident: string; Value, Default: Integer);
		procedure WriteInt64IfNotDefault(const Section, Ident: string; Value, Default: Int64);

		{Section operations}
		procedure ReadSections(Strings: TStrings);
		function SectionExists(const Section: string): Boolean;
		procedure EraseSection(const Section: string);

		{Key operations}
		procedure DeleteKey(const Section, Ident: string);
	end;

	{Null implementation for testing - returns defaults, does nothing on writes}
	TNullConfigFile = class(TInterfacedObject, IConfigFile)
	public
		function GetFilePath: string;
		function ReadString(const Section, Ident, Default: string): string;
		function ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
		function ReadInteger(const Section, Ident: string; Default: Integer): Integer;
		function ReadInt64(const Section, Ident: string; Default: Int64): Int64;

		procedure WriteString(const Section, Ident, Value: string);
		procedure WriteBool(const Section, Ident: string; Value: Boolean);
		procedure WriteInteger(const Section, Ident: string; Value: Integer);
		procedure WriteInt64(const Section, Ident: string; Value: Int64);

		procedure WriteStringIfNotDefault(const Section, Ident, Value, Default: string);
		procedure WriteBoolIfNotDefault(const Section, Ident: string; Value, Default: Boolean);
		procedure WriteIntegerIfNotDefault(const Section, Ident: string; Value, Default: Integer);
		procedure WriteInt64IfNotDefault(const Section, Ident: string; Value, Default: Int64);

		procedure ReadSections(Strings: TStrings);
		function SectionExists(const Section: string): Boolean;
		procedure EraseSection(const Section: string);

		procedure DeleteKey(const Section, Ident: string);
	end;

	{In-memory implementation for testing - stores configuration in nested dictionaries}
	TMemoryConfigFile = class(TInterfacedObject, IConfigFile)
	private
		{Section -> (Ident -> Value) mapping}
		FData: TObjectDictionary<string, TDictionary<string, string>>;
		FFilePath: string;

		function GetSection(const Section: string; CreateIfMissing: Boolean): TDictionary<string, string>;
	public
		constructor Create; overload;
		constructor Create(const FilePath: string); overload;
		destructor Destroy; override;

		function GetFilePath: string;
		function ReadString(const Section, Ident, Default: string): string;
		function ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
		function ReadInteger(const Section, Ident: string; Default: Integer): Integer;
		function ReadInt64(const Section, Ident: string; Default: Int64): Int64;

		procedure WriteString(const Section, Ident, Value: string);
		procedure WriteBool(const Section, Ident: string; Value: Boolean);
		procedure WriteInteger(const Section, Ident: string; Value: Integer);
		procedure WriteInt64(const Section, Ident: string; Value: Int64);

		procedure WriteStringIfNotDefault(const Section, Ident, Value, Default: string);
		procedure WriteBoolIfNotDefault(const Section, Ident: string; Value, Default: Boolean);
		procedure WriteIntegerIfNotDefault(const Section, Ident: string; Value, Default: Integer);
		procedure WriteInt64IfNotDefault(const Section, Ident: string; Value, Default: Int64);

		procedure ReadSections(Strings: TStrings);
		function SectionExists(const Section: string): Boolean;
		procedure EraseSection(const Section: string);

		procedure DeleteKey(const Section, Ident: string);

		{Test helper: clear all data}
		procedure Clear;
	end;

implementation

{TNullConfigFile}

function TNullConfigFile.GetFilePath: string;
begin
	Result := '';
end;

function TNullConfigFile.ReadString(const Section, Ident, Default: string): string;
begin
	Result := Default;
end;

function TNullConfigFile.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
begin
	Result := Default;
end;

function TNullConfigFile.ReadInteger(const Section, Ident: string; Default: Integer): Integer;
begin
	Result := Default;
end;

function TNullConfigFile.ReadInt64(const Section, Ident: string; Default: Int64): Int64;
begin
	Result := Default;
end;

procedure TNullConfigFile.WriteString(const Section, Ident, Value: string);
begin
	{No-op}
end;

procedure TNullConfigFile.WriteBool(const Section, Ident: string; Value: Boolean);
begin
	{No-op}
end;

procedure TNullConfigFile.WriteInteger(const Section, Ident: string; Value: Integer);
begin
	{No-op}
end;

procedure TNullConfigFile.WriteInt64(const Section, Ident: string; Value: Int64);
begin
	{No-op}
end;

procedure TNullConfigFile.WriteStringIfNotDefault(const Section, Ident, Value, Default: string);
begin
	{No-op}
end;

procedure TNullConfigFile.WriteBoolIfNotDefault(const Section, Ident: string; Value, Default: Boolean);
begin
	{No-op}
end;

procedure TNullConfigFile.WriteIntegerIfNotDefault(const Section, Ident: string; Value, Default: Integer);
begin
	{No-op}
end;

procedure TNullConfigFile.WriteInt64IfNotDefault(const Section, Ident: string; Value, Default: Int64);
begin
	{No-op}
end;

procedure TNullConfigFile.ReadSections(Strings: TStrings);
begin
	{No-op - leaves list empty}
end;

function TNullConfigFile.SectionExists(const Section: string): Boolean;
begin
	Result := False;
end;

procedure TNullConfigFile.EraseSection(const Section: string);
begin
	{No-op}
end;

procedure TNullConfigFile.DeleteKey(const Section, Ident: string);
begin
	{No-op}
end;

{TMemoryConfigFile}

constructor TMemoryConfigFile.Create;
begin
	inherited Create;
	FData := TObjectDictionary<string, TDictionary<string, string>>.Create([doOwnsValues]);
	FFilePath := '';
end;

constructor TMemoryConfigFile.Create(const FilePath: string);
begin
	Create;
	FFilePath := FilePath;
end;

function TMemoryConfigFile.GetFilePath: string;
begin
	Result := FFilePath;
end;

destructor TMemoryConfigFile.Destroy;
begin
	FData.Free;
	inherited;
end;

function TMemoryConfigFile.GetSection(const Section: string; CreateIfMissing: Boolean): TDictionary<string, string>;
begin
	if not FData.TryGetValue(Section, Result) then
	begin
		if CreateIfMissing then
		begin
			Result := TDictionary<string, string>.Create;
			FData.Add(Section, Result);
		end
		else
			Result := nil;
	end;
end;

function TMemoryConfigFile.ReadString(const Section, Ident, Default: string): string;
var
	SectionData: TDictionary<string, string>;
begin
	SectionData := GetSection(Section, False);
	if (SectionData <> nil) and SectionData.TryGetValue(Ident, Result) then
		Exit;
	Result := Default;
end;

function TMemoryConfigFile.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
var
	Value: string;
begin
	Value := ReadString(Section, Ident, '');
	if Value = '' then
		Result := Default
	else
		Result := (Value = '1') or SameText(Value, 'true');
end;

function TMemoryConfigFile.ReadInteger(const Section, Ident: string; Default: Integer): Integer;
var
	Value: string;
begin
	Value := ReadString(Section, Ident, '');
	if Value = '' then
		Result := Default
	else
		Result := StrToIntDef(Value, Default);
end;

function TMemoryConfigFile.ReadInt64(const Section, Ident: string; Default: Int64): Int64;
var
	Value: string;
begin
	Value := ReadString(Section, Ident, '');
	if Value = '' then
		Result := Default
	else
		Result := StrToInt64Def(Value, Default);
end;

procedure TMemoryConfigFile.WriteString(const Section, Ident, Value: string);
var
	SectionData: TDictionary<string, string>;
begin
	SectionData := GetSection(Section, True);
	SectionData.AddOrSetValue(Ident, Value);
end;

procedure TMemoryConfigFile.WriteBool(const Section, Ident: string; Value: Boolean);
begin
	if Value then
		WriteString(Section, Ident, '1')
	else
		WriteString(Section, Ident, '0');
end;

procedure TMemoryConfigFile.WriteInteger(const Section, Ident: string; Value: Integer);
begin
	WriteString(Section, Ident, IntToStr(Value));
end;

procedure TMemoryConfigFile.WriteInt64(const Section, Ident: string; Value: Int64);
begin
	WriteString(Section, Ident, IntToStr(Value));
end;

procedure TMemoryConfigFile.WriteStringIfNotDefault(const Section, Ident, Value, Default: string);
begin
	if Value <> Default then
		WriteString(Section, Ident, Value)
	else
		DeleteKey(Section, Ident);
end;

procedure TMemoryConfigFile.WriteBoolIfNotDefault(const Section, Ident: string; Value, Default: Boolean);
begin
	if Value <> Default then
		WriteBool(Section, Ident, Value)
	else
		DeleteKey(Section, Ident);
end;

procedure TMemoryConfigFile.WriteIntegerIfNotDefault(const Section, Ident: string; Value, Default: Integer);
begin
	if Value <> Default then
		WriteInteger(Section, Ident, Value)
	else
		DeleteKey(Section, Ident);
end;

procedure TMemoryConfigFile.WriteInt64IfNotDefault(const Section, Ident: string; Value, Default: Int64);
begin
	if Value <> Default then
		WriteInt64(Section, Ident, Value)
	else
		DeleteKey(Section, Ident);
end;

procedure TMemoryConfigFile.ReadSections(Strings: TStrings);
var
	Section: string;
begin
	Strings.Clear;
	for Section in FData.Keys do
		Strings.Add(Section);
end;

function TMemoryConfigFile.SectionExists(const Section: string): Boolean;
begin
	Result := FData.ContainsKey(Section);
end;

procedure TMemoryConfigFile.EraseSection(const Section: string);
begin
	FData.Remove(Section);
end;

procedure TMemoryConfigFile.DeleteKey(const Section, Ident: string);
var
	SectionData: TDictionary<string, string>;
begin
	SectionData := GetSection(Section, False);
	if SectionData <> nil then
		SectionData.Remove(Ident);
end;

procedure TMemoryConfigFile.Clear;
begin
	FData.Clear;
end;

end.

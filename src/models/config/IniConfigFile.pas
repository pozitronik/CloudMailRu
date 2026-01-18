unit IniConfigFile;

{TIniFile wrapper implementing IConfigFile interface for production use.
 Each operation opens and closes the file to match original behavior pattern.}

interface

uses
	IConfigFileInterface,
	IniFiles,
	IniFilesHelper,
	System.SysUtils,
	System.Classes;

type
	TIniConfigFile = class(TInterfacedObject, IConfigFile)
	private
		FFilePath: string;
	public
		constructor Create(const FilePath: string);

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

		property FilePath: string read FFilePath;
	end;

implementation

{TIniConfigFile}

constructor TIniConfigFile.Create(const FilePath: string);
begin
	inherited Create;
	FFilePath := FilePath;
end;

function TIniConfigFile.GetFilePath: string;
begin
	Result := FFilePath;
end;

function TIniConfigFile.ReadString(const Section, Ident, Default: string): string;
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FFilePath);
	try
		Result := IniFile.ReadString(Section, Ident, Default);
	finally
		IniFile.Free;
	end;
end;

function TIniConfigFile.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FFilePath);
	try
		Result := IniFile.ReadBool(Section, Ident, Default);
	finally
		IniFile.Free;
	end;
end;

function TIniConfigFile.ReadInteger(const Section, Ident: string; Default: Integer): Integer;
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FFilePath);
	try
		Result := IniFile.ReadInteger(Section, Ident, Default);
	finally
		IniFile.Free;
	end;
end;

function TIniConfigFile.ReadInt64(const Section, Ident: string; Default: Int64): Int64;
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FFilePath);
	try
		Result := IniFile.ReadInt64(Section, Ident, Default);
	finally
		IniFile.Free;
	end;
end;

procedure TIniConfigFile.WriteString(const Section, Ident, Value: string);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FFilePath);
	try
		IniFile.WriteString(Section, Ident, Value);
	finally
		IniFile.Free;
	end;
end;

procedure TIniConfigFile.WriteBool(const Section, Ident: string; Value: Boolean);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FFilePath);
	try
		IniFile.WriteBool(Section, Ident, Value);
	finally
		IniFile.Free;
	end;
end;

procedure TIniConfigFile.WriteInteger(const Section, Ident: string; Value: Integer);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FFilePath);
	try
		IniFile.WriteInteger(Section, Ident, Value);
	finally
		IniFile.Free;
	end;
end;

procedure TIniConfigFile.WriteInt64(const Section, Ident: string; Value: Int64);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FFilePath);
	try
		IniFile.WriteInt64(Section, Ident, Value);
	finally
		IniFile.Free;
	end;
end;

procedure TIniConfigFile.WriteStringIfNotDefault(const Section, Ident, Value, Default: string);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FFilePath);
	try
		IniFile.WriteStringIfNotDefault(Section, Ident, Value, Default);
	finally
		IniFile.Free;
	end;
end;

procedure TIniConfigFile.WriteBoolIfNotDefault(const Section, Ident: string; Value, Default: Boolean);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FFilePath);
	try
		IniFile.WriteBoolIfNotDefault(Section, Ident, Value, Default);
	finally
		IniFile.Free;
	end;
end;

procedure TIniConfigFile.WriteIntegerIfNotDefault(const Section, Ident: string; Value, Default: Integer);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FFilePath);
	try
		IniFile.WriteIntegerIfNotDefault(Section, Ident, Value, Default);
	finally
		IniFile.Free;
	end;
end;

procedure TIniConfigFile.WriteInt64IfNotDefault(const Section, Ident: string; Value, Default: Int64);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FFilePath);
	try
		IniFile.WriteInt64IfNotDefault(Section, Ident, Value, Default);
	finally
		IniFile.Free;
	end;
end;

procedure TIniConfigFile.ReadSections(Strings: TStrings);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FFilePath);
	try
		IniFile.ReadSections(Strings);
	finally
		IniFile.Free;
	end;
end;

function TIniConfigFile.SectionExists(const Section: string): Boolean;
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FFilePath);
	try
		Result := IniFile.SectionExists(Section);
	finally
		IniFile.Free;
	end;
end;

procedure TIniConfigFile.EraseSection(const Section: string);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FFilePath);
	try
		IniFile.EraseSection(Section);
	finally
		IniFile.Free;
	end;
end;

procedure TIniConfigFile.DeleteKey(const Section, Ident: string);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FFilePath);
	try
		IniFile.DeleteKey(Section, Ident);
	finally
		IniFile.Free;
	end;
end;

end.

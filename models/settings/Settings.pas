unit Settings;

interface

uses
	IniFiles,
	Classes,
	SysUtils,
	SETTINGS_CONSTANTS;

procedure GetStreamingExtensionsFromIniFile(IniFilePath: WideString; var StreamingExtensions: TStringList);
procedure DeleteStreamingExtensionsFromIniFile(IniFilePath: WideString; StreamingExtension: WideString);

implementation

//loads all streaming extensions list
procedure GetStreamingExtensionsFromIniFile(IniFilePath: WideString; var StreamingExtensions: TStringList);
var
	IniFile: TIniFile;
	TempList: TStrings;
	line: String;
begin
	IniFile := TIniFile.Create(IniFilePath);
	TempList := TStringList.Create;
	IniFile.ReadSections(TempList);
	for line in TempList do
	begin
		if line.StartsWith(StreamingPrefix) then
			StreamingExtensions.Add(line.Substring(Length(StreamingPrefix)));
	end;
	TempList.Destroy;
	IniFile.Destroy;
end;

procedure DeleteStreamingExtensionsFromIniFile(IniFilePath: WideString; StreamingExtension: WideString);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	IniFile.EraseSection(StreamingPrefix + StreamingExtension);
	IniFile.Destroy;
end;

end.

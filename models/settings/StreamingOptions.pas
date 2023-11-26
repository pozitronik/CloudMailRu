unit StreamingOptions;

interface

uses
	IniFiles,
	SETTINGS_CONSTANTS,
	SysUtils,
	PathHelper;

type

	{Параметры стриминга для расширения}
	TStreamingOptions = record
		Command: WideString; //Вызываемое приложение
		Parameters: WideString; //параметры, передаваемые приложению
		StartPath: WideString; //каталог запуска
		Format: integer;
	end;

function GetStreamingOptionsFromIniFile(IniFilePath, FileName: WideString; var StreamingOptions: TStreamingOptions): boolean;
function SetStreamingOptionsToIniFile(IniFilePath, FileName: WideString; StreamingOptions: TStreamingOptions): boolean;

implementation

function GetStreamingOptionsFromIniFile(IniFilePath, FileName: WideString; var StreamingOptions: TStreamingOptions): boolean;
var
	IniFile: TIniFile;
	SectionName: WideString;
begin
	result := false;
	StreamingOptions := default (TStreamingOptions);
	IniFile := TIniFile.Create(IniFilePath);
	SectionName := StreamingPrefix + ExtractUniversalFileExt(FileName, true);
	if IniFile.SectionExists(SectionName) then
	begin
		result := true;
		StreamingOptions.Command := IniFile.ReadString(SectionName, 'Command', EmptyWideStr);
		StreamingOptions.Parameters := IniFile.ReadString(SectionName, 'Parameters', EmptyWideStr);
		StreamingOptions.StartPath := IniFile.ReadString(SectionName, 'StartPath', EmptyWideStr);
		StreamingOptions.Format := IniFile.ReadInteger(SectionName, 'Format', 0);
	end;
	IniFile.Destroy;
end;

function SetStreamingOptionsToIniFile(IniFilePath, FileName: WideString; StreamingOptions: TStreamingOptions): boolean;
var
	IniFile: TIniFile;
	SectionName: WideString;
begin
	result := false;
	if ExtractUniversalFileExt(FileName, true) <> EmptyWideStr then
	begin
		result := true;
		SectionName := StreamingPrefix + ExtractUniversalFileExt(FileName, true);
		IniFile := TIniFile.Create(IniFilePath);
		IniFile.WriteString(SectionName, 'Command', StreamingOptions.Command);
		IniFile.WriteString(SectionName, 'Parameters', StreamingOptions.Parameters);
		IniFile.WriteString(SectionName, 'StartPath', StreamingOptions.StartPath);
		IniFile.WriteInteger(SectionName, 'Format', StreamingOptions.Format);
		IniFile.Destroy;
	end;
end;

end.

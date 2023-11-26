unit PluginSettings;

interface

uses
	IniFiles,
	SysUtils,
	SETTINGS_CONSTANTS,
	TCLogger,
	Variants,
	WindowsHelper,
	Windows,
	IOUtils,
	CMRStrings,
	ConnectionSettings;

type
	{Global plugin options}
	TPluginSettings = record
		ConnectionSettings: TConnectionSettings;
		IniPath: integer;
		LoadSSLDLLOnlyFromPluginDir: boolean;
		PreserveFileTime: boolean;
		DescriptionEnabled: boolean;
		DescriptionEditorEnabled: boolean;
		DescriptionCopyToCloud: boolean;
		DescriptionCopyFromCloud: boolean;
		DescriptionTrackCloudFS: boolean;
		DescriptionFileName: WideString;
		CopyBetweenAccountsMode: integer;
		CloudMaxFileSize: int64;
		ChunkOverwriteMode: integer;
		DeleteFailOnUploadMode: integer;
		OperationErrorMode: integer;
		RetryAttempts: integer;
		AttemptWait: integer;
		OverwriteLocalMode: integer;
		DisableMultiThreading: boolean;
		LogUserSpace: boolean;
		IconsMode: integer;
		DownloadLinksEncode: boolean;
		AutoUpdateDownloadListing: boolean;
		ShowTrashFolders: boolean;
		ShowSharedFolders: boolean;
		ShowInvitesFolders: boolean;
		LogLevel: integer;
		PrecalculateHash: boolean;
		ForcePrecalculateSize: int64;
		CheckCRC: boolean;
	end;

function GetPluginSettings(IniFilePath: WideString): TPluginSettings;
procedure SetPluginSettingsValue(IniFilePath: WideString; OptionName: WideString; OptionValue: Variant);
function GetDescriptionFileName(SettingsIniFilePath: WideString): WideString;

implementation

function GetPluginSettings(IniFilePath: WideString): TPluginSettings;
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	GetPluginSettings.IniPath := IniFile.ReadInteger('Main', 'IniPath', 0);
	GetPluginSettings.LoadSSLDLLOnlyFromPluginDir := IniFile.ReadBool('Main', 'LoadSSLDLLOnlyFromPluginDir', false);
	GetPluginSettings.PreserveFileTime := IniFile.ReadBool('Main', 'PreserveFileTime', false);
	GetPluginSettings.DescriptionEnabled := IniFile.ReadBool('Main', 'DescriptionEnabled', false);
	GetPluginSettings.DescriptionEditorEnabled := IniFile.ReadBool('Main', 'DescriptionEditorEnabled', false);
	GetPluginSettings.DescriptionCopyToCloud := IniFile.ReadBool('Main', 'DescriptionCopyToCloud', false);
	GetPluginSettings.DescriptionCopyFromCloud := IniFile.ReadBool('Main', 'DescriptionCopyFromCloud', false);
	GetPluginSettings.DescriptionTrackCloudFS := IniFile.ReadBool('Main', 'DescriptionTrackCloudFS', false);
	GetPluginSettings.DescriptionFileName := IniFile.ReadString('Main', 'DescriptionFileName', 'descript.ion');
	GetPluginSettings.CopyBetweenAccountsMode := IniFile.ReadInteger('Main', 'CopyBetweenAccountsMode', CopyBetweenAccountsModeDisabled);

	GetPluginSettings.DisableMultiThreading := IniFile.ReadBool('Main', 'DisableMultiThreading', false);
	GetPluginSettings.LogUserSpace := IniFile.ReadBool('Main', 'LogUserSpace', true);
	GetPluginSettings.IconsMode := IniFile.ReadInteger('Main', 'IconsMode', 0);
	GetPluginSettings.ConnectionSettings.SocketTimeout := IniFile.ReadInteger('Main', 'SocketTimeout', -1);
	GetPluginSettings.ConnectionSettings.UploadBPS := IniFile.ReadInteger('Main', 'UploadBPS', -1);
	GetPluginSettings.ConnectionSettings.DownloadBPS := IniFile.ReadInteger('Main', 'DownloadBPS', -1);
	GetPluginSettings.CloudMaxFileSize := IniFile.ReadInt64('Main', 'CloudMaxFileSize', CLOUD_MAX_FILESIZE_DEFAULT);
	GetPluginSettings.ChunkOverwriteMode := IniFile.ReadInteger('Main', 'ChunkOverwriteMode', 0);
	GetPluginSettings.DeleteFailOnUploadMode := IniFile.ReadInteger('Main', 'DeleteFailOnUploadMode', 0);
	GetPluginSettings.OverwriteLocalMode := IniFile.ReadInteger('Main', 'OverwriteLocalMode', 0);
	GetPluginSettings.OperationErrorMode := IniFile.ReadInteger('Main', 'OperationErrorMode', 0);
	GetPluginSettings.RetryAttempts := IniFile.ReadInteger('Main', 'RetryAttempts', 1);
	GetPluginSettings.AttemptWait := IniFile.ReadInteger('Main', 'AttemptWait', 1000);
	GetPluginSettings.ConnectionSettings.ProxySettings.ProxyType := IniFile.ReadInteger('Main', 'ProxyType', ProxyNone);
	GetPluginSettings.ConnectionSettings.ProxySettings.Server := IniFile.ReadString('Main', 'ProxyServer', EmptyWideStr);
	GetPluginSettings.ConnectionSettings.ProxySettings.Port := IniFile.ReadInteger('Main', 'ProxyPort', 0);
	GetPluginSettings.ConnectionSettings.ProxySettings.user := IniFile.ReadString('Main', 'ProxyUser', EmptyWideStr);
	GetPluginSettings.ConnectionSettings.ProxySettings.use_tc_password_manager := IniFile.ReadBool('Main', 'ProxyTCPwdMngr', false);
	GetPluginSettings.ConnectionSettings.ProxySettings.password := IniFile.ReadString('Main', 'ProxyPassword', EmptyWideStr);
	GetPluginSettings.ConnectionSettings.UserAgent := IniFile.ReadString('Main', 'UserAgent', DEFAULT_USERAGENT);
	GetPluginSettings.DownloadLinksEncode := IniFile.ReadBool('Main', 'DownloadLinksEncode', true);
	GetPluginSettings.AutoUpdateDownloadListing := IniFile.ReadBool('Main', 'AutoUpdateDownloadListing', true);
	GetPluginSettings.ShowTrashFolders := IniFile.ReadBool('Main', 'ShowTrashFolders', true);
	GetPluginSettings.ShowSharedFolders := IniFile.ReadBool('Main', 'ShowSharedFolders', true);
	GetPluginSettings.ShowInvitesFolders := IniFile.ReadBool('Main', 'ShowInvitesFolders', true);
	GetPluginSettings.LogLevel := IniFile.ReadInteger('Main', 'LogLevel', LOG_LEVEL_CONNECT + LOG_LEVEL_FILE_OPERATION + LOG_LEVEL_DETAIL + LOG_LEVEL_WARNING + LOG_LEVEL_ERROR);
	GetPluginSettings.PrecalculateHash := IniFile.ReadBool('Main', 'PrecalculateHash', true);
	GetPluginSettings.ForcePrecalculateSize := IniFile.ReadInt64('Main', 'ForcePrecalculateSize', CLOUD_PRECALCULATE_LIMIT_DEFAULT);
	GetPluginSettings.CheckCRC := IniFile.ReadBool('Main', 'CheckCRC', true);
	IniFile.Destroy;
end;

procedure SetPluginSettingsValue(IniFilePath: WideString; OptionName: WideString; OptionValue: Variant);
var
	IniFile: TIniFile;
	basicType: integer;
begin
	IniFile := TIniFile.Create(IniFilePath);

	basicType := VarType(OptionValue);
	try
		case basicType of
			varNull:
				IniFile.DeleteKey('Main', OptionName); //remove value in that case
			varInteger:
				IniFile.WriteInteger('Main', OptionName, OptionValue);
			varString, varUString, varOleStr:
				IniFile.WriteString('Main', OptionName, OptionValue);
			varBoolean:
				IniFile.WriteBool('Main', OptionName, OptionValue);
		end;
	except
		On E: EIniFileException do
		begin
			MsgBox(0, E.Message, ERR_INI_GENERAL, MB_ICONERROR + MB_OK);
			IniFile.Destroy;
			exit;
		end;
	end;
	IniFile.Destroy;
end;

function GetDescriptionFileName(SettingsIniFilePath: WideString): WideString;
begin
	GetDescriptionFileName := GetPluginSettings(SettingsIniFilePath).DescriptionFileName;
	if TPath.HasValidFileNameChars(GetPluginSettings(SettingsIniFilePath).DescriptionFileName, false) then
		exit;
	exit('descript.ion');
end;

end.

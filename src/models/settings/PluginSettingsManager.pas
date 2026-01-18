unit PluginSettingsManager;

interface

uses
	SysUtils,
	FileHelper,
	PathHelper,
	WindowsHelper,
	Windows,
	SETTINGS_CONSTANTS,
	LANGUAGE_STRINGS,
	CMRConstants,
	Classes,
	PluginSettings,
	StreamingSettings,
	IPluginSettingsManagerInterface,
	IConfigFileInterface,
	IEnvironmentInterface;

type

	{Files names and path naming conventions:
	 (File)Path or (Dir)Patn should contain the full absolute path for a file or a directory
	 (File)Name or (Dir)Name should contain only the name of a file or a directory
	}

	TPluginSettingsManager = class(TInterfacedObject, IPluginSettingsManager)
	private
		FApplicationPath: WideString; {the directory of the current executable file}
		FIniFilePath: WideString; {the absolute path of the current configuration file}
		FIniFileDir: WideString; {the directory where the currently used ini files (global+accounts) are}
		FConfigFile: IConfigFile;
		FEnvironment: IEnvironment;

		function GetAccountsIniFilePath: WideString;

	public
		Settings: TPluginSettings;
		property ApplicationPath: WideString read FApplicationPath; {Required for tests}
		property IniFileDir: WideString read FIniFileDir; {Required for tests}
		property IniFilePath: WideString read FIniFilePath;
		property AccountsIniFilePath: WideString read GetAccountsIniFilePath; {The path to the accounts config file}

		constructor Create(); overload; {finds the settings file by itself}
		constructor Create(ConfigFile: IConfigFile; IniFilePath: WideString); overload;
		constructor Create(ConfigFile: IConfigFile; Environment: IEnvironment; IniFilePath: WideString); overload; {Full DI constructor for testing}
		procedure Refresh();

		procedure Save(); {save current options set into the file}
		procedure SwitchProxyPasswordStorage;

		{IPluginSettingsManager interface method}
		function GetSettings: TPluginSettings;

		function GetStreamingSettings(const FileName: WideString): TStreamingSettings;
		procedure SetStreamingSettings(const FileName: WideString; StreamingSettings: TStreamingSettings);
		procedure GetStreamingExtensionsList(ExtensionsList: TStrings);
		procedure RemoveStreamingExtension(const Extension: WideString);

	end;

implementation

uses
	IniConfigFile,
	WindowsEnvironment;

{TPluginSettingsManager}

constructor TPluginSettingsManager.Create(ConfigFile: IConfigFile; IniFilePath: WideString);
begin
	FConfigFile := ConfigFile;
	FEnvironment := nil; {Not needed when config file is provided directly}
	FIniFilePath := IniFilePath;
	Refresh();
end;

constructor TPluginSettingsManager.Create(ConfigFile: IConfigFile; Environment: IEnvironment; IniFilePath: WideString);
begin
	FConfigFile := ConfigFile;
	FEnvironment := Environment;
	FIniFilePath := IniFilePath;
	Refresh();
end;

constructor TPluginSettingsManager.Create;
var
	AppDataDir: WideString;
	TempConfigFile: IConfigFile;
	TempIniDir: Integer;
begin
	FEnvironment := TWindowsEnvironment.Create;
	AppDataDir := IncludeTrailingBackslash(IncludeTrailingBackslash(FEnvironment.GetEnvironmentVariable('APPDATA')) + APPDATA_DIR_NAME);
	FApplicationPath := FEnvironment.GetModulePath;

	if FEnvironment.FileExists(FApplicationPath + PLUGIN_CONFIG_FILE_NAME) then
	begin
		{Read IniDir setting from plugin directory config to determine actual config location}
		TempConfigFile := TIniConfigFile.Create(GetUNCFilePath(FApplicationPath + PLUGIN_CONFIG_FILE_NAME));
		try
			TempIniDir := TempConfigFile.ReadInteger('Main', 'IniPath', INI_DIR_PLUGIN);
			case TempIniDir of
				INI_DIR_PLUGIN:
					begin
						FIniFileDir := FApplicationPath;
					end;
				INI_DIR_APPDATA:
					begin
						FIniFileDir := AppDataDir;
					end;
				INI_DIR_AUTO:
					begin
						if FEnvironment.IsDirectoryWriteable(FApplicationPath) then
							FIniFileDir := FApplicationPath
						else
							FIniFileDir := AppDataDir;
					end;
			end;
		finally
			TempConfigFile := nil;
		end;

	end else begin
		if FEnvironment.IsDirectoryWriteable(FApplicationPath) then
		begin
			FIniFileDir := FApplicationPath;
		end else begin
			FIniFileDir := AppDataDir;
		end;
	end;

	if not FEnvironment.DirectoryExists(FIniFileDir) then
		FEnvironment.CreateDirectory(FIniFileDir);

	FIniFilePath := GetUNCFilePath(FIniFileDir + PLUGIN_CONFIG_FILE_NAME);
	FConfigFile := TIniConfigFile.Create(FIniFilePath);
	Refresh();
end;

function TPluginSettingsManager.GetAccountsIniFilePath: WideString;
begin
	result := self.FIniFileDir + ACCOUNTS_CONFIG_FILE_NAME;
end;

procedure TPluginSettingsManager.Refresh;
begin
	with Settings do
	begin
		try
			IniDir := FConfigFile.ReadInteger('Main', 'IniPath', INI_DIR_PLUGIN);
		except
			on E: ERangeError do
			begin
				IniDir := INI_DIR_PLUGIN;
			end;
		end;
		LoadSSLDLLOnlyFromPluginDir := FConfigFile.ReadBool('Main', 'LoadSSLDLLOnlyFromPluginDir', False);
		PreserveFileTime := FConfigFile.ReadBool('Main', 'PreserveFileTime', False);
		DescriptionEnabled := FConfigFile.ReadBool('Main', 'DescriptionEnabled', False);
		DescriptionEditorEnabled := FConfigFile.ReadBool('Main', 'DescriptionEditorEnabled', False);
		DescriptionCopyToCloud := FConfigFile.ReadBool('Main', 'DescriptionCopyToCloud', False);
		DescriptionCopyFromCloud := FConfigFile.ReadBool('Main', 'DescriptionCopyFromCloud', False);
		DescriptionTrackCloudFS := FConfigFile.ReadBool('Main', 'DescriptionTrackCloudFS', False);
		DescriptionFileName := FConfigFile.ReadString('Main', 'DescriptionFileName', 'descript.ion');
		CopyBetweenAccountsMode := FConfigFile.ReadInteger('Main', 'CopyBetweenAccountsMode', CopyBetweenAccountsModeDisabled);
		DisableMultiThreading := FConfigFile.ReadBool('Main', 'DisableMultiThreading', False);
		LogUserSpace := FConfigFile.ReadBool('Main', 'LogUserSpace', True);
		IconsMode := FConfigFile.ReadInteger('Main', 'IconsMode', IconsModeDisabled);
		ConnectionSettings.SocketTimeout := FConfigFile.ReadInteger('Main', 'SocketTimeout', -1);
		ConnectionSettings.UploadBPS := FConfigFile.ReadInteger('Main', 'UploadBPS', -1);
		ConnectionSettings.DownloadBPS := FConfigFile.ReadInteger('Main', 'DownloadBPS', -1);
		CloudMaxFileSize := FConfigFile.ReadInt64('Main', 'CloudMaxFileSize', CLOUD_MAX_FILESIZE_DEFAULT);
		ChunkOverwriteMode := FConfigFile.ReadInteger('Main', 'ChunkOverwriteMode', ChunkOverwrite);
		DeleteFailOnUploadMode := FConfigFile.ReadInteger('Main', 'DeleteFailOnUploadMode', DeleteFailOnUploadAsk);
		OverwriteLocalMode := FConfigFile.ReadInteger('Main', 'OverwriteLocalMode', OverwriteLocalModeAsk);
		OperationErrorMode := FConfigFile.ReadInteger('Main', 'OperationErrorMode', OperationErrorModeAsk);
		RetryAttempts := FConfigFile.ReadInteger('Main', 'RetryAttempts', 1);
		AttemptWait := FConfigFile.ReadInteger('Main', 'AttemptWait', 1000);
		ConnectionSettings.ProxySettings.ProxyType := FConfigFile.ReadInteger('Main', 'ProxyType', ProxyNone);
		ConnectionSettings.ProxySettings.Server := FConfigFile.ReadString('Main', 'ProxyServer', EmptyWideStr);
		ConnectionSettings.ProxySettings.Port := FConfigFile.ReadInteger('Main', 'ProxyPort', 0);
		ConnectionSettings.ProxySettings.User := FConfigFile.ReadString('Main', 'ProxyUser', EmptyWideStr);
		ConnectionSettings.ProxySettings.UseTCPasswordManager := FConfigFile.ReadBool('Main', 'ProxyTCPwdMngr', False);
		ConnectionSettings.ProxySettings.Password := FConfigFile.ReadString('Main', 'ProxyPassword', EmptyWideStr);
		ConnectionSettings.UserAgent := FConfigFile.ReadString('Main', 'UserAgent', DEFAULT_USERAGENT);
		DownloadLinksEncode := FConfigFile.ReadBool('Main', 'DownloadLinksEncode', True);
		AutoUpdateDownloadListing := FConfigFile.ReadBool('Main', 'AutoUpdateDownloadListing', True);
		ShowTrashFolders := FConfigFile.ReadBool('Main', 'ShowTrashFolders', True);
		ShowSharedFolders := FConfigFile.ReadBool('Main', 'ShowSharedFolders', True);
		ShowInvitesFolders := FConfigFile.ReadBool('Main', 'ShowInvitesFolders', True);
		LogLevel := FConfigFile.ReadInteger('Main', 'LogLevel', LOG_LEVEL_CONNECT + LOG_LEVEL_FILE_OPERATION + LOG_LEVEL_DETAIL + LOG_LEVEL_WARNING + LOG_LEVEL_ERROR);
		PrecalculateHash := FConfigFile.ReadBool('Main', 'PrecalculateHash', True);
		ForcePrecalculateSize := FConfigFile.ReadInt64('Main', 'ForcePrecalculateSize', CLOUD_PRECALCULATE_LIMIT_DEFAULT);
		CheckCRC := FConfigFile.ReadBool('Main', 'CheckCRC', True);
	end;
	Settings.IniFilePath := FIniFilePath;
	Settings.AccountsIniFilePath := AccountsIniFilePath;
end;

procedure TPluginSettingsManager.Save;
begin
	with Settings do
	begin
		FConfigFile.WriteIntegerIfNotDefault('Main', 'IniPath', IniDir, INI_DIR_PLUGIN);
		FConfigFile.WriteBoolIfNotDefault('Main', 'LoadSSLDLLOnlyFromPluginDir', LoadSSLDLLOnlyFromPluginDir, False);
		FConfigFile.WriteBoolIfNotDefault('Main', 'PreserveFileTime', PreserveFileTime, False);
		FConfigFile.WriteBoolIfNotDefault('Main', 'DescriptionEnabled', DescriptionEnabled, False);
		FConfigFile.WriteBoolIfNotDefault('Main', 'DescriptionEditorEnabled', DescriptionEditorEnabled, False);
		FConfigFile.WriteBoolIfNotDefault('Main', 'DescriptionCopyToCloud', DescriptionCopyToCloud, False);
		FConfigFile.WriteBoolIfNotDefault('Main', 'DescriptionCopyFromCloud', DescriptionCopyFromCloud, False);
		FConfigFile.WriteBoolIfNotDefault('Main', 'DescriptionTrackCloudFS', DescriptionTrackCloudFS, False);
		FConfigFile.WriteStringIfNotDefault('Main', 'DescriptionFileName', DescriptionFileName, 'descript.ion');
		FConfigFile.WriteIntegerIfNotDefault('Main', 'CopyBetweenAccountsMode', CopyBetweenAccountsMode, CopyBetweenAccountsModeDisabled);
		FConfigFile.WriteBoolIfNotDefault('Main', 'DisableMultiThreading', DisableMultiThreading, False);
		FConfigFile.WriteBoolIfNotDefault('Main', 'LogUserSpace', LogUserSpace, True);
		FConfigFile.WriteIntegerIfNotDefault('Main', 'IconsMode', IconsMode, IconsModeDisabled);
		FConfigFile.WriteIntegerIfNotDefault('Main', 'SocketTimeout', ConnectionSettings.SocketTimeout, -1);
		FConfigFile.WriteIntegerIfNotDefault('Main', 'UploadBPS', ConnectionSettings.UploadBPS, -1);
		FConfigFile.WriteIntegerIfNotDefault('Main', 'DownloadBPS', ConnectionSettings.DownloadBPS, -1);
		FConfigFile.WriteInt64IfNotDefault('Main', 'CloudMaxFileSize', CloudMaxFileSize, CLOUD_MAX_FILESIZE_DEFAULT);
		FConfigFile.WriteIntegerIfNotDefault('Main', 'ChunkOverwriteMode', ChunkOverwriteMode, ChunkOverwrite);
		FConfigFile.WriteIntegerIfNotDefault('Main', 'DeleteFailOnUploadMode', DeleteFailOnUploadMode, DeleteFailOnUploadAsk);
		FConfigFile.WriteIntegerIfNotDefault('Main', 'OverwriteLocalMode', OverwriteLocalMode, OverwriteLocalModeAsk);
		FConfigFile.WriteIntegerIfNotDefault('Main', 'OperationErrorMode', OperationErrorMode, OperationErrorModeAsk);
		FConfigFile.WriteIntegerIfNotDefault('Main', 'RetryAttempts', RetryAttempts, 1);
		FConfigFile.WriteIntegerIfNotDefault('Main', 'AttemptWait', AttemptWait, 1000);
		FConfigFile.WriteIntegerIfNotDefault('Main', 'ProxyType', ConnectionSettings.ProxySettings.ProxyType, ProxyNone);
		FConfigFile.WriteStringIfNotDefault('Main', 'ProxyServer', ConnectionSettings.ProxySettings.Server, EmptyWideStr);
		FConfigFile.WriteIntegerIfNotDefault('Main', 'ProxyPort', ConnectionSettings.ProxySettings.Port, 0);
		FConfigFile.WriteStringIfNotDefault('Main', 'ProxyUser', ConnectionSettings.ProxySettings.User, EmptyWideStr);
		FConfigFile.WriteBoolIfNotDefault('Main', 'ProxyTCPwdMngr', ConnectionSettings.ProxySettings.UseTCPasswordManager, False);
		FConfigFile.WriteStringIfNotDefault('Main', 'ProxyPassword', ConnectionSettings.ProxySettings.Password, EmptyWideStr);
		FConfigFile.WriteStringIfNotDefault('Main', 'UserAgent', ConnectionSettings.UserAgent, DEFAULT_USERAGENT);
		FConfigFile.WriteBoolIfNotDefault('Main', 'DownloadLinksEncode', DownloadLinksEncode, True);
		FConfigFile.WriteBoolIfNotDefault('Main', 'AutoUpdateDownloadListing', AutoUpdateDownloadListing, True);
		FConfigFile.WriteBoolIfNotDefault('Main', 'ShowTrashFolders', ShowTrashFolders, True);
		FConfigFile.WriteBoolIfNotDefault('Main', 'ShowSharedFolders', ShowSharedFolders, True);
		FConfigFile.WriteBoolIfNotDefault('Main', 'ShowInvitesFolders', ShowInvitesFolders, True);
		FConfigFile.WriteIntegerIfNotDefault('Main', 'LogLevel', LogLevel, LOG_LEVEL_CONNECT + LOG_LEVEL_FILE_OPERATION + LOG_LEVEL_DETAIL + LOG_LEVEL_WARNING + LOG_LEVEL_ERROR);
		FConfigFile.WriteBoolIfNotDefault('Main', 'PrecalculateHash', PrecalculateHash, True);
		FConfigFile.WriteInt64IfNotDefault('Main', 'ForcePrecalculateSize', ForcePrecalculateSize, CLOUD_PRECALCULATE_LIMIT_DEFAULT);
		FConfigFile.WriteBoolIfNotDefault('Main', 'CheckCRC', CheckCRC, True);
	end;
end;

procedure TPluginSettingsManager.GetStreamingExtensionsList(ExtensionsList: TStrings);
var
	TempList: TStrings;
	Line: String;
begin
	ExtensionsList.Clear;
	TempList := TStringList.Create;
	try
		FConfigFile.ReadSections(TempList);
		for Line in TempList do
		begin
			if Line.StartsWith(StreamingPrefix) then
				ExtensionsList.Add(Line.Substring(Length(StreamingPrefix)));
		end;
	finally
		TempList.Free;
	end;
end;

procedure TPluginSettingsManager.RemoveStreamingExtension(const Extension: WideString);
begin
	FConfigFile.EraseSection(StreamingPrefix + Extension);
end;

function TPluginSettingsManager.GetStreamingSettings(const FileName: WideString): TStreamingSettings;
var
	SectionName: WideString;
begin
	Result := default (TStreamingSettings);
	Result.Format := STREAMING_FORMAT_UNSET;
	SectionName := StreamingPrefix + ExtractUniversalFileExt(FileName, True);
	if FConfigFile.SectionExists(SectionName) then
	begin
		Result.Command := FConfigFile.ReadString(SectionName, 'Command', EmptyWideStr);
		Result.Parameters := FConfigFile.ReadString(SectionName, 'Parameters', EmptyWideStr);
		Result.StartPath := FConfigFile.ReadString(SectionName, 'StartPath', EmptyWideStr);
		Result.Format := FConfigFile.ReadInteger(SectionName, 'Format', STREAMING_FORMAT_NONE);
	end;
end;

procedure TPluginSettingsManager.SetStreamingSettings(const FileName: WideString; StreamingSettings: TStreamingSettings);
var
	SectionName: WideString;
begin
	if ExtractUniversalFileExt(FileName, True) <> EmptyWideStr then
	begin
		SectionName := StreamingPrefix + ExtractUniversalFileExt(FileName, True);
		FConfigFile.WriteString(SectionName, 'Command', StreamingSettings.Command);
		FConfigFile.WriteString(SectionName, 'Parameters', StreamingSettings.Parameters);
		FConfigFile.WriteString(SectionName, 'StartPath', StreamingSettings.StartPath);
		FConfigFile.WriteInteger(SectionName, 'Format', StreamingSettings.Format);
	end;
end;

procedure TPluginSettingsManager.SwitchProxyPasswordStorage;
begin
	FConfigFile.WriteBool('Main', 'ProxyTCPwdMngr', True);
	FConfigFile.DeleteKey('Main', 'ProxyPassword');
end;

function TPluginSettingsManager.GetSettings: TPluginSettings;
begin
	Result := Settings;
end;

end.

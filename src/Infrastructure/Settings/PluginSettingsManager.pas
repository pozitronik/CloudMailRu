unit PluginSettingsManager;

{Interface for plugin settings management, decoupled from INI file implementation}

interface

uses
	SysUtils,
	PathHelper,
	Windows,
	SettingsConstants,
	LanguageStrings,
	CloudConstants,
	Classes,
	PluginSettings,
	StreamingSettings,
	ConfigFile,
	Environment;

type

	IPluginSettingsManager = interface
		['{1028658B-966F-43D1-9329-A82E41726BEB}']
		function GetSettings: TPluginSettings;
		procedure SetSettings(Value: TPluginSettings);
		procedure Save;
		procedure SwitchProxyPasswordStorage;
		function GetStreamingSettings(const FileName: WideString): TStreamingSettings;
		procedure SetStreamingSettings(const FileName: WideString; StreamingSettings: TStreamingSettings);
		procedure GetStreamingExtensionsList(ExtensionsList: TStrings);
		procedure RemoveStreamingExtension(const Extension: WideString);
		function GetAccountsIniFilePath: WideString;
		procedure Refresh;
	end;

	{Null implementation for testing - returns defaults, no-op for writes}
	TNullPluginSettingsManager = class(TInterfacedObject, IPluginSettingsManager)
	private
		FSettings: TPluginSettings;
	public
		function GetSettings: TPluginSettings;
		procedure SetSettings(Value: TPluginSettings);
		procedure Save;
		procedure SwitchProxyPasswordStorage;
		function GetStreamingSettings(const FileName: WideString): TStreamingSettings;
		procedure SetStreamingSettings(const FileName: WideString; StreamingSettings: TStreamingSettings);
		procedure GetStreamingExtensionsList(ExtensionsList: TStrings);
		procedure RemoveStreamingExtension(const Extension: WideString);
		function GetAccountsIniFilePath: WideString;
		procedure Refresh;
	end;

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
		Settings: TPluginSettings; {Public field for direct nested access in tests and legacy code}
		property ApplicationPath: WideString read FApplicationPath; {Required for tests}
		property IniFileDir: WideString read FIniFileDir; {Required for tests}
		property IniFilePath: WideString read FIniFilePath;
		property AccountsIniFilePath: WideString read GetAccountsIniFilePath; {The path to the accounts config file}

		constructor Create(Environment: IEnvironment = nil); overload; {finds the settings file using Environment}
		constructor Create(ConfigFile: IConfigFile); overload; {uses provided config file}
		procedure Refresh();

		procedure Save(); {save current options set into the file}
		procedure SwitchProxyPasswordStorage;

		{IPluginSettingsManager interface methods}
		function GetSettings: TPluginSettings;
		procedure SetSettings(Value: TPluginSettings);

		function GetStreamingSettings(const FileName: WideString): TStreamingSettings;
		procedure SetStreamingSettings(const FileName: WideString; StreamingSettings: TStreamingSettings);
		procedure GetStreamingExtensionsList(ExtensionsList: TStrings);
		procedure RemoveStreamingExtension(const Extension: WideString);

	end;

implementation

{TNullPluginSettingsManager}

function TNullPluginSettingsManager.GetSettings: TPluginSettings;
begin
	Result := FSettings;
end;

procedure TNullPluginSettingsManager.SetSettings(Value: TPluginSettings);
begin
	FSettings := Value;
end;

procedure TNullPluginSettingsManager.Save;
begin
	{No-op for null implementation}
end;

procedure TNullPluginSettingsManager.SwitchProxyPasswordStorage;
begin
	{No-op for null implementation}
end;

function TNullPluginSettingsManager.GetStreamingSettings(const FileName: WideString): TStreamingSettings;
begin
	Result := Default(TStreamingSettings);
end;

procedure TNullPluginSettingsManager.SetStreamingSettings(const FileName: WideString; StreamingSettings: TStreamingSettings);
begin
	{No-op for null implementation}
end;

procedure TNullPluginSettingsManager.GetStreamingExtensionsList(ExtensionsList: TStrings);
begin
	ExtensionsList.Clear;
end;

procedure TNullPluginSettingsManager.RemoveStreamingExtension(const Extension: WideString);
begin
	{No-op for null implementation}
end;

function TNullPluginSettingsManager.GetAccountsIniFilePath: WideString;
begin
	Result := EmptyWideStr;
end;

procedure TNullPluginSettingsManager.Refresh;
begin
	{No-op for null implementation}
end;

{TPluginSettingsManager}

function TPluginSettingsManager.GetSettings: TPluginSettings;
begin
	Result := Settings;
end;

procedure TPluginSettingsManager.SetSettings(Value: TPluginSettings);
begin
	Settings := Value;
end;

constructor TPluginSettingsManager.Create(ConfigFile: IConfigFile);
begin
	FConfigFile := ConfigFile;
	FEnvironment := nil;
	FIniFilePath := ConfigFile.GetFilePath;
	FIniFileDir := IncludeTrailingBackslash(ExtractFilePath(FIniFilePath));
	Refresh();
end;

constructor TPluginSettingsManager.Create(Environment: IEnvironment);
var
	AppDataDir: WideString;
	TempConfigFile: IConfigFile;
	TempIniDir: Integer;
begin
	if Environment = nil then
		FEnvironment := TWindowsEnvironment.Create
	else
		FEnvironment := Environment;

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
	Result := self.FIniFileDir + ACCOUNTS_CONFIG_FILE_NAME;
end;

procedure TPluginSettingsManager.Refresh;
begin
	try
		Settings.IniDir := FConfigFile.ReadInteger('Main', 'IniPath', INI_DIR_PLUGIN);
	except
		on E: ERangeError do
			Settings.IniDir := INI_DIR_PLUGIN;
	end;
	Settings.LoadSSLDLLOnlyFromPluginDir := FConfigFile.ReadBool('Main', 'LoadSSLDLLOnlyFromPluginDir', False);
	Settings.DescriptionEnabled := FConfigFile.ReadBool('Main', 'DescriptionEnabled', False);
	Settings.DescriptionEditorEnabled := FConfigFile.ReadBool('Main', 'DescriptionEditorEnabled', False);
	Settings.DescriptionCopyToCloud := FConfigFile.ReadBool('Main', 'DescriptionCopyToCloud', False);
	Settings.DescriptionCopyFromCloud := FConfigFile.ReadBool('Main', 'DescriptionCopyFromCloud', False);
	Settings.DescriptionTrackCloudFS := FConfigFile.ReadBool('Main', 'DescriptionTrackCloudFS', False);
	Settings.DescriptionFileName := FConfigFile.ReadString('Main', 'DescriptionFileName', 'descript.ion');
	Settings.TimestampMode := FConfigFile.ReadInteger('Main', 'TimestampMode', TimestampModeDisabled);
	Settings.TimestampFileName := FConfigFile.ReadString('Main', 'TimestampFileName', DEFAULT_TIMESTAMP_FILENAME);
	Settings.TimestampConflictMode := FConfigFile.ReadInteger('Main', 'TimestampConflictMode', TimestampConflictUseStored);
	Settings.CopyBetweenAccountsMode := FConfigFile.ReadInteger('Main', 'CopyBetweenAccountsMode', CopyBetweenAccountsModeDisabled);
	Settings.DisableMultiThreading := FConfigFile.ReadBool('Main', 'DisableMultiThreading', False);
	Settings.LogUserSpace := FConfigFile.ReadBool('Main', 'LogUserSpace', True);
	Settings.IconsMode := FConfigFile.ReadInteger('Main', 'IconsMode', IconsModeDisabled);
	Settings.ConnectionSettings.SocketTimeout := FConfigFile.ReadInteger('Main', 'SocketTimeout', DEFAULT_SOCKET_TIMEOUT);
	Settings.ConnectionSettings.UploadBPS := FConfigFile.ReadInteger('Main', 'UploadBPS', DEFAULT_SPEED_LIMIT);
	Settings.ConnectionSettings.DownloadBPS := FConfigFile.ReadInteger('Main', 'DownloadBPS', DEFAULT_SPEED_LIMIT);
	Settings.ChunkOverwriteMode := FConfigFile.ReadInteger('Main', 'ChunkOverwriteMode', ChunkOverwrite);
	Settings.DeleteFailOnUploadMode := FConfigFile.ReadInteger('Main', 'DeleteFailOnUploadMode', DeleteFailOnUploadAsk);
	Settings.OverwriteLocalMode := FConfigFile.ReadInteger('Main', 'OverwriteLocalMode', OverwriteLocalModeAsk);
	Settings.OperationErrorMode := FConfigFile.ReadInteger('Main', 'OperationErrorMode', OperationErrorModeAsk);
	Settings.RetryAttempts := FConfigFile.ReadInteger('Main', 'RetryAttempts', 1);
	Settings.AttemptWait := FConfigFile.ReadInteger('Main', 'AttemptWait', 1000);
	Settings.ConnectionSettings.ProxySettings.ProxyType := FConfigFile.ReadInteger('Main', 'ProxyType', ProxyNone);
	Settings.ConnectionSettings.ProxySettings.Server := FConfigFile.ReadString('Main', 'ProxyServer', EmptyWideStr);
	Settings.ConnectionSettings.ProxySettings.Port := FConfigFile.ReadInteger('Main', 'ProxyPort', 0);
	Settings.ConnectionSettings.ProxySettings.User := FConfigFile.ReadString('Main', 'ProxyUser', EmptyWideStr);
	Settings.ConnectionSettings.ProxySettings.UseTCPasswordManager := FConfigFile.ReadBool('Main', 'ProxyTCPwdMngr', False);
	Settings.ConnectionSettings.ProxySettings.Password := FConfigFile.ReadString('Main', 'ProxyPassword', EmptyWideStr);
	Settings.ConnectionSettings.UserAgent := FConfigFile.ReadString('Main', 'UserAgent', DEFAULT_USERAGENT);
	Settings.ShowTrashFolders := FConfigFile.ReadBool('Main', 'ShowTrashFolders', True);
	Settings.ShowSharedFolders := FConfigFile.ReadBool('Main', 'ShowSharedFolders', True);
	Settings.ShowInvitesFolders := FConfigFile.ReadBool('Main', 'ShowInvitesFolders', True);
	Settings.LogLevel := FConfigFile.ReadInteger('Main', 'LogLevel', LOG_LEVEL_CONNECT + LOG_LEVEL_FILE_OPERATION + LOG_LEVEL_DETAIL + LOG_LEVEL_WARNING + LOG_LEVEL_ERROR);
	Settings.PrecalculateHash := FConfigFile.ReadBool('Main', 'PrecalculateHash', True);
	Settings.ForcePrecalculateSize := FConfigFile.ReadInt64('Main', 'ForcePrecalculateSize', CLOUD_PRECALCULATE_LIMIT_DEFAULT);
	Settings.CheckCRC := FConfigFile.ReadBool('Main', 'CheckCRC', True);
	Settings.HashCalculatorStrategy := FConfigFile.ReadInteger('Main', 'HashCalculatorStrategy', HashStrategyAuto);
	Settings.SSLBackend := FConfigFile.ReadInteger('Main', 'SSLBackend', SSLBackendAuto);
	Settings.ThumbnailExtensions := FConfigFile.ReadString('Main', 'ThumbnailExtensions', DEFAULT_THUMBNAIL_EXTENSIONS);
	Settings.Language := FConfigFile.ReadString('Main', 'Language', EmptyWideStr);
	Settings.FileHistoryEnabled := FConfigFile.ReadBool('Main', 'FileHistoryEnabled', False);
	Settings.HideDescriptionFile := FConfigFile.ReadBool('Main', 'HideDescriptionFile', False);
	Settings.HideTimestampFile := FConfigFile.ReadBool('Main', 'HideTimestampFile', False);
	Settings.SkipDescriptionDownload := FConfigFile.ReadBool('Main', 'SkipDescriptionDownload', False);
	Settings.SkipTimestampDownload := FConfigFile.ReadBool('Main', 'SkipTimestampDownload', False);
	Settings.BuildThumbnailExtList;
	Settings.IniFilePath := FIniFilePath;
	Settings.AccountsIniFilePath := AccountsIniFilePath;
end;

procedure TPluginSettingsManager.Save;
begin
	FConfigFile.WriteIntegerIfNotDefault('Main', 'IniPath', Settings.IniDir, INI_DIR_PLUGIN);
	FConfigFile.WriteBoolIfNotDefault('Main', 'LoadSSLDLLOnlyFromPluginDir', Settings.LoadSSLDLLOnlyFromPluginDir, False);
	FConfigFile.WriteBoolIfNotDefault('Main', 'DescriptionEnabled', Settings.DescriptionEnabled, False);
	FConfigFile.WriteBoolIfNotDefault('Main', 'DescriptionEditorEnabled', Settings.DescriptionEditorEnabled, False);
	FConfigFile.WriteBoolIfNotDefault('Main', 'DescriptionCopyToCloud', Settings.DescriptionCopyToCloud, False);
	FConfigFile.WriteBoolIfNotDefault('Main', 'DescriptionCopyFromCloud', Settings.DescriptionCopyFromCloud, False);
	FConfigFile.WriteBoolIfNotDefault('Main', 'DescriptionTrackCloudFS', Settings.DescriptionTrackCloudFS, False);
	FConfigFile.WriteStringIfNotDefault('Main', 'DescriptionFileName', Settings.DescriptionFileName, 'descript.ion');
	FConfigFile.WriteIntegerIfNotDefault('Main', 'TimestampMode', Settings.TimestampMode, TimestampModeDisabled);
	FConfigFile.WriteStringIfNotDefault('Main', 'TimestampFileName', Settings.TimestampFileName, DEFAULT_TIMESTAMP_FILENAME);
	FConfigFile.WriteIntegerIfNotDefault('Main', 'TimestampConflictMode', Settings.TimestampConflictMode, TimestampConflictUseStored);
	FConfigFile.WriteIntegerIfNotDefault('Main', 'CopyBetweenAccountsMode', Settings.CopyBetweenAccountsMode, CopyBetweenAccountsModeDisabled);
	FConfigFile.WriteBoolIfNotDefault('Main', 'DisableMultiThreading', Settings.DisableMultiThreading, False);
	FConfigFile.WriteBoolIfNotDefault('Main', 'LogUserSpace', Settings.LogUserSpace, True);
	FConfigFile.WriteIntegerIfNotDefault('Main', 'IconsMode', Settings.IconsMode, IconsModeDisabled);
	FConfigFile.WriteIntegerIfNotDefault('Main', 'SocketTimeout', Settings.ConnectionSettings.SocketTimeout, DEFAULT_SOCKET_TIMEOUT);
	FConfigFile.WriteIntegerIfNotDefault('Main', 'UploadBPS', Settings.ConnectionSettings.UploadBPS, DEFAULT_SPEED_LIMIT);
	FConfigFile.WriteIntegerIfNotDefault('Main', 'DownloadBPS', Settings.ConnectionSettings.DownloadBPS, DEFAULT_SPEED_LIMIT);
	FConfigFile.WriteIntegerIfNotDefault('Main', 'ChunkOverwriteMode', Settings.ChunkOverwriteMode, ChunkOverwrite);
	FConfigFile.WriteIntegerIfNotDefault('Main', 'DeleteFailOnUploadMode', Settings.DeleteFailOnUploadMode, DeleteFailOnUploadAsk);
	FConfigFile.WriteIntegerIfNotDefault('Main', 'OverwriteLocalMode', Settings.OverwriteLocalMode, OverwriteLocalModeAsk);
	FConfigFile.WriteIntegerIfNotDefault('Main', 'OperationErrorMode', Settings.OperationErrorMode, OperationErrorModeAsk);
	FConfigFile.WriteIntegerIfNotDefault('Main', 'RetryAttempts', Settings.RetryAttempts, 1);
	FConfigFile.WriteIntegerIfNotDefault('Main', 'AttemptWait', Settings.AttemptWait, 1000);
	FConfigFile.WriteIntegerIfNotDefault('Main', 'ProxyType', Settings.ConnectionSettings.ProxySettings.ProxyType, ProxyNone);
	FConfigFile.WriteStringIfNotDefault('Main', 'ProxyServer', Settings.ConnectionSettings.ProxySettings.Server, EmptyWideStr);
	FConfigFile.WriteIntegerIfNotDefault('Main', 'ProxyPort', Settings.ConnectionSettings.ProxySettings.Port, 0);
	FConfigFile.WriteStringIfNotDefault('Main', 'ProxyUser', Settings.ConnectionSettings.ProxySettings.User, EmptyWideStr);
	FConfigFile.WriteBoolIfNotDefault('Main', 'ProxyTCPwdMngr', Settings.ConnectionSettings.ProxySettings.UseTCPasswordManager, False);
	FConfigFile.WriteStringIfNotDefault('Main', 'ProxyPassword', Settings.ConnectionSettings.ProxySettings.Password, EmptyWideStr);
	FConfigFile.WriteStringIfNotDefault('Main', 'UserAgent', Settings.ConnectionSettings.UserAgent, DEFAULT_USERAGENT);
	FConfigFile.WriteBoolIfNotDefault('Main', 'ShowTrashFolders', Settings.ShowTrashFolders, True);
	FConfigFile.WriteBoolIfNotDefault('Main', 'ShowSharedFolders', Settings.ShowSharedFolders, True);
	FConfigFile.WriteBoolIfNotDefault('Main', 'ShowInvitesFolders', Settings.ShowInvitesFolders, True);
	FConfigFile.WriteIntegerIfNotDefault('Main', 'LogLevel', Settings.LogLevel, LOG_LEVEL_CONNECT + LOG_LEVEL_FILE_OPERATION + LOG_LEVEL_DETAIL + LOG_LEVEL_WARNING + LOG_LEVEL_ERROR);
	FConfigFile.WriteBoolIfNotDefault('Main', 'PrecalculateHash', Settings.PrecalculateHash, True);
	FConfigFile.WriteInt64IfNotDefault('Main', 'ForcePrecalculateSize', Settings.ForcePrecalculateSize, CLOUD_PRECALCULATE_LIMIT_DEFAULT);
	FConfigFile.WriteBoolIfNotDefault('Main', 'CheckCRC', Settings.CheckCRC, True);
	FConfigFile.WriteIntegerIfNotDefault('Main', 'HashCalculatorStrategy', Settings.HashCalculatorStrategy, HashStrategyAuto);
	FConfigFile.WriteIntegerIfNotDefault('Main', 'SSLBackend', Settings.SSLBackend, SSLBackendAuto);
	FConfigFile.WriteStringIfNotDefault('Main', 'ThumbnailExtensions', Settings.ThumbnailExtensions, DEFAULT_THUMBNAIL_EXTENSIONS);
	FConfigFile.WriteStringIfNotDefault('Main', 'Language', Settings.Language, EmptyWideStr);
	FConfigFile.WriteBoolIfNotDefault('Main', 'FileHistoryEnabled', Settings.FileHistoryEnabled, False);
	FConfigFile.WriteBoolIfNotDefault('Main', 'HideDescriptionFile', Settings.HideDescriptionFile, False);
	FConfigFile.WriteBoolIfNotDefault('Main', 'HideTimestampFile', Settings.HideTimestampFile, False);
	FConfigFile.WriteBoolIfNotDefault('Main', 'SkipDescriptionDownload', Settings.SkipDescriptionDownload, False);
	FConfigFile.WriteBoolIfNotDefault('Main', 'SkipTimestampDownload', Settings.SkipTimestampDownload, False);
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

function ExtractStreamingExt(const FileName: WideString): WideString;
{Extracts extension from filename or extension-only input.
 Handles both 'file.mp4' -> 'mp4' and '.mp4' -> 'mp4'}
var
	I: Integer;
	S: string;
begin
	{First try standard extraction for normal filenames}
	S := FileName;
	I := S.LastDelimiter('.' + '/' + '\' + DriveDelim);
	if (I > 0) and (S.Chars[I] = '.') then
		Exit(S.Substring(I + 1));

	{Handle extension-only input like '.mp4': strip leading dot}
	if (Length(FileName) > 1) and (FileName[1] = '.') then
		Exit(Copy(FileName, 2, Length(FileName) - 1));

	Result := EmptyWideStr;
end;

function TPluginSettingsManager.GetStreamingSettings(const FileName: WideString): TStreamingSettings;
var
	SectionName: WideString;
	Ext: WideString;
begin
	Result := default (TStreamingSettings);
	Result.Format := STREAMING_FORMAT_UNSET;
	Ext := ExtractStreamingExt(FileName);
	if Ext = EmptyWideStr then
		Exit;
	SectionName := StreamingPrefix + Ext;
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
	Ext: WideString;
begin
	Ext := ExtractStreamingExt(FileName);
	if Ext <> EmptyWideStr then
	begin
		SectionName := StreamingPrefix + Ext;
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

end.

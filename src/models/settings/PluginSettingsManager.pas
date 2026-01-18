unit PluginSettingsManager;

interface

uses
	IniFiles,
	SysUtils,
	FileHelper,
	PathHelper,
	WindowsHelper,
	Windows,
	SETTINGS_CONSTANTS,
	LANGUAGE_STRINGS,
	CMRConstants,
	Classes,
	IniFilesHelper,
	PluginSettings,
	StreamingSettings,
	IPluginSettingsManagerInterface;

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

		function GetAccountsIniFilePath: WideString;

	public
		Settings: TPluginSettings;
		property ApplicationPath: WideString read FApplicationPath; {Required for tests}
		property IniFileDir: WideString read FIniFileDir; {Required for tests}
		property IniFilePath: WideString read FIniFilePath;
		property AccountsIniFilePath: WideString read GetAccountsIniFilePath; {The path to the accounts config file}

		constructor Create(); overload; {finds the settings file by itself}
		constructor Create(IniFilePath: WideString); overload;
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

{TMRCSettings}

constructor TPluginSettingsManager.Create(IniFilePath: WideString);
begin
	self.FIniFilePath := IniFilePath;
	Refresh();
end;

constructor TPluginSettingsManager.Create;
var
	AppDataDir: WideString;
	TempManager: TPluginSettingsManager;
begin
	AppDataDir := IncludeTrailingBackslash(IncludeTrailingBackslash(SysUtils.GetEnvironmentVariable('APPDATA')) + APPDATA_DIR_NAME);
	FApplicationPath := IncludeTrailingBackslash(ExtractFilePath(GetModuleName(hInstance)));

	if FileExists(GetUNCFilePath(FApplicationPath + PLUGIN_CONFIG_FILE_NAME)) then
	begin
		TempManager := TPluginSettingsManager.Create(GetUNCFilePath(FApplicationPath + PLUGIN_CONFIG_FILE_NAME));
		try
			case TempManager.Settings.IniDir of
				INI_DIR_PLUGIN:
					begin
						self.FIniFileDir := FApplicationPath;
					end;
				INI_DIR_APPDATA: //use appdata path
					begin
						self.FIniFileDir := AppDataDir;
					end;
				INI_DIR_AUTO: //use plugin dir if writeable
					begin
						if IsWriteable(FApplicationPath) then
							self.FIniFileDir := FApplicationPath
						else
							self.FIniFileDir := AppDataDir;
					end;
			end;
		finally
			TempManager.Free;
		end;

	end else begin
		if IsWriteable(FApplicationPath) then
		begin
			self.FIniFileDir := FApplicationPath;
		end else begin
			self.FIniFileDir := AppDataDir;
		end;
	end;

	if not DirectoryExists(GetUNCFilePath(self.FIniFileDir)) then
		createDir(GetUNCFilePath(self.FIniFileDir)); //assuming this is inside the appdata dir

	self.FIniFilePath := GetUNCFilePath(self.FIniFileDir + PLUGIN_CONFIG_FILE_NAME);
	Refresh();

end;

function TPluginSettingsManager.GetAccountsIniFilePath: WideString;
begin
	result := self.FIniFileDir + ACCOUNTS_CONFIG_FILE_NAME;
end;

procedure TPluginSettingsManager.Refresh;
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	try
		with self.Settings do
		begin
			try
				IniDir := IniFile.ReadInteger('Main', 'IniPath', INI_DIR_PLUGIN); //TODO: Key should be renamed
			except
				on E: ERangeError do
				begin
					IniDir := INI_DIR_PLUGIN;
				end;
			end;
			LoadSSLDLLOnlyFromPluginDir := IniFile.ReadBool('Main', 'LoadSSLDLLOnlyFromPluginDir', False);
			PreserveFileTime := IniFile.ReadBool('Main', 'PreserveFileTime', False);
			DescriptionEnabled := IniFile.ReadBool('Main', 'DescriptionEnabled', False);
			DescriptionEditorEnabled := IniFile.ReadBool('Main', 'DescriptionEditorEnabled', False);
			DescriptionCopyToCloud := IniFile.ReadBool('Main', 'DescriptionCopyToCloud', False);
			DescriptionCopyFromCloud := IniFile.ReadBool('Main', 'DescriptionCopyFromCloud', False);
			DescriptionTrackCloudFS := IniFile.ReadBool('Main', 'DescriptionTrackCloudFS', False);
			DescriptionFileName := IniFile.ReadString('Main', 'DescriptionFileName', 'descript.ion');
			CopyBetweenAccountsMode := IniFile.ReadInteger('Main', 'CopyBetweenAccountsMode', CopyBetweenAccountsModeDisabled);
			DisableMultiThreading := IniFile.ReadBool('Main', 'DisableMultiThreading', False);
			LogUserSpace := IniFile.ReadBool('Main', 'LogUserSpace', True);
			IconsMode := IniFile.ReadInteger('Main', 'IconsMode', IconsModeDisabled);
			ConnectionSettings.SocketTimeout := IniFile.ReadInteger('Main', 'SocketTimeout', -1);
			ConnectionSettings.UploadBPS := IniFile.ReadInteger('Main', 'UploadBPS', -1);
			ConnectionSettings.DownloadBPS := IniFile.ReadInteger('Main', 'DownloadBPS', -1);
			CloudMaxFileSize := IniFile.ReadInt64('Main', 'CloudMaxFileSize', CLOUD_MAX_FILESIZE_DEFAULT);
			ChunkOverwriteMode := IniFile.ReadInteger('Main', 'ChunkOverwriteMode', ChunkOverwrite);
			DeleteFailOnUploadMode := IniFile.ReadInteger('Main', 'DeleteFailOnUploadMode', DeleteFailOnUploadAsk);
			OverwriteLocalMode := IniFile.ReadInteger('Main', 'OverwriteLocalMode', OverwriteLocalModeAsk);
			OperationErrorMode := IniFile.ReadInteger('Main', 'OperationErrorMode', OperationErrorModeAsk);
			RetryAttempts := IniFile.ReadInteger('Main', 'RetryAttempts', 1);
			AttemptWait := IniFile.ReadInteger('Main', 'AttemptWait', 1000);
			ConnectionSettings.ProxySettings.ProxyType := IniFile.ReadInteger('Main', 'ProxyType', ProxyNone);
			ConnectionSettings.ProxySettings.Server := IniFile.ReadString('Main', 'ProxyServer', EmptyWideStr);
			ConnectionSettings.ProxySettings.Port := IniFile.ReadInteger('Main', 'ProxyPort', 0);
			ConnectionSettings.ProxySettings.User := IniFile.ReadString('Main', 'ProxyUser', EmptyWideStr);
			ConnectionSettings.ProxySettings.UseTCPasswordManager := IniFile.ReadBool('Main', 'ProxyTCPwdMngr', False);
			ConnectionSettings.ProxySettings.Password := IniFile.ReadString('Main', 'ProxyPassword', EmptyWideStr);
			ConnectionSettings.UserAgent := IniFile.ReadString('Main', 'UserAgent', DEFAULT_USERAGENT);
			DownloadLinksEncode := IniFile.ReadBool('Main', 'DownloadLinksEncode', True);
			AutoUpdateDownloadListing := IniFile.ReadBool('Main', 'AutoUpdateDownloadListing', True);
			ShowTrashFolders := IniFile.ReadBool('Main', 'ShowTrashFolders', True);
			ShowSharedFolders := IniFile.ReadBool('Main', 'ShowSharedFolders', True);
			ShowInvitesFolders := IniFile.ReadBool('Main', 'ShowInvitesFolders', True);
			LogLevel := IniFile.ReadInteger('Main', 'LogLevel', LOG_LEVEL_CONNECT + LOG_LEVEL_FILE_OPERATION + LOG_LEVEL_DETAIL + LOG_LEVEL_WARNING + LOG_LEVEL_ERROR);
			PrecalculateHash := IniFile.ReadBool('Main', 'PrecalculateHash', True);
			ForcePrecalculateSize := IniFile.ReadInt64('Main', 'ForcePrecalculateSize', CLOUD_PRECALCULATE_LIMIT_DEFAULT);
			CheckCRC := IniFile.ReadBool('Main', 'CheckCRC', True);
		end;
	finally
		IniFile.Free;
	end;
	self.Settings.IniFilePath := self.FIniFilePath;
	self.Settings.AccountsIniFilePath := self.AccountsIniFilePath;
end;

procedure TPluginSettingsManager.Save;
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	try
		with self.Settings do
		begin
			IniFile.WriteIntegerIfNotDefault('Main', 'IniPath', IniDir, INI_DIR_PLUGIN);
			IniFile.WriteBoolIfNotDefault('Main', 'LoadSSLDLLOnlyFromPluginDir', LoadSSLDLLOnlyFromPluginDir, False);
			IniFile.WriteBoolIfNotDefault('Main', 'PreserveFileTime', PreserveFileTime, False);
			IniFile.WriteBoolIfNotDefault('Main', 'DescriptionEnabled', DescriptionEnabled, False);
			IniFile.WriteBoolIfNotDefault('Main', 'DescriptionEditorEnabled', DescriptionEditorEnabled, False);
			IniFile.WriteBoolIfNotDefault('Main', 'DescriptionCopyToCloud', DescriptionCopyToCloud, False);
			IniFile.WriteBoolIfNotDefault('Main', 'DescriptionCopyFromCloud', DescriptionCopyFromCloud, False);
			IniFile.WriteBoolIfNotDefault('Main', 'DescriptionTrackCloudFS', DescriptionTrackCloudFS, False);
			IniFile.WriteStringIfNotDefault('Main', 'DescriptionFileName', DescriptionFileName, 'descript.ion');
			IniFile.WriteIntegerIfNotDefault('Main', 'CopyBetweenAccountsMode', CopyBetweenAccountsMode, CopyBetweenAccountsModeDisabled);
			IniFile.WriteBoolIfNotDefault('Main', 'DisableMultiThreading', DisableMultiThreading, False);
			IniFile.WriteBoolIfNotDefault('Main', 'LogUserSpace', LogUserSpace, True);
			IniFile.WriteIntegerIfNotDefault('Main', 'IconsMode', IconsMode, IconsModeDisabled);
			IniFile.WriteIntegerIfNotDefault('Main', 'SocketTimeout', ConnectionSettings.SocketTimeout, -1);
			IniFile.WriteIntegerIfNotDefault('Main', 'UploadBPS', ConnectionSettings.UploadBPS, -1);
			IniFile.WriteIntegerIfNotDefault('Main', 'DownloadBPS', ConnectionSettings.DownloadBPS, -1);
			IniFile.WriteInt64IfNotDefault('Main', 'CloudMaxFileSize', CloudMaxFileSize, CLOUD_MAX_FILESIZE_DEFAULT);
			IniFile.WriteIntegerIfNotDefault('Main', 'ChunkOverwriteMode', ChunkOverwriteMode, ChunkOverwrite);
			IniFile.WriteIntegerIfNotDefault('Main', 'DeleteFailOnUploadMode', DeleteFailOnUploadMode, DeleteFailOnUploadAsk);
			IniFile.WriteIntegerIfNotDefault('Main', 'OverwriteLocalMode', OverwriteLocalMode, OverwriteLocalModeAsk);
			IniFile.WriteIntegerIfNotDefault('Main', 'OperationErrorMode', OperationErrorMode, OperationErrorModeAsk);
			IniFile.WriteIntegerIfNotDefault('Main', 'RetryAttempts', RetryAttempts, 1);
			IniFile.WriteIntegerIfNotDefault('Main', 'AttemptWait', AttemptWait, 1000);
			IniFile.WriteIntegerIfNotDefault('Main', 'ProxyType', ConnectionSettings.ProxySettings.ProxyType, ProxyNone);
			IniFile.WriteStringIfNotDefault('Main', 'ProxyServer', ConnectionSettings.ProxySettings.Server, EmptyWideStr);
			IniFile.WriteIntegerIfNotDefault('Main', 'ProxyPort', ConnectionSettings.ProxySettings.Port, 0);
			IniFile.WriteStringIfNotDefault('Main', 'ProxyUser', ConnectionSettings.ProxySettings.User, EmptyWideStr);
			IniFile.WriteBoolIfNotDefault('Main', 'ProxyTCPwdMngr', ConnectionSettings.ProxySettings.UseTCPasswordManager, False);
			IniFile.WriteStringIfNotDefault('Main', 'ProxyPassword', ConnectionSettings.ProxySettings.Password, EmptyWideStr);
			IniFile.WriteStringIfNotDefault('Main', 'UserAgent', ConnectionSettings.UserAgent, DEFAULT_USERAGENT);
			IniFile.WriteBoolIfNotDefault('Main', 'DownloadLinksEncode', DownloadLinksEncode, True);
			IniFile.WriteBoolIfNotDefault('Main', 'AutoUpdateDownloadListing', AutoUpdateDownloadListing, True);
			IniFile.WriteBoolIfNotDefault('Main', 'ShowTrashFolders', ShowTrashFolders, True);
			IniFile.WriteBoolIfNotDefault('Main', 'ShowSharedFolders', ShowSharedFolders, True);
			IniFile.WriteBoolIfNotDefault('Main', 'ShowInvitesFolders', ShowInvitesFolders, True);
			IniFile.WriteIntegerIfNotDefault('Main', 'LogLevel', LogLevel, LOG_LEVEL_CONNECT + LOG_LEVEL_FILE_OPERATION + LOG_LEVEL_DETAIL + LOG_LEVEL_WARNING + LOG_LEVEL_ERROR);
			IniFile.WriteBoolIfNotDefault('Main', 'PrecalculateHash', PrecalculateHash, True);
			IniFile.WriteInt64IfNotDefault('Main', 'ForcePrecalculateSize', ForcePrecalculateSize, CLOUD_PRECALCULATE_LIMIT_DEFAULT);
			IniFile.WriteBoolIfNotDefault('Main', 'CheckCRC', CheckCRC, True);
		end;
	finally
		IniFile.Free;
	end;
end;

procedure TPluginSettingsManager.GetStreamingExtensionsList(ExtensionsList: TStrings);
var
	IniFile: TIniFile;
	TempList: TStrings;
	Line: String;
begin
	ExtensionsList.Clear;
	IniFile := TIniFile.Create(IniFilePath);
	try
		TempList := TStringList.Create;
		try
			IniFile.ReadSections(TempList);
			for Line in TempList do
			begin
				if Line.StartsWith(StreamingPrefix) then
					ExtensionsList.Add(Line.Substring(Length(StreamingPrefix)));
			end;
		finally
			TempList.Free;
		end;
	finally
		IniFile.Free;
	end;
end;

procedure TPluginSettingsManager.RemoveStreamingExtension(const Extension: WideString);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	try
		IniFile.EraseSection(StreamingPrefix + Extension);
	finally
		IniFile.Free;
	end;
end;

function TPluginSettingsManager.GetStreamingSettings(const FileName: WideString): TStreamingSettings;
var
	IniFile: TIniFile;
	SectionName: WideString;
begin
	result := default (TStreamingSettings);
	result.Format := STREAMING_FORMAT_UNSET;
	IniFile := TIniFile.Create(IniFilePath);
	try
		SectionName := StreamingPrefix + ExtractUniversalFileExt(FileName, True);
		if IniFile.SectionExists(SectionName) then
		begin
			result.Command := IniFile.ReadString(SectionName, 'Command', EmptyWideStr);
			result.Parameters := IniFile.ReadString(SectionName, 'Parameters', EmptyWideStr);
			result.StartPath := IniFile.ReadString(SectionName, 'StartPath', EmptyWideStr);
			result.Format := IniFile.ReadInteger(SectionName, 'Format', STREAMING_FORMAT_NONE);
		end;
	finally
		IniFile.Free;
	end;
end;

procedure TPluginSettingsManager.SetStreamingSettings(const FileName: WideString; StreamingSettings: TStreamingSettings);
var
	IniFile: TIniFile;
	SectionName: WideString;
begin
	if ExtractUniversalFileExt(FileName, True) <> EmptyWideStr then
	begin
		SectionName := StreamingPrefix + ExtractUniversalFileExt(FileName, True);
		IniFile := TIniFile.Create(IniFilePath);
		try
			IniFile.WriteString(SectionName, 'Command', StreamingSettings.Command);
			IniFile.WriteString(SectionName, 'Parameters', StreamingSettings.Parameters);
			IniFile.WriteString(SectionName, 'StartPath', StreamingSettings.StartPath);
			IniFile.WriteInteger(SectionName, 'Format', StreamingSettings.Format);
		finally
			IniFile.Free;
		end;
	end;
end;

procedure TPluginSettingsManager.SwitchProxyPasswordStorage;
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	try
		IniFile.WriteBool('Main', 'ProxyTCPwdMngr', True);
		IniFile.DeleteKey('Main', 'ProxyPassword');
	finally
		IniFile.Free;
	end;
end;

function TPluginSettingsManager.GetSettings: TPluginSettings;
begin
	Result := Settings;
end;

end.

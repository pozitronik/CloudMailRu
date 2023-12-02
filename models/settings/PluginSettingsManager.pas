﻿unit PluginSettingsManager;

interface

uses
	IniFiles,
	SysUtils,
	FileHelper,
	PathHelper,
	WindowsHelper,
	Windows,
	SETTINGS_CONSTANTS,
	CMRStrings,
	CMRConstants,
	PluginSettings;

type

	{Files names and path naming convetions:
	 (File)Path or (Dir)Patn should contain the full absolute path for a file or a directory
	 (File)Name or (Dir)Name should contain only the name of a file or a directory
	}

	TPluginSettingsManager = class
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
		TempManager.Free;

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
	with self.Settings do
	begin
		IniDir := IniFile.ReadInteger('Main', 'IniPath', 0); //TODO: Key should be renamed
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
		IconsMode := IniFile.ReadInteger('Main', 'IconsMode', 0);
		ConnectionSettings.SocketTimeout := IniFile.ReadInteger('Main', 'SocketTimeout', -1);
		ConnectionSettings.UploadBPS := IniFile.ReadInteger('Main', 'UploadBPS', -1);
		ConnectionSettings.DownloadBPS := IniFile.ReadInteger('Main', 'DownloadBPS', -1);
		CloudMaxFileSize := IniFile.ReadInt64('Main', 'CloudMaxFileSize', CLOUD_MAX_FILESIZE_DEFAULT);
		ChunkOverwriteMode := IniFile.ReadInteger('Main', 'ChunkOverwriteMode', 0);
		DeleteFailOnUploadMode := IniFile.ReadInteger('Main', 'DeleteFailOnUploadMode', 0);
		OverwriteLocalMode := IniFile.ReadInteger('Main', 'OverwriteLocalMode', 0);
		OperationErrorMode := IniFile.ReadInteger('Main', 'OperationErrorMode', 0);
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
	IniFile.Destroy;
	self.Settings.IniFilePath := self.FIniFilePath;
	self.Settings.AccountsIniFilePath := self.AccountsIniFilePath;
end;

procedure TPluginSettingsManager.Save;
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	with self.Settings do
	begin
		IniFile.WriteInteger('Main', 'IniPath', IniDir);
		IniFile.WriteBool('Main', 'LoadSSLDLLOnlyFromPluginDir', LoadSSLDLLOnlyFromPluginDir);
		IniFile.WriteBool('Main', 'PreserveFileTime', PreserveFileTime);
		IniFile.WriteBool('Main', 'DescriptionEnabled', DescriptionEnabled);
		IniFile.WriteBool('Main', 'DescriptionEditorEnabled', DescriptionEditorEnabled);
		IniFile.WriteBool('Main', 'DescriptionCopyToCloud', DescriptionCopyToCloud);
		IniFile.WriteBool('Main', 'DescriptionCopyFromCloud', DescriptionCopyFromCloud);
		IniFile.WriteBool('Main', 'DescriptionTrackCloudFS', DescriptionTrackCloudFS);
		IniFile.WriteString('Main', 'DescriptionFileName', DescriptionFileName);
		IniFile.WriteInteger('Main', 'CopyBetweenAccountsMode', CopyBetweenAccountsMode);
		IniFile.WriteBool('Main', 'DisableMultiThreading', DisableMultiThreading);
		IniFile.WriteBool('Main', 'LogUserSpace', LogUserSpace);
		IniFile.WriteInteger('Main', 'IconsMode', IconsMode);
		IniFile.WriteInteger('Main', 'SocketTimeout', ConnectionSettings.SocketTimeout);
		IniFile.WriteInteger('Main', 'UploadBPS', ConnectionSettings.UploadBPS);
		IniFile.WriteInteger('Main', 'DownloadBPS', ConnectionSettings.DownloadBPS);
		IniFile.WriteInt64('Main', 'CloudMaxFileSize', CloudMaxFileSize);
		IniFile.WriteInteger('Main', 'ChunkOverwriteMode', ChunkOverwriteMode);
		IniFile.WriteInteger('Main', 'DeleteFailOnUploadMode', DeleteFailOnUploadMode);
		IniFile.WriteInteger('Main', 'OverwriteLocalMode', OverwriteLocalMode);
		IniFile.WriteInteger('Main', 'OperationErrorMode', OperationErrorMode);
		IniFile.WriteInteger('Main', 'RetryAttempts', RetryAttempts);
		IniFile.WriteInteger('Main', 'AttemptWait', AttemptWait);
		IniFile.WriteInteger('Main', 'ProxyType', ConnectionSettings.ProxySettings.ProxyType);
		IniFile.WriteString('Main', 'ProxyServer', ConnectionSettings.ProxySettings.Server);
		IniFile.WriteInteger('Main', 'ProxyPort', ConnectionSettings.ProxySettings.Port);
		IniFile.WriteString('Main', 'ProxyUser', ConnectionSettings.ProxySettings.User);
		IniFile.WriteBool('Main', 'ProxyTCPwdMngr', ConnectionSettings.ProxySettings.UseTCPasswordManager);
		IniFile.WriteString('Main', 'ProxyPassword', ConnectionSettings.ProxySettings.Password);
		IniFile.WriteString('Main', 'UserAgent', ConnectionSettings.UserAgent);
		IniFile.WriteBool('Main', 'DownloadLinksEncode', DownloadLinksEncode);
		IniFile.WriteBool('Main', 'AutoUpdateDownloadListing', AutoUpdateDownloadListing);
		IniFile.WriteBool('Main', 'ShowTrashFolders', ShowTrashFolders);
		IniFile.WriteBool('Main', 'ShowSharedFolders', ShowSharedFolders);
		IniFile.WriteBool('Main', 'ShowInvitesFolders', ShowInvitesFolders);
		IniFile.WriteInteger('Main', 'LogLevel', LogLevel);
		IniFile.WriteBool('Main', 'PrecalculateHash', PrecalculateHash);
		IniFile.WriteInt64('Main', 'ForcePrecalculateSize', ForcePrecalculateSize);
		IniFile.WriteBool('Main', 'CheckCRC', CheckCRC);
	end;
	IniFile.Destroy;
end;

procedure TPluginSettingsManager.SwitchProxyPasswordStorage;
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	IniFile.WriteBool('Main', 'ProxyTCPwdMngr', True);
	IniFile.DeleteKey('Main', 'ProxyPassword');
	IniFile.Destroy;
end;

end.

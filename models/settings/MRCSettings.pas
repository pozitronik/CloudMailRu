unit MRCSettings;

interface

uses
	IniFiles,
	SysUtils,
	Variants,
	FileHelper,
	PathHelper,
	WindowsHelper,
	Windows,
	SETTINGS_CONSTANTS,
	CMRStrings,
	CMRConstants,
	ConnectionSettings,
	AbstractMRCSettings;

type
	TMRCSettings = class(TAbstractMRCSettings)
	private
		ApplicationPath: WideString; // the directory of the current binary file
		IniFilePath: WideString;
		IniFileDir: WideString; // the directory where the currently used ini files (global+accounts) are

		function GetAccountsIniFileName: WideString;
	public

		property IniDir: WideString read IniFileDir;
		property PluginPath: WideString read ApplicationPath;
		property AccountsIniFileName: WideString read GetAccountsIniFileName; //Path to the accounts config file
		property IniFileName: WideString read IniFilePath;
		constructor Create(); overload; //finds the settings file by itself
		constructor Create(IniFilePath: WideString); overload;
		procedure Refresh();

		procedure SetSettingValue(OptionName: WideString; OptionValue: Variant); override;
		procedure Save(); override; //save current options set into the file

	end;

implementation

{TMRCSettings}

constructor TMRCSettings.Create(IniFilePath: WideString);
begin
	self.IniFilePath := IniFilePath;
	self.FSaveOnChange := True;
	Refresh();
end;

constructor TMRCSettings.Create;
var
	AppDataDir: WideString;
	TempSettings: TMRCSettings;
begin
	AppDataDir := IncludeTrailingBackslash(IncludeTrailingBackslash(SysUtils.GetEnvironmentVariable('APPDATA')) + APPDATA_DIR_NAME);
	ApplicationPath := IncludeTrailingBackslash(ExtractFilePath(GetModuleName(hInstance)));

	if FileExists(GetUNCFilePath(ApplicationPath + PLUGIN_CONFIG_FILE_NAME)) then
	begin
		TempSettings := TMRCSettings.Create(GetUNCFilePath(ApplicationPath + PLUGIN_CONFIG_FILE_NAME));

		case TempSettings.IniPath of
			INI_PATH_PLUGIN_DIR:
				begin
					self.IniFileDir := PluginPath;
				end;
			INI_PATH_APPDATA: //use appdata path
				begin
					self.IniFileDir := AppDataDir;
				end;
			INI_PATH_AUTO: //use plugin dir if writeable
				begin
					if IsWriteable(PluginPath) then
						self.IniFileDir := PluginPath
					else
						self.IniFileDir := AppDataDir;
				end;
		end;
		TempSettings.Free;

	end else begin
		if IsWriteable(ApplicationPath) then
		begin
			self.IniFileDir := ApplicationPath;
		end else begin
			self.IniFileDir := AppDataDir;
		end;
	end;

	if not DirectoryExists(GetUNCFilePath(self.IniFileDir)) then
		createDir(GetUNCFilePath(self.IniFileDir)); //assuming this is inside the appdata dir

	self.IniFilePath := GetUNCFilePath(self.IniFileDir + PLUGIN_CONFIG_FILE_NAME);
	self.FSaveOnChange := True;
	Refresh();

end;

function TMRCSettings.GetAccountsIniFileName: WideString;
begin
	result := self.IniDir + ACCOUNTS_CONFIG_FILE_NAME;
end;

procedure TMRCSettings.Refresh;
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	FIniPath := IniFile.ReadInteger('Main', 'IniPath', 0);
	FLoadSSLDLLOnlyFromPluginDir := IniFile.ReadBool('Main', 'LoadSSLDLLOnlyFromPluginDir', false);
	FPreserveFileTime := IniFile.ReadBool('Main', 'PreserveFileTime', false);
	FDescriptionEnabled := IniFile.ReadBool('Main', 'DescriptionEnabled', false);
	FDescriptionEditorEnabled := IniFile.ReadBool('Main', 'DescriptionEditorEnabled', false);
	FDescriptionCopyToCloud := IniFile.ReadBool('Main', 'DescriptionCopyToCloud', false);
	FDescriptionCopyFromCloud := IniFile.ReadBool('Main', 'DescriptionCopyFromCloud', false);
	FDescriptionTrackCloudFS := IniFile.ReadBool('Main', 'DescriptionTrackCloudFS', false);
	FDescriptionFileName := IniFile.ReadString('Main', 'DescriptionFileName', 'descript.ion');
	FCopyBetweenAccountsMode := IniFile.ReadInteger('Main', 'CopyBetweenAccountsMode', CopyBetweenAccountsModeDisabled);
	FDisableMultiThreading := IniFile.ReadBool('Main', 'DisableMultiThreading', false);
	FLogUserSpace := IniFile.ReadBool('Main', 'LogUserSpace', True);
	FIconsMode := IniFile.ReadInteger('Main', 'IconsMode', 0);
	FConnectionSettings.SocketTimeout := IniFile.ReadInteger('Main', 'SocketTimeout', -1);
	FConnectionSettings.UploadBPS := IniFile.ReadInteger('Main', 'UploadBPS', -1);
	FConnectionSettings.DownloadBPS := IniFile.ReadInteger('Main', 'DownloadBPS', -1);
	FCloudMaxFileSize := IniFile.ReadInt64('Main', 'CloudMaxFileSize', CLOUD_MAX_FILESIZE_DEFAULT);
	FChunkOverwriteMode := IniFile.ReadInteger('Main', 'ChunkOverwriteMode', 0);
	FDeleteFailOnUploadMode := IniFile.ReadInteger('Main', 'DeleteFailOnUploadMode', 0);
	FOverwriteLocalMode := IniFile.ReadInteger('Main', 'OverwriteLocalMode', 0);
	FOperationErrorMode := IniFile.ReadInteger('Main', 'OperationErrorMode', 0);
	FRetryAttempts := IniFile.ReadInteger('Main', 'RetryAttempts', 1);
	FAttemptWait := IniFile.ReadInteger('Main', 'AttemptWait', 1000);
	FConnectionSettings.ProxySettings.ProxyType := IniFile.ReadInteger('Main', 'ProxyType', ProxyNone);
	FConnectionSettings.ProxySettings.Server := IniFile.ReadString('Main', 'ProxyServer', EmptyWideStr);
	FConnectionSettings.ProxySettings.Port := IniFile.ReadInteger('Main', 'ProxyPort', 0);
	FConnectionSettings.ProxySettings.user := IniFile.ReadString('Main', 'ProxyUser', EmptyWideStr);
	FConnectionSettings.ProxySettings.use_tc_password_manager := IniFile.ReadBool('Main', 'ProxyTCPwdMngr', false);
	FConnectionSettings.ProxySettings.password := IniFile.ReadString('Main', 'ProxyPassword', EmptyWideStr);
	FConnectionSettings.UserAgent := IniFile.ReadString('Main', 'UserAgent', DEFAULT_USERAGENT);
	FDownloadLinksEncode := IniFile.ReadBool('Main', 'DownloadLinksEncode', True);
	FAutoUpdateDownloadListing := IniFile.ReadBool('Main', 'AutoUpdateDownloadListing', True);
	FShowTrashFolders := IniFile.ReadBool('Main', 'ShowTrashFolders', True);
	FShowSharedFolders := IniFile.ReadBool('Main', 'ShowSharedFolders', True);
	FShowInvitesFolders := IniFile.ReadBool('Main', 'ShowInvitesFolders', True);
	FLogLevel := IniFile.ReadInteger('Main', 'LogLevel', LOG_LEVEL_CONNECT + LOG_LEVEL_FILE_OPERATION + LOG_LEVEL_DETAIL + LOG_LEVEL_WARNING + LOG_LEVEL_ERROR);
	FPrecalculateHash := IniFile.ReadBool('Main', 'PrecalculateHash', True);
	FForcePrecalculateSize := IniFile.ReadInt64('Main', 'ForcePrecalculateSize', CLOUD_PRECALCULATE_LIMIT_DEFAULT);
	FCheckCRC := IniFile.ReadBool('Main', 'CheckCRC', True);
	IniFile.Destroy;
end;

procedure TMRCSettings.Save;
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	IniFile.WriteInteger('Main', 'IniPath', IniPath);
	IniFile.WriteBool('Main', 'LoadSSLDLLOnlyFromPluginDir', FLoadSSLDLLOnlyFromPluginDir);
	IniFile.WriteBool('Main', 'PreserveFileTime', FPreserveFileTime);
	IniFile.WriteBool('Main', 'DescriptionEnabled', FDescriptionEnabled);
	IniFile.WriteBool('Main', 'DescriptionEditorEnabled', FDescriptionEditorEnabled);
	IniFile.WriteBool('Main', 'DescriptionCopyToCloud', FDescriptionCopyToCloud);
	IniFile.WriteBool('Main', 'DescriptionCopyFromCloud', FDescriptionCopyFromCloud);
	IniFile.WriteBool('Main', 'DescriptionTrackCloudFS', FDescriptionTrackCloudFS);
	IniFile.WriteString('Main', 'DescriptionFileName', FDescriptionFileName);
	IniFile.WriteInteger('Main', 'CopyBetweenAccountsMode', FCopyBetweenAccountsMode);
	IniFile.WriteBool('Main', 'DisableMultiThreading', FDisableMultiThreading);
	IniFile.WriteBool('Main', 'LogUserSpace', FLogUserSpace);
	IniFile.WriteInteger('Main', 'IconsMode', FIconsMode);
	IniFile.WriteInteger('Main', 'SocketTimeout', FConnectionSettings.SocketTimeout);
	IniFile.WriteInteger('Main', 'UploadBPS', FConnectionSettings.UploadBPS);
	IniFile.WriteInteger('Main', 'DownloadBPS', FConnectionSettings.DownloadBPS);
	IniFile.WriteInt64('Main', 'CloudMaxFileSize', FCloudMaxFileSize);
	IniFile.WriteInteger('Main', 'ChunkOverwriteMode', FChunkOverwriteMode);
	IniFile.WriteInteger('Main', 'DeleteFailOnUploadMode', FDeleteFailOnUploadMode);
	IniFile.WriteInteger('Main', 'OverwriteLocalMode', FOverwriteLocalMode);
	IniFile.WriteInteger('Main', 'OperationErrorMode', FOperationErrorMode);
	IniFile.WriteInteger('Main', 'RetryAttempts', FRetryAttempts);
	IniFile.WriteInteger('Main', 'AttemptWait', FAttemptWait);
	IniFile.WriteInteger('Main', 'ProxyType', FConnectionSettings.ProxySettings.ProxyType);
	IniFile.WriteString('Main', 'ProxyServer', FConnectionSettings.ProxySettings.Server);
	IniFile.WriteInteger('Main', 'ProxyPort', FConnectionSettings.ProxySettings.Port);
	IniFile.WriteString('Main', 'ProxyUser', FConnectionSettings.ProxySettings.user);
	IniFile.WriteBool('Main', 'ProxyTCPwdMngr', FConnectionSettings.ProxySettings.use_tc_password_manager);
	IniFile.WriteString('Main', 'ProxyPassword', FConnectionSettings.ProxySettings.password);
	IniFile.WriteString('Main', 'UserAgent', FConnectionSettings.UserAgent);
	IniFile.WriteBool('Main', 'DownloadLinksEncode', FDownloadLinksEncode);
	IniFile.WriteBool('Main', 'AutoUpdateDownloadListing', FAutoUpdateDownloadListing);
	IniFile.WriteBool('Main', 'ShowTrashFolders', FShowTrashFolders);
	IniFile.WriteBool('Main', 'ShowSharedFolders', FShowSharedFolders);
	IniFile.WriteBool('Main', 'ShowInvitesFolders', FShowInvitesFolders);
	IniFile.WriteInteger('Main', 'LogLevel', FLogLevel);
	IniFile.WriteBool('Main', 'PrecalculateHash', FPrecalculateHash);
	IniFile.WriteInt64('Main', 'ForcePrecalculateSize', FForcePrecalculateSize);
	IniFile.WriteBool('Main', 'CheckCRC', FCheckCRC);
	IniFile.Destroy;
end;

procedure TMRCSettings.SetSettingValue(OptionName: WideString; OptionValue: Variant);
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

end.

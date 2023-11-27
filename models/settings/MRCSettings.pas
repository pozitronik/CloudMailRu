unit MRCSettings;

interface

uses
	IniFiles,
	SysUtils,
	Variants,
	IOUtils,
	FileHelper,
	PathHelper,
	WindowsHelper,
	Windows,
	SETTINGS_CONSTANTS,
	CMRStrings,
	CMRConstants,
	ConnectionSettings;

type

	TMRCSettings = class
		ConnectionSettings: TConnectionSettings;
		IniPath: integer;
		LoadSSLDLLOnlyFromPluginDir: boolean;
		PreserveFileTime: boolean;
		DescriptionEnabled: boolean;
		DescriptionEditorEnabled: boolean;
		DescriptionCopyToCloud: boolean;
		DescriptionCopyFromCloud: boolean;
		DescriptionTrackCloudFS: boolean;
		DescriptionFileName_: WideString; //contains the raw string, real filename can be received via DescriptionFileName property
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
	private
		ApplicationPath: WideString; // the directory of the current binary file
		IniFilePath: WideString;
		IniFileDir: WideString; // the directory where the currently used ini files (global+accounts) are
		function GetDescriptionFileName: WideString;
		function GetAccountsIniFileName: WideString;
	public
		property IniDir: WideString read IniFileDir;
		property DescriptionFileName: WideString read GetDescriptionFileName write DescriptionFileName_;
		property PluginPath: WideString read ApplicationPath;
		property AccountsIniFileName: WideString read GetAccountsIniFileName; //Path to the accounts config file

		constructor Create(); overload; //finds the settings file by itself
		constructor Create(IniFilePath: WideString); overload;
		destructor Destroy; override;
		procedure Refresh();

		procedure SetSettingValue(OptionName: WideString; OptionValue: Variant);

	end;

implementation

{TMRCSettings}

constructor TMRCSettings.Create(IniFilePath: WideString);
begin
	self.IniFilePath := IniFilePath;
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
	Refresh();

end;

destructor TMRCSettings.Destroy;
begin

	inherited;
end;

function TMRCSettings.GetAccountsIniFileName: WideString;
begin
	result := self.IniDir + ACCOUNTS_CONFIG_FILE_NAME;
end;

function TMRCSettings.GetDescriptionFileName: WideString;
begin
	result := self.DescriptionFileName_;
	if TPath.HasValidFileNameChars(result, false) then
		exit;
	exit('descript.ion');
end;

procedure TMRCSettings.Refresh;
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	self.IniPath := IniFile.ReadInteger('Main', 'IniPath', 0);
	self.LoadSSLDLLOnlyFromPluginDir := IniFile.ReadBool('Main', 'LoadSSLDLLOnlyFromPluginDir', false);
	self.PreserveFileTime := IniFile.ReadBool('Main', 'PreserveFileTime', false);
	self.DescriptionEnabled := IniFile.ReadBool('Main', 'DescriptionEnabled', false);
	self.DescriptionEditorEnabled := IniFile.ReadBool('Main', 'DescriptionEditorEnabled', false);
	self.DescriptionCopyToCloud := IniFile.ReadBool('Main', 'DescriptionCopyToCloud', false);
	self.DescriptionCopyFromCloud := IniFile.ReadBool('Main', 'DescriptionCopyFromCloud', false);
	self.DescriptionTrackCloudFS := IniFile.ReadBool('Main', 'DescriptionTrackCloudFS', false);
	self.DescriptionFileName := IniFile.ReadString('Main', 'DescriptionFileName', 'descript.ion');
	self.CopyBetweenAccountsMode := IniFile.ReadInteger('Main', 'CopyBetweenAccountsMode', CopyBetweenAccountsModeDisabled);

	self.DisableMultiThreading := IniFile.ReadBool('Main', 'DisableMultiThreading', false);
	self.LogUserSpace := IniFile.ReadBool('Main', 'LogUserSpace', true);
	self.IconsMode := IniFile.ReadInteger('Main', 'IconsMode', 0);
	self.ConnectionSettings.SocketTimeout := IniFile.ReadInteger('Main', 'SocketTimeout', -1);
	self.ConnectionSettings.UploadBPS := IniFile.ReadInteger('Main', 'UploadBPS', -1);
	self.ConnectionSettings.DownloadBPS := IniFile.ReadInteger('Main', 'DownloadBPS', -1);
	self.CloudMaxFileSize := IniFile.ReadInt64('Main', 'CloudMaxFileSize', CLOUD_MAX_FILESIZE_DEFAULT);
	self.ChunkOverwriteMode := IniFile.ReadInteger('Main', 'ChunkOverwriteMode', 0);
	self.DeleteFailOnUploadMode := IniFile.ReadInteger('Main', 'DeleteFailOnUploadMode', 0);
	self.OverwriteLocalMode := IniFile.ReadInteger('Main', 'OverwriteLocalMode', 0);
	self.OperationErrorMode := IniFile.ReadInteger('Main', 'OperationErrorMode', 0);
	self.RetryAttempts := IniFile.ReadInteger('Main', 'RetryAttempts', 1);
	self.AttemptWait := IniFile.ReadInteger('Main', 'AttemptWait', 1000);
	self.ConnectionSettings.ProxySettings.ProxyType := IniFile.ReadInteger('Main', 'ProxyType', ProxyNone);
	self.ConnectionSettings.ProxySettings.Server := IniFile.ReadString('Main', 'ProxyServer', EmptyWideStr);
	self.ConnectionSettings.ProxySettings.Port := IniFile.ReadInteger('Main', 'ProxyPort', 0);
	self.ConnectionSettings.ProxySettings.user := IniFile.ReadString('Main', 'ProxyUser', EmptyWideStr);
	self.ConnectionSettings.ProxySettings.use_tc_password_manager := IniFile.ReadBool('Main', 'ProxyTCPwdMngr', false);
	self.ConnectionSettings.ProxySettings.password := IniFile.ReadString('Main', 'ProxyPassword', EmptyWideStr);
	self.ConnectionSettings.UserAgent := IniFile.ReadString('Main', 'UserAgent', DEFAULT_USERAGENT);
	self.DownloadLinksEncode := IniFile.ReadBool('Main', 'DownloadLinksEncode', true);
	self.AutoUpdateDownloadListing := IniFile.ReadBool('Main', 'AutoUpdateDownloadListing', true);
	self.ShowTrashFolders := IniFile.ReadBool('Main', 'ShowTrashFolders', true);
	self.ShowSharedFolders := IniFile.ReadBool('Main', 'ShowSharedFolders', true);
	self.ShowInvitesFolders := IniFile.ReadBool('Main', 'ShowInvitesFolders', true);
	self.LogLevel := IniFile.ReadInteger('Main', 'LogLevel', LOG_LEVEL_CONNECT + LOG_LEVEL_FILE_OPERATION + LOG_LEVEL_DETAIL + LOG_LEVEL_WARNING + LOG_LEVEL_ERROR);
	self.PrecalculateHash := IniFile.ReadBool('Main', 'PrecalculateHash', true);
	self.ForcePrecalculateSize := IniFile.ReadInt64('Main', 'ForcePrecalculateSize', CLOUD_PRECALCULATE_LIMIT_DEFAULT);
	self.CheckCRC := IniFile.ReadBool('Main', 'CheckCRC', true);
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

unit PluginSettings;

interface

uses
	SettingsConstants,
	ConnectionSettings,
	AccountSettings;

type
	TPluginSettings = record
		ConnectionSettings: TConnectionSettings;
		IniDir: TIniDirTypes; {See INI_DIR_* constants}
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
		HashCalculatorStrategy: integer;
	private
		FIniFilePath: WideString;
		FAccountsIniFilePath: WideString;
		function GetEnabledVirtualTypes: EVirtualType;
	public
		property IniFilePath: WideString read FIniFilePath write FIniFilePath;
		property AccountsIniFilePath: WideString read FAccountsIniFilePath write FAccountsIniFilePath;
		property EnabledVirtualTypes: EVirtualType read GetEnabledVirtualTypes;
	end;

implementation

{TPluginSettings}

function TPluginSettings.GetEnabledVirtualTypes: EVirtualType;
begin
	result := [];
	if ShowTrashFolders then
		result := result + [VTTrash];
	if ShowSharedFolders then
		result := result + [VTShared];
	if ShowInvitesFolders then
		result := result + [VTInvites];
end;

end.

unit PluginSettings;

interface

uses
	ConnectionSettings,
	AccountSettings;

type
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
	private
		FIniFilePath: WideString;
		function GetEnabledVirtualTypes: EVirtualType;
	public
		property IniFilePath: WideString read FIniFilePath;
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

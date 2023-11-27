unit AbstractPluginSettings;

{This base class contains and handles only the set of plugin parameters variables}
interface

uses
	IOUtils,
	ConnectionSettings;

type

	TAbstractPluginSettings = class abstract
	protected
		{Parameters}
		FConnectionSettings: TConnectionSettings;
		FIniPath: integer;
		FLoadSSLDLLOnlyFromPluginDir: boolean;
		FPreserveFileTime: boolean;
		FDescriptionEnabled: boolean;
		FDescriptionEditorEnabled: boolean;
		FDescriptionCopyToCloud: boolean;
		FDescriptionCopyFromCloud: boolean;
		FDescriptionTrackCloudFS: boolean;
		FDescriptionFileName: WideString;
		FCopyBetweenAccountsMode: integer;
		FCloudMaxFileSize: int64;
		FChunkOverwriteMode: integer;
		FDeleteFailOnUploadMode: integer;
		FOperationErrorMode: integer;
		FRetryAttempts: integer;
		FAttemptWait: integer;
		FOverwriteLocalMode: integer;
		FDisableMultiThreading: boolean;
		FLogUserSpace: boolean;
		FIconsMode: integer;
		FDownloadLinksEncode: boolean;
		FAutoUpdateDownloadListing: boolean;
		FShowTrashFolders: boolean;
		FShowSharedFolders: boolean;
		FShowInvitesFolders: boolean;
		FLogLevel: integer;
		FPrecalculateHash: boolean;
		FForcePrecalculateSize: int64;
		FCheckCRC: boolean;
		FSaveOnChange: boolean; // save options to the file when any option changes
	private
		function GetDescriptionFileName: WideString;
		procedure SetAttemptWait(const Value: integer);
		procedure SetAutoUpdateDownloadListing(const Value: boolean);
		procedure SetCheckCRC(const Value: boolean);
		procedure SetChunkOverwriteMode(const Value: integer);
		procedure SetCloudMaxFileSize(const Value: int64);
		procedure SetCopyBetweenAccountsMode(const Value: integer);
		procedure SetDeleteFailOnUploadMode(const Value: integer);
		procedure SetDescriptionCopyFromCloud(const Value: boolean);
		procedure SetDescriptionCopyToCloud(const Value: boolean);
		procedure SetDescriptionEditorEnabled(const Value: boolean);
		procedure SetDescriptionEnabled(const Value: boolean);
		procedure SetDescriptionFileName(const Value: WideString);
		procedure SetDescriptionTrackCloudFS(const Value: boolean);
		procedure SetDisableMultiThreading(const Value: boolean);
		procedure SetDownloadLinksEncode(const Value: boolean);
		procedure SetForcePrecalculateSize(const Value: int64);
		procedure SetIconsMode(const Value: integer);
		procedure SetIniPath(const Value: integer);
		procedure SetLoadSSLDLLOnlyFromPluginDir(const Value: boolean);
		procedure SetLogLevel(const Value: integer);
		procedure SetLogUserSpace(const Value: boolean);
		procedure SetOperationErrorMode(const Value: integer);
		procedure SetOverwriteLocalMode(const Value: integer);
		procedure SetPrecalculateHash(const Value: boolean);
		procedure SetPreserveFileTime(const Value: boolean);
		procedure SetRetryAttempts(const Value: integer);
		procedure SetShowInvitesFolders(const Value: boolean);
		procedure SetShowSharedFolders(const Value: boolean);
		procedure SetShowTrashFolders(const Value: boolean);
		procedure SetConnectionSettings(const Value: TConnectionSettings);
	public
		{Properties, reflecting every parameter}
		property ConnectionSettings: TConnectionSettings read FConnectionSettings write SetConnectionSettings;
		property IniPath: integer read FIniPath write SetIniPath;
		property LoadSSLDLLOnlyFromPluginDir: boolean read FLoadSSLDLLOnlyFromPluginDir write SetLoadSSLDLLOnlyFromPluginDir;
		property PreserveFileTime: boolean read FPreserveFileTime write SetPreserveFileTime;
		property DescriptionEnabled: boolean read FDescriptionEnabled write SetDescriptionEnabled;
		property DescriptionEditorEnabled: boolean read FDescriptionEditorEnabled write SetDescriptionEditorEnabled;
		property DescriptionCopyToCloud: boolean read FDescriptionCopyToCloud write SetDescriptionCopyToCloud;
		property DescriptionCopyFromCloud: boolean read FDescriptionCopyFromCloud write SetDescriptionCopyFromCloud;
		property DescriptionTrackCloudFS: boolean read FDescriptionTrackCloudFS write SetDescriptionTrackCloudFS;
		property DescriptionFileName: WideString read GetDescriptionFileName write SetDescriptionFileName;
		property CopyBetweenAccountsMode: integer read FCopyBetweenAccountsMode write SetCopyBetweenAccountsMode;
		property CloudMaxFileSize: int64 read FCloudMaxFileSize write SetCloudMaxFileSize;
		property ChunkOverwriteMode: integer read FChunkOverwriteMode write SetChunkOverwriteMode;
		property DeleteFailOnUploadMode: integer read FDeleteFailOnUploadMode write SetDeleteFailOnUploadMode;
		property OperationErrorMode: integer read FOperationErrorMode write SetOperationErrorMode;
		property RetryAttempts: integer read FRetryAttempts write SetRetryAttempts;
		property AttemptWait: integer read FAttemptWait write SetAttemptWait;
		property OverwriteLocalMode: integer read FOverwriteLocalMode write SetOverwriteLocalMode;
		property DisableMultiThreading: boolean read FDisableMultiThreading write SetDisableMultiThreading;
		property LogUserSpace: boolean read FLogUserSpace write SetLogUserSpace;
		property IconsMode: integer read FIconsMode write SetIconsMode;
		property DownloadLinksEncode: boolean read FDownloadLinksEncode write SetDownloadLinksEncode;
		property AutoUpdateDownloadListing: boolean read FAutoUpdateDownloadListing write SetAutoUpdateDownloadListing;
		property ShowTrashFolders: boolean read FShowTrashFolders write SetShowTrashFolders;
		property ShowSharedFolders: boolean read FShowSharedFolders write SetShowSharedFolders;
		property ShowInvitesFolders: boolean read FShowInvitesFolders write SetShowInvitesFolders;
		property LogLevel: integer read FLogLevel write SetLogLevel;
		property PrecalculateHash: boolean read FPrecalculateHash write SetPrecalculateHash;
		property ForcePrecalculateSize: int64 read FForcePrecalculateSize write SetForcePrecalculateSize;
		property CheckCRC: boolean read FCheckCRC write SetCheckCRC;

		property SaveOnChange: boolean read FSaveOnChange write FSaveOnChange;
		procedure SetSettingValue(OptionName: WideString; OptionValue: Variant); virtual; abstract;
		procedure Save(); virtual; abstract;
	end;

implementation

function TAbstractPluginSettings.GetDescriptionFileName: WideString;
begin
	result := self.FDescriptionFileName;
	if TPath.HasValidFileNameChars(result, false) then
		exit;
	exit('descript.ion');
end;

procedure TAbstractPluginSettings.SetAttemptWait(const Value: integer);
begin
	FAttemptWait := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetAutoUpdateDownloadListing(const Value: boolean);
begin
	FAutoUpdateDownloadListing := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetCheckCRC(const Value: boolean);
begin
	FCheckCRC := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetChunkOverwriteMode(const Value: integer);
begin
	FChunkOverwriteMode := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetCloudMaxFileSize(const Value: int64);
begin
	FCloudMaxFileSize := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetConnectionSettings(const Value: TConnectionSettings);
begin
	FConnectionSettings := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetCopyBetweenAccountsMode(const Value: integer);
begin
	FCopyBetweenAccountsMode := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetDeleteFailOnUploadMode(const Value: integer);
begin
	FDeleteFailOnUploadMode := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetDescriptionCopyFromCloud(const Value: boolean);
begin
	FDescriptionCopyFromCloud := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetDescriptionCopyToCloud(const Value: boolean);
begin
	FDescriptionCopyToCloud := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetDescriptionEditorEnabled(const Value: boolean);
begin
	FDescriptionEditorEnabled := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetDescriptionEnabled(const Value: boolean);
begin
	FDescriptionEnabled := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetDescriptionFileName(const Value: WideString);
begin
	FDescriptionFileName := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetDescriptionTrackCloudFS(const Value: boolean);
begin
	FDescriptionTrackCloudFS := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetDisableMultiThreading(const Value: boolean);
begin
	FDisableMultiThreading := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetDownloadLinksEncode(const Value: boolean);
begin
	FDownloadLinksEncode := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetForcePrecalculateSize(const Value: int64);
begin
	FForcePrecalculateSize := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetIconsMode(const Value: integer);
begin
	FIconsMode := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetIniPath(const Value: integer);
begin
	FIniPath := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetLoadSSLDLLOnlyFromPluginDir(const Value: boolean);
begin
	FLoadSSLDLLOnlyFromPluginDir := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetLogLevel(const Value: integer);
begin
	FLogLevel := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetLogUserSpace(const Value: boolean);
begin
	FLogUserSpace := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetOperationErrorMode(const Value: integer);
begin
	FOperationErrorMode := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetOverwriteLocalMode(const Value: integer);
begin
	FOverwriteLocalMode := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetPrecalculateHash(const Value: boolean);
begin
	FPrecalculateHash := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetPreserveFileTime(const Value: boolean);
begin
	FPreserveFileTime := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetRetryAttempts(const Value: integer);
begin
	FRetryAttempts := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetShowInvitesFolders(const Value: boolean);
begin
	FShowInvitesFolders := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetShowSharedFolders(const Value: boolean);
begin
	FShowSharedFolders := Value;
	if FSaveOnChange then
		Save();
end;

procedure TAbstractPluginSettings.SetShowTrashFolders(const Value: boolean);
begin
	FShowTrashFolders := Value;
	if FSaveOnChange then
		Save();
end;

end.

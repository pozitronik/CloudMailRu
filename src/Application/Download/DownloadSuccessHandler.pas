unit DownloadSuccessHandler;

{Post-download success handler.
 Performs CRC verification, timestamp preservation, move cleanup,
 progress reporting, logging, and description sync after successful download.}

interface

uses
	Windows,
	SysUtils,
	DateUtils,
	IPluginSettingsManagerInterface,
	ILoggerInterface,
	IProgressInterface,
	DescriptionSyncGuard,
	PLUGIN_TYPES,
	CMRConstants,
	RealPath,
	CMRDirItem,
	CloudMailRu,
	FileHelper,
	SystemHelper;

type
	{Context for download success handling}
	TDownloadContext = record
		RemotePath: TRealPath;
		LocalName: WideString;
		RemoteName: WideString;
		CopyFlags: Integer;
		ResultHash: WideString;  {Hash calculated during download, empty if not calculated}
		Item: TCMRDirItem;       {Directory item with expected hash and mtime}
		Cloud: TCloudMailRu;     {Cloud connection for move operations}
	end;

	IDownloadSuccessHandler = interface
		['{E7F8A9B0-C1D2-3E4F-5A6B-7C8D9E0F1A2B}']

		{Handles all post-download success operations.
		 @param Context Download context with all required data
		 @return FS_FILE_OK on success, FS_FILE_READERROR if CRC mismatch}
		function HandleSuccess(const Context: TDownloadContext): Integer;
	end;

	{Null implementation for testing - always succeeds}
	TNullDownloadSuccessHandler = class(TInterfacedObject, IDownloadSuccessHandler)
	public
		function HandleSuccess(const Context: TDownloadContext): Integer;
	end;

	TDownloadSuccessHandler = class(TInterfacedObject, IDownloadSuccessHandler)
	private
		FSettings: IPluginSettingsManager;
		FLogger: ILogger;
		FProgress: IProgress;
		FDescriptionSyncGuard: IDescriptionSyncGuard;

		{Verifies downloaded file hash matches expected hash.
		 @return True if hash matches or verification disabled/skipped}
		function VerifyCRC(const ResultHash, ExpectedHash: WideString): Boolean;

		{Sets file modification time from Unix timestamp}
		procedure PreserveFileTime(const LocalName: WideString; UnixTime: Int64);

		{Handles move operation - deletes remote file after download}
		procedure HandleMoveOperation(const Context: TDownloadContext);

		{Reports completion progress and logs transfer}
		procedure ReportCompletion(const LocalName, RemoteName: WideString);
	public
		constructor Create(
			Settings: IPluginSettingsManager;
			Logger: ILogger;
			Progress: IProgress;
			DescriptionSyncGuard: IDescriptionSyncGuard
		);

		function HandleSuccess(const Context: TDownloadContext): Integer;
	end;

implementation

{TNullDownloadSuccessHandler}

function TNullDownloadSuccessHandler.HandleSuccess(const Context: TDownloadContext): Integer;
begin
	Result := FS_FILE_OK;
end;

{TDownloadSuccessHandler}

constructor TDownloadSuccessHandler.Create(
	Settings: IPluginSettingsManager;
	Logger: ILogger;
	Progress: IProgress;
	DescriptionSyncGuard: IDescriptionSyncGuard
);
begin
	inherited Create;
	FSettings := Settings;
	FLogger := Logger;
	FProgress := Progress;
	FDescriptionSyncGuard := DescriptionSyncGuard;
end;

function TDownloadSuccessHandler.VerifyCRC(const ResultHash, ExpectedHash: WideString): Boolean;
begin
	{Hash verification: fail if both hashes present and don't match}
	Result := not((ResultHash <> EmptyWideStr) and (ExpectedHash <> ResultHash));
end;

procedure TDownloadSuccessHandler.PreserveFileTime(const LocalName: WideString; UnixTime: Int64);
begin
	if UnixTime <> 0 then
		SetAllFileTime(ExpandUNCFileName(LocalName), DateTimeToFileTime(UnixToDateTime(UnixTime)));
end;

procedure TDownloadSuccessHandler.HandleMoveOperation(const Context: TDownloadContext);
begin
	if Assigned(Context.Cloud) then
		Context.Cloud.deleteFile(Context.RemotePath.Path);
	FDescriptionSyncGuard.OnFileDeleted(Context.RemotePath, Context.Cloud);
end;

procedure TDownloadSuccessHandler.ReportCompletion(const LocalName, RemoteName: WideString);
begin
	FProgress.Progress(LocalName, RemoteName, 100);
	FLogger.Log(LOG_LEVEL_FILE_OPERATION, MSGTYPE_TRANSFERCOMPLETE, '%s -> %s', [RemoteName, LocalName]);
end;

function TDownloadSuccessHandler.HandleSuccess(const Context: TDownloadContext): Integer;
begin
	Result := FS_FILE_OK;

	{CRC verification if enabled}
	if FSettings.GetSettings.CheckCRC then
	begin
		if not VerifyCRC(Context.ResultHash, Context.Item.hash) then
			Exit(FS_FILE_READERROR);
	end;

	{Preserve file modification time if enabled}
	if FSettings.GetSettings.PreserveFileTime then
		PreserveFileTime(Context.LocalName, Context.Item.mtime);

	{Delete remote file if this is a move operation}
	if CheckFlag(FS_COPYFLAGS_MOVE, Context.CopyFlags) then
		HandleMoveOperation(Context);

	{Report progress and log completion}
	ReportCompletion(Context.LocalName, Context.RemoteName);

	{Sync description from cloud to local}
	FDescriptionSyncGuard.OnFileDownloaded(Context.RemotePath, Context.LocalName, Context.Cloud);
end;

end.

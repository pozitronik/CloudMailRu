unit CloudFileDownloader;

interface

uses
	System.Classes,
	System.SysUtils,
	CloudConstants,
	CloudOAuth,
	CloudOperationResult,
	WFXTypes,
	LanguageStrings,
	TCLogger,
	TCProgress,
	TCRequest,
	FileCipher,
	PathHelper,
	StringHelper,
	RealPath,
	CloudShardManager,
	CloudHashCalculator,
	CloudHTTP,
	CloudErrorMapper,
	CloudContext,
	WindowsFileSystem,
	DCPbase64;

type
	{Interface for cloud file download operations}
	ICloudFileDownloader = interface
		['{D7A8E5F2-3B4C-4D6E-9F1A-2C5B8E0F4D7A}']
		{Download file from cloud to local path.
			Returns FS_FILE_OK on success, or appropriate error code on failure.
			ResultHash receives the cloud hash of downloaded file.}
		function Download(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean = True): Integer;
		{Get URL for shared/public file download.}
		function GetSharedFileUrl(RemotePath: WideString; ShardType: WideString = SHARD_TYPE_DEFAULT): WideString;
	end;

	{File download service - handles both regular and shared account downloads}
	TCloudFileDownloader = class(TInterfacedObject, ICloudFileDownloader)
	private
		FContext: ICloudContext;
		FShardManager: ICloudShardManager;
		FHashCalculator: ICloudHashCalculator;
		FCipher: ICipher;
		FFileSystem: IFileSystem;
		FLogger: ILogger;
		FProgress: IProgress;
		FRequest: IRequest;
		FDoCryptFiles: Boolean;
		{Internal download methods}
		function DownloadRegular(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
		function DownloadShared(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
	public
		constructor Create(Context: ICloudContext; ShardManager: ICloudShardManager; HashCalculator: ICloudHashCalculator; Cipher: ICipher; FileSystem: IFileSystem; Logger: ILogger; Progress: IProgress; Request: IRequest; DoCryptFiles: Boolean);

		{ICloudFileDownloader}
		function Download(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean = True): Integer;
		function GetSharedFileUrl(RemotePath: WideString; ShardType: WideString = SHARD_TYPE_DEFAULT): WideString;
	end;

implementation

uses
	Winapi.Windows,
	ChunkedFileStream;

{TCloudFileDownloader}

constructor TCloudFileDownloader.Create(Context: ICloudContext; ShardManager: ICloudShardManager; HashCalculator: ICloudHashCalculator; Cipher: ICipher; FileSystem: IFileSystem; Logger: ILogger; Progress: IProgress; Request: IRequest; DoCryptFiles: Boolean);
begin
	inherited Create;
	FContext := Context;
	FShardManager := ShardManager;
	FHashCalculator := HashCalculator;
	FCipher := Cipher;
	FFileSystem := FileSystem;
	FLogger := Logger;
	FProgress := Progress;
	FRequest := Request;
	FDoCryptFiles := DoCryptFiles;
end;

function TCloudFileDownloader.Download(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
begin
	FContext.GetHTTP.SetProgressNames(RemotePath, LocalPath);
	if FContext.IsPublicAccount then
		Result := DownloadShared(RemotePath, LocalPath, ResultHash, LogErrors)
	else
		Result := DownloadRegular(RemotePath, LocalPath, ResultHash, LogErrors);
end;

function TCloudFileDownloader.DownloadRegular(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
var
	FileStream: TBufferedFileStream;
	URL: WideString;
	MemoryStream: TMemoryStream;
	DecryptingStream: TStream;
	DispatcherResponse: WideString;
	Progress: Boolean;
	DownloadShard: WideString;
	OAuthToken: TCloudOAuth;
	SavedUserAgent: string;
begin
	Result := FS_FILE_NOTSUPPORTED;
	DownloadShard := FShardManager.GetDownloadShard;
	if DownloadShard = EmptyWideStr then
	begin
		FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, UNDEFINED_DOWNLOAD_SHARD);
		if FShardManager.HasDownloadOverride then
		begin
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_DETAILS, SHARD_OVERRIDDEN);
			DownloadShard := FShardManager.GetDownloadShardOverride;
			FShardManager.SetDownloadShard(DownloadShard);
		end else begin
			{OAuth uses different dispatcher endpoint that returns plain text URL}
			Progress := False;
			OAuthToken := FContext.GetOAuthToken;
			if FContext.GetHTTP.GetPage(Format('%s/d?token=%s', [OAUTH_DISPATCHER_URL, OAuthToken.access_token]), DispatcherResponse, Progress) then
			begin
				{Response format: "URL IP COUNT", extract the URL (first word)}
				DownloadShard := Trim(Copy(DispatcherResponse, 1, Pos(' ', DispatcherResponse) - 1));
				FShardManager.SetDownloadShard(DownloadShard);
			end
			else
				Exit;
		end;
	end;
	try
		FileStream := TBufferedFileStream.Create(GetUNCFilePath(LocalPath), fmCreate);
	except
		on E: Exception do
		begin
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, E.Message);
			Exit(FS_FILE_WRITEERROR);
		end;
	end;

	{OAuth download endpoint blocks browser-like User-Agents (Mozilla/*)}
	SavedUserAgent := FContext.GetHTTP.HTTP.Request.UserAgent;
	FContext.GetHTTP.HTTP.Request.UserAgent := OAUTH_CLIENT_ID;
	try
		OAuthToken := FContext.GetOAuthToken;
		if FDoCryptFiles then //Загрузка файла в память, дешифрация в файл
		begin
			MemoryStream := TMemoryStream.Create;
			try
				{OAuth requires client_id and token parameters for download authentication}
				URL := Format('%s%s?client_id=%s&token=%s', [DownloadShard, PathToUrl(RemotePath, False), OAUTH_CLIENT_ID, OAuthToken.access_token]);
				Result := FContext.GetHTTP.GetFile(URL, MemoryStream, LogErrors);

				if (CLOUD_ERROR_TOKEN_OUTDATED = Result) and FContext.RefreshCSRFToken then
					Exit(DownloadRegular(RemotePath, LocalPath, ResultHash, LogErrors));

				if Result in [FS_FILE_NOTSUPPORTED] then //this code returned on shard connection error
				begin
					FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s', [PREFIX_REDIRECTION_LIMIT, URL]);
					DownloadShard := EmptyWideStr;
					if (FRequest.Request(RT_MsgYesNo, REDIRECTION_LIMIT, TRY_ANOTHER_SHARD, EmptyWideStr, 0)) and (FShardManager.ResolveShard(DownloadShard, SHARD_TYPE_GET)) then
					begin
						FShardManager.SetDownloadShard(DownloadShard);
						Exit(DownloadRegular(RemotePath, LocalPath, ResultHash, LogErrors));
					end;
				end;

				if Result in [FS_FILE_OK] then
				begin
					ResultHash := FHashCalculator.CalculateHash(MemoryStream, CALCULATING_HASH);
					MemoryStream.Position := 0;
					{Use stream wrapper for decryption - processes in chunks with constant memory.
						Note: MemoryStream still buffers entire encrypted file due to HTTP download pattern.
						True streaming decryption would require a write-based wrapper for GetFile.}
					DecryptingStream := FCipher.GetDecryptingStream(MemoryStream);
					try
						FileStream.CopyFrom(DecryptingStream, DecryptingStream.Size);
					finally
						DecryptingStream.Free;
					end;
				end;
			finally
				MemoryStream.Free;
			end;
		end else begin
			{OAuth requires client_id and token parameters for download authentication}
			Result := FContext.GetHTTP.GetFile(Format('%s%s?client_id=%s&token=%s', [DownloadShard, PathToUrl(RemotePath, False), OAUTH_CLIENT_ID, OAuthToken.access_token]), FileStream, LogErrors);
			if (CLOUD_ERROR_TOKEN_OUTDATED = Result) and FContext.RefreshCSRFToken then
				Exit(DownloadRegular(RemotePath, LocalPath, ResultHash, LogErrors));
			if ((Result in [FS_FILE_OK]) and (EmptyWideStr = ResultHash)) then
				ResultHash := FHashCalculator.CalculateHash(FileStream, CALCULATING_HASH);
		end;

		FlushFileBuffers(FileStream.Handle);
	finally
		FContext.GetHTTP.HTTP.Request.UserAgent := SavedUserAgent;
		FileStream.Free;
	end;

	if not(Result in [FS_FILE_OK]) then
		FFileSystem.DeleteFile(GetUNCFilePath(LocalPath));
end;

{since 29.07.2022: изменена логика получения ссылок, см. issue #285. URL теперь всегда должны быть кодированы, иначе в некоторых случаях приходит 400}
function TCloudFileDownloader.GetSharedFileUrl(RemotePath: WideString; ShardType: WideString = SHARD_TYPE_DEFAULT): WideString;
var
	usedShard: WideString;
begin
	if not FContext.IsPublicAccount then
		Exit(EmptyWideStr); {Only valid for public accounts; private accounts use TempPublicCloud}

	if ShardType = SHARD_TYPE_DEFAULT then
		usedShard := FShardManager.GetPublicShard
	else if not FShardManager.ResolveShard(usedShard, ShardType) then
	begin
		FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_DETAILS, 'Failed to resolve shard: %s', [ShardType]);
		Exit(EmptyWideStr);
	end;

	Result := Format('%s%s%s', [IncludeSlash(usedShard), IncludeSlash(FContext.GetPublicLink), PathToUrl(RemotePath, True, True)]);
end;

function TCloudFileDownloader.DownloadShared(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
var
	FileStream: TBufferedFileStream;
begin
	Result := FS_FILE_NOTFOUND;
	if (FShardManager.GetPublicShard = EmptyWideStr) then
		Exit;
	try
		FileStream := TBufferedFileStream.Create(GetUNCFilePath(LocalPath), fmCreate);
	except
		on E: Exception do
		begin
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, E.Message);
			Exit(FS_FILE_WRITEERROR);
		end;
	end;
	try
		Result := FContext.GetHTTP.GetFile(GetSharedFileUrl(RemotePath), FileStream, LogErrors);
		if ((Result in [FS_FILE_OK]) and (EmptyWideStr = ResultHash)) then
			ResultHash := FHashCalculator.CalculateHash(FileStream, CALCULATING_HASH);
		FlushFileBuffers(FileStream.Handle);
	finally
		FileStream.Free;
	end;
	if Result <> FS_FILE_OK then
		FFileSystem.DeleteFile(GetUNCFilePath(LocalPath));
end;

end.

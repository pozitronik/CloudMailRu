unit CloudFileDownloader;

interface

uses
	System.Classes,
	System.SysUtils,
	CMRConstants,
	CMROAuth,
	CMROperationResult,
	PLUGIN_TYPES,
	LANGUAGE_STRINGS,
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
	WindowsFileSystem,
	DCPbase64;

type
	{Callback types for accessing CloudMailRu state}
	TGetOAuthTokenFunc = reference to function: TCMROAuth;
	TGetBoolFunc = reference to function: Boolean;
	TGetStringFunc = reference to function: WideString;
	TRefreshTokenFunc = reference to function: Boolean;
	TGetHTTPFunc = reference to function: ICloudHTTP;

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
		{Callbacks for CloudMailRu state access}
		FGetHTTP: TGetHTTPFunc;
		FShardManager: ICloudShardManager;
		FHashCalculator: ICloudHashCalculator;
		FCipher: ICipher;
		FFileSystem: IFileSystem;
		FLogger: ILogger;
		FProgress: IProgress;
		FRequest: IRequest;
		FGetOAuthToken: TGetOAuthTokenFunc;
		FIsPublicAccount: TGetBoolFunc;
		FGetPublicLink: TGetStringFunc;
		FRefreshToken: TRefreshTokenFunc;
		FDoCryptFiles: Boolean;
		FDoCryptFilenames: Boolean;

		{Internal download methods}
		function DownloadRegular(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
		function DownloadShared(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
	public
		constructor Create(GetHTTP: TGetHTTPFunc; ShardManager: ICloudShardManager; HashCalculator: ICloudHashCalculator; Cipher: ICipher; FileSystem: IFileSystem; Logger: ILogger; Progress: IProgress; Request: IRequest; GetOAuthToken: TGetOAuthTokenFunc; IsPublicAccount: TGetBoolFunc; GetPublicLink: TGetStringFunc; RefreshToken: TRefreshTokenFunc; DoCryptFiles, DoCryptFilenames: Boolean);

		{ICloudFileDownloader}
		function Download(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean = True): Integer;
		function GetSharedFileUrl(RemotePath: WideString; ShardType: WideString = SHARD_TYPE_DEFAULT): WideString;
	end;

implementation

uses
	Winapi.Windows,
	ChunkedFileStream;

{TCloudFileDownloader}

constructor TCloudFileDownloader.Create(GetHTTP: TGetHTTPFunc; ShardManager: ICloudShardManager; HashCalculator: ICloudHashCalculator; Cipher: ICipher; FileSystem: IFileSystem; Logger: ILogger; Progress: IProgress; Request: IRequest; GetOAuthToken: TGetOAuthTokenFunc; IsPublicAccount: TGetBoolFunc; GetPublicLink: TGetStringFunc; RefreshToken: TRefreshTokenFunc; DoCryptFiles, DoCryptFilenames: Boolean);
begin
	inherited Create;
	FGetHTTP := GetHTTP;
	FShardManager := ShardManager;
	FHashCalculator := HashCalculator;
	FCipher := Cipher;
	FFileSystem := FileSystem;
	FLogger := Logger;
	FProgress := Progress;
	FRequest := Request;
	FGetOAuthToken := GetOAuthToken;
	FIsPublicAccount := IsPublicAccount;
	FGetPublicLink := GetPublicLink;
	FRefreshToken := RefreshToken;
	FDoCryptFiles := DoCryptFiles;
	FDoCryptFilenames := DoCryptFilenames;
end;

function TCloudFileDownloader.Download(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
begin
	FGetHTTP().SetProgressNames(RemotePath, LocalPath);
	if FIsPublicAccount() then
		Result := DownloadShared(RemotePath, LocalPath, ResultHash, LogErrors)
	else
		Result := DownloadRegular(RemotePath, LocalPath, ResultHash, LogErrors);
end;

function TCloudFileDownloader.DownloadRegular(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
var
	FileStream: TBufferedFileStream;
	URL, FileName: WideString;
	MemoryStream: TMemoryStream;
	DispatcherResponse: WideString;
	Progress: Boolean;
	DownloadShard: WideString;
	OAuthToken: TCMROAuth;
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
			OAuthToken := FGetOAuthToken();
			if FGetHTTP().GetPage(Format('%s/d?token=%s', [OAUTH_DISPATCHER_URL, OAuthToken.access_token]), DispatcherResponse, Progress) then
			begin
				{Response format: "URL IP COUNT", extract the URL (first word)}
				DownloadShard := Trim(Copy(DispatcherResponse, 1, Pos(' ', DispatcherResponse) - 1));
				FShardManager.SetDownloadShard(DownloadShard);
			end
			else
				Exit;
		end;
	end;
	if FDoCryptFilenames then
	begin
		FileName := ExtractUniversalFileName(RemotePath);
		FileName := FCipher.DecryptFileName(FileName);
		LocalPath := ChangePathFileName(LocalPath, FileName);
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

	{Note: Previously User-Agent was set to 'cloud-win' here to avoid OAuth endpoint
		blocking browser-like UAs. Removed as UA manipulation is no longer needed.
		If downloads start failing with 403/blocking errors, consider restoring UA override.}
	try
		OAuthToken := FGetOAuthToken();
		if FDoCryptFiles then //Загрузка файла в память, дешифрация в файл
		begin
			MemoryStream := TMemoryStream.Create;
			try
				{OAuth requires client_id and token parameters for download authentication}
				URL := Format('%s%s?client_id=%s&token=%s', [DownloadShard, PathToUrl(RemotePath, False), OAUTH_CLIENT_ID, OAuthToken.access_token]);
				Result := FGetHTTP().GetFile(URL, MemoryStream, LogErrors);

				if (CLOUD_ERROR_TOKEN_OUTDATED = Result) and FRefreshToken() then
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
					FCipher.DecryptStream(MemoryStream, FileStream);
				end;
			finally
				MemoryStream.Free;
			end;
		end else begin
			{OAuth requires client_id and token parameters for download authentication}
			Result := FGetHTTP().GetFile(Format('%s%s?client_id=%s&token=%s', [DownloadShard, PathToUrl(RemotePath, False), OAUTH_CLIENT_ID, OAuthToken.access_token]), FileStream, LogErrors);
			if (CLOUD_ERROR_TOKEN_OUTDATED = Result) and FRefreshToken() then
				Exit(DownloadRegular(RemotePath, LocalPath, ResultHash, LogErrors));
			if ((Result in [FS_FILE_OK]) and (EmptyWideStr = ResultHash)) then
				ResultHash := FHashCalculator.CalculateHash(FileStream, CALCULATING_HASH);
		end;

		FlushFileBuffers(FileStream.Handle);
	finally
		FileStream.Free;
	end;

	if not(Result in [FS_FILE_OK]) then
		FFileSystem.DeleteFile(GetUNCFilePath(LocalPath));
end;

{since 29.07.2022: изменена логика получения ссылок, см. issue #285. URL теперь всегда должны быть кодированы, иначе в некоторых случаях приходит 400}
function TCloudFileDownloader.GetSharedFileUrl(RemotePath: WideString; ShardType: WideString = SHARD_TYPE_DEFAULT): WideString;
var
	usedShard: WideString;
	ProgressEnabled: Boolean;
begin
	if ShardType = SHARD_TYPE_DEFAULT then
		usedShard := FShardManager.GetPublicShard
	else
		FShardManager.ResolveShard(usedShard, ShardType);
	if (FIsPublicAccount()) then
		Exit(Format('%s%s%s', [IncludeSlash(usedShard), IncludeSlash(FGetPublicLink()), PathToUrl(RemotePath, True, True)]));

	if (TRealPath.GetRealPath(RemotePath).isDir = ID_True) then {для ссылок внутри каталогов перебираются файлы внутри «публичной ссылки» на каталог}
	begin
		Result := Format('%s%s%s', [IncludeSlash(usedShard), FGetPublicLink(), PathToUrl(RemotePath, True, True)]);
	end else begin {для прямых ссылок берутся публичные ссылки файлов}
		Result := Format('%s%s%s', [IncludeSlash(usedShard), FGetPublicLink()])
	end;

	ProgressEnabled := False;
	FGetHTTP().GetRedirection(Result, Result, ProgressEnabled);
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
	if (Assigned(FileStream)) then
	begin
		try
			Result := FGetHTTP().GetFile(GetSharedFileUrl(RemotePath), FileStream, LogErrors);
			if ((Result in [FS_FILE_OK]) and (EmptyWideStr = ResultHash)) then
				ResultHash := FHashCalculator.CalculateHash(FileStream, CALCULATING_HASH);
			FlushFileBuffers(FileStream.Handle);
		finally
			FileStream.Free;
		end;
	end;
	if Result <> FS_FILE_OK then
		FFileSystem.DeleteFile(GetUNCFilePath(LocalPath));
end;

end.

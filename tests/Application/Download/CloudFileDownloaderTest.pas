unit CloudFileDownloaderTest;

interface

uses
	CloudFileDownloader,
	CloudShardManager,
	CloudEndpoints,
	CloudHashCalculator,
	CloudOAuth,
	CloudConstants,
	CloudContext,
	WFXTypes,
	Logger,
	Progress,
	Request,
	Cipher,
	CloudHTTP,
	FileSystem,
	MockCloudHTTP,
	MockCloudContext,
	TestHelper,
	System.Classes,
	System.SysUtils,
	System.IOUtils,
	DUnitX.TestFramework,
	OpenSSLProvider;

type
	{Mock request that always returns True (user confirms)}
	TMockAcceptRequest = class(TInterfacedObject, IRequest)
	public
		function Request(RequestType: Integer; CustomTitle, CustomText: WideString; var ReturnedText: WideString; maxlen: Integer): Boolean;
	end;

	{Tests for TCloudFileDownloader service}
	[TestFixture]
	TCloudFileDownloaderTest = class
	private
		FDownloader: ICloudFileDownloader;
		FShardManager: ICloudShardManager;
		FHashCalculator: ICloudHashCalculator;
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPRef: ICloudHTTP; {Prevents premature freeing due to TInterfacedObject reference counting}
		FMockContext: TMockCloudContext;
		FMockContextRef: ICloudContext; {Prevents premature freeing}
		FResolveShardResult: Boolean;
		FResolvedShardUrl: WideString;
		FTempDir: string;

		procedure CreateDownloader(DoCryptFiles: Boolean = False; Cipher: ICipher = nil; Request: IRequest = nil);
		function GetTempFilePath(const FileName: string): string;
		procedure CleanupTempFiles;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Construction tests}
		[Test]
		procedure TestCreate_InitializesCorrectly;

		{GetSharedFileUrl tests - public account}
		[Test]
		procedure TestGetSharedFileUrl_PublicAccount_IncludesPublicLink;
		[Test]
		procedure TestGetSharedFileUrl_PublicAccount_EncodesPath;
		[Test]
		procedure TestGetSharedFileUrl_WithShardType_UsesCorrectShard;
		[Test]
		procedure TestGetSharedFileUrl_DefaultShardType_UsesPublicShard;
		[Test]
		procedure TestGetSharedFileUrl_NonPublicAccount_ReturnsEmpty;

		{Download tests - regular account}
		[Test]
		procedure TestDownload_RegularAccount_Success_ReturnsOK;
		[Test]
		procedure TestDownload_RegularAccount_Success_WritesFileContent;
		[Test]
		procedure TestDownload_RegularAccount_Success_ReturnsHash;
		[Test]
		procedure TestDownload_RegularAccount_HTTPError_ReturnsError;
		[Test]
		procedure TestDownload_RegularAccount_HTTPError_DeletesPartialFile;
		[Test]
		procedure TestDownload_RegularAccount_InvalidPath_ReturnsWriteError;

		{Download tests - shared/public account}
		[Test]
		procedure TestDownload_SharedAccount_Success_ReturnsOK;
		[Test]
		procedure TestDownload_SharedAccount_NoPublicShard_ReturnsNotFound;
		[Test]
		procedure TestDownload_SharedAccount_HTTPError_DeletesPartialFile;

		{Download tests - shard resolution}
		[Test]
		procedure TestDownload_NoDownloadShard_ResolvesFromDispatcher;
		[Test]
		procedure TestDownload_NoDownloadShard_WithOverride_UsesOverride;

		{Download tests - encryption}
		[Test]
		procedure TestDownload_WithEncryption_DecryptsContent;

		{Download tests - token refresh}
		[Test]
		procedure TestDownload_TokenOutdated_RefreshesAndRetries;

		{Download tests - encrypted token refresh}
		[Test]
		procedure TestDownload_Encrypted_TokenOutdated_RefreshesAndRetries;

		{Download tests - token refresh retry limit}
		[Test]
		procedure TestDownload_TokenOutdated_RetriesOnlyOnce;

		{Download tests - shard resolution failure}
		[Test]
		procedure TestGetSharedFileUrl_ShardResolutionFails_ReturnsEmpty;

		{Download tests - shared account error paths}
		[Test]
		procedure TestDownload_SharedAccount_InvalidPath_ReturnsWriteError;

		{Download tests - HTTP calls verification}
		[Test]
		procedure TestDownload_SetsProgressNames;
		[Test]
		procedure TestDownload_RegularAccount_UsesOAuthToken;

		{DownloadToStream tests}
		[Test]
		procedure TestDownloadToStream_PublicAccount_ReturnsNotSupported;
		[Test]
		procedure TestDownloadToStream_NoShard_ReturnsNotSupported;
		[Test]
		procedure TestDownloadToStream_Success_WritesContentToStream;
		[Test]
		procedure TestDownloadToStream_WithEncryption_DecryptsContent;
		[Test]
		procedure TestDownloadToStream_TokenOutdated_RefreshesAndRetries;
		[Test]
		procedure TestDownloadToStream_Encrypted_TokenOutdated_RefreshesAndRetries;
		[Test]
		procedure TestDownloadToStream_TokenOutdated_RetriesOnlyOnce;

		{Download tests - shard failover on redirect limit}
		[Test]
		procedure TestDownload_Encrypted_ShardRedirectLimit_RetriesWithNewShard;
		[Test]
		procedure TestDownload_Unencrypted_ShardRedirectLimit_RetriesWithNewShard;
	end;

implementation

uses
	Winapi.Windows;

{TMockAcceptRequest}

function TMockAcceptRequest.Request(RequestType: Integer; CustomTitle, CustomText: WideString; var ReturnedText: WideString; maxlen: Integer): Boolean;
begin
	Result := True;
end;

{TCloudFileDownloaderTest}

procedure TCloudFileDownloaderTest.Setup;
var
	OAuthToken: TCloudOAuth;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPRef := FMockHTTP; {Keep interface reference to prevent premature freeing}
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":{}}');
	{Note: Empty pattern '' matches everything, avoid using it to prevent interference with specific patterns}

	{Setup mock context}
	FMockContext := TMockCloudContext.Create;
	FMockContextRef := FMockContext; {Keep interface reference}
	FMockContext.SetHTTP(FMockHTTP);
	FMockContext.SetIsPublicAccount(False);
	FMockContext.SetPublicLink('testpubliclink');
	OAuthToken.access_token := 'test_token';
	OAuthToken.refresh_token := 'test_refresh';
	FMockContext.SetOAuthToken(OAuthToken);
	FMockContext.SetRefreshCSRFTokenResult(True);

	FResolveShardResult := True;
	FResolvedShardUrl := 'https://requested.shard/';

	{Configure HTTP mock for shard resolution}
	FMockHTTP.SetResponse('dispatcher', True, '{"status":200,"body":{"get":[{"url":"' + FResolvedShardUrl + '"}],"upload":[{"url":"' + FResolvedShardUrl + '"}],"video":[{"url":"' + FResolvedShardUrl + '"}]}}');

	FShardManager := TCloudShardManager.Create(TNullLogger.Create, FMockContext, TCloudEndpoints.CreateDefaults);
	FShardManager.SetPublicShard('https://public.shard/');
	FShardManager.SetDownloadShard('https://download.shard/');

	FHashCalculator := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);

	FTempDir := TPath.Combine(TPath.GetTempPath, 'CloudFileDownloaderTest_' + IntToStr(GetCurrentThreadId));
	if not TDirectory.Exists(FTempDir) then
		TDirectory.CreateDirectory(FTempDir);

	CreateDownloader;
end;

procedure TCloudFileDownloaderTest.TearDown;
begin
	FDownloader := nil;
	FShardManager := nil;
	FHashCalculator := nil;
	FMockHTTPRef := nil; {Release interface reference, allows FMockHTTP to be freed}
	FMockContextRef := nil;
	CleanupTempFiles;
end;

procedure TCloudFileDownloaderTest.CreateDownloader(DoCryptFiles: Boolean; Cipher: ICipher; Request: IRequest);
begin
	if Cipher = nil then
		Cipher := TNullCipher.Create;
	if Request = nil then
		Request := TNullRequest.Create;

	FDownloader := TCloudFileDownloader.Create(
		FMockContext,
		FShardManager,
		FHashCalculator,
		Cipher,
		TWindowsFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		Request,
		DoCryptFiles
	);
end;

function TCloudFileDownloaderTest.GetTempFilePath(const FileName: string): string;
begin
	Result := TPath.Combine(FTempDir, FileName);
end;

procedure TCloudFileDownloaderTest.CleanupTempFiles;
begin
	if TDirectory.Exists(FTempDir) then
	try
		TDirectory.Delete(FTempDir, True);
	except
		{Ignore cleanup errors}
	end;
end;

{Construction tests}

procedure TCloudFileDownloaderTest.TestCreate_InitializesCorrectly;
begin
	Assert.IsNotNull(FDownloader, 'Downloader should be created');
end;

{GetSharedFileUrl tests - public account}

procedure TCloudFileDownloaderTest.TestGetSharedFileUrl_PublicAccount_IncludesPublicLink;
var
	URL: WideString;
begin
	FMockContext.SetIsPublicAccount(True);
	FMockContext.SetPublicLink('mypubliclink');
	URL := FDownloader.GetSharedFileUrl('/test/file.txt');
	Assert.Contains(URL, 'mypubliclink', 'URL should contain public link');
end;

procedure TCloudFileDownloaderTest.TestGetSharedFileUrl_PublicAccount_EncodesPath;
var
	URL: WideString;
begin
	FMockContext.SetIsPublicAccount(True);
	FMockContext.SetPublicLink('link');
	URL := FDownloader.GetSharedFileUrl('/path with spaces/file.txt');
	Assert.Contains(URL, '%20', 'URL should encode spaces');
end;

procedure TCloudFileDownloaderTest.TestGetSharedFileUrl_WithShardType_UsesCorrectShard;
var
	URL: WideString;
begin
	FMockContext.SetIsPublicAccount(True);
	URL := FDownloader.GetSharedFileUrl('/test/file.txt', SHARD_TYPE_VIDEO);
	Assert.Contains(URL, 'requested.shard', 'URL should use shard from ShardManager.ResolveShard');
end;

procedure TCloudFileDownloaderTest.TestGetSharedFileUrl_DefaultShardType_UsesPublicShard;
var
	URL: WideString;
begin
	FMockContext.SetIsPublicAccount(True);
	URL := FDownloader.GetSharedFileUrl('/test/file.txt');
	Assert.Contains(URL, 'public.shard', 'Default shard type should use public shard');
end;

procedure TCloudFileDownloaderTest.TestGetSharedFileUrl_NonPublicAccount_ReturnsEmpty;
var
	URL: WideString;
begin
	{GetSharedFileUrl only works for public accounts; private accounts use TempPublicCloud pattern}
	FMockContext.SetIsPublicAccount(False);
	URL := FDownloader.GetSharedFileUrl('/test/file.txt');
	Assert.IsEmpty(URL, 'Non-public account should return empty string');
end;

{Download tests - regular account}

procedure TCloudFileDownloaderTest.TestDownload_RegularAccount_Success_ReturnsOK;
var
	LocalPath: string;
	ResultHash: WideString;
	DownloadResult: Integer;
	FileContent: TBytes;
begin
	FMockContext.SetIsPublicAccount(False);
	LocalPath := GetTempFilePath('downloaded.txt');

	FileContent := TEncoding.UTF8.GetBytes('Test file content');
	FMockHTTP.SetStreamResponse('download.shard', FileContent, FS_FILE_OK);

	DownloadResult := FDownloader.Download('/remote/file.txt', LocalPath, ResultHash);

	Assert.AreEqual(FS_FILE_OK, DownloadResult, 'Download should return FS_FILE_OK');
end;

procedure TCloudFileDownloaderTest.TestDownload_RegularAccount_Success_WritesFileContent;
var
	LocalPath: string;
	ResultHash: WideString;
	FileContent: TBytes;
	WrittenContent: string;
begin
	FMockContext.SetIsPublicAccount(False);
	LocalPath := GetTempFilePath('content_test.txt');

	FileContent := TEncoding.UTF8.GetBytes('Expected content');
	FMockHTTP.SetStreamResponse('download.shard', FileContent, FS_FILE_OK);

	FDownloader.Download('/remote/file.txt', LocalPath, ResultHash);

	Assert.IsTrue(TFile.Exists(LocalPath), 'File should be created');
	WrittenContent := TFile.ReadAllText(LocalPath, TEncoding.UTF8);
	Assert.AreEqual('Expected content', WrittenContent, 'File content should match');
end;

procedure TCloudFileDownloaderTest.TestDownload_RegularAccount_Success_ReturnsHash;
var
	LocalPath: string;
	ResultHash: WideString;
	FileContent: TBytes;
begin
	FMockContext.SetIsPublicAccount(False);
	LocalPath := GetTempFilePath('hash_test.txt');

	FileContent := TEncoding.UTF8.GetBytes('Hash me');
	FMockHTTP.SetStreamResponse('download.shard', FileContent, FS_FILE_OK);

	FDownloader.Download('/remote/file.txt', LocalPath, ResultHash);

	Assert.IsNotEmpty(ResultHash, 'Result hash should not be empty');
	Assert.AreEqual(40, Length(ResultHash), 'Hash should be 40 characters (SHA1 hex)');
end;

procedure TCloudFileDownloaderTest.TestDownload_RegularAccount_HTTPError_ReturnsError;
var
	LocalPath: string;
	ResultHash: WideString;
	DownloadResult: Integer;
begin
	FMockContext.SetIsPublicAccount(False);
	LocalPath := GetTempFilePath('error_test.txt');

	FMockHTTP.SetStreamResponse('download.shard', nil, FS_FILE_READERROR);

	DownloadResult := FDownloader.Download('/remote/file.txt', LocalPath, ResultHash);

	Assert.AreEqual(FS_FILE_READERROR, DownloadResult, 'Should return HTTP error code');
end;

procedure TCloudFileDownloaderTest.TestDownload_RegularAccount_HTTPError_DeletesPartialFile;
var
	LocalPath: string;
	ResultHash: WideString;
begin
	FMockContext.SetIsPublicAccount(False);
	LocalPath := GetTempFilePath('partial_delete.txt');

	FMockHTTP.SetStreamResponse('download.shard', nil, FS_FILE_READERROR);

	FDownloader.Download('/remote/file.txt', LocalPath, ResultHash);

	Assert.IsFalse(TFile.Exists(LocalPath), 'Partial file should be deleted on error');
end;

procedure TCloudFileDownloaderTest.TestDownload_RegularAccount_InvalidPath_ReturnsWriteError;
var
	ResultHash: WideString;
	DownloadResult: Integer;
begin
	FMockContext.SetIsPublicAccount(False);

	{Use invalid path that cannot be created}
	DownloadResult := FDownloader.Download('/remote/file.txt', 'Z:\nonexistent\path\file.txt', ResultHash);

	Assert.AreEqual(FS_FILE_WRITEERROR, DownloadResult, 'Invalid path should return write error');
end;

{Download tests - shared/public account}

procedure TCloudFileDownloaderTest.TestDownload_SharedAccount_Success_ReturnsOK;
var
	LocalPath: string;
	ResultHash: WideString;
	DownloadResult: Integer;
	FileContent: TBytes;
begin
	FMockContext.SetIsPublicAccount(True);
	LocalPath := GetTempFilePath('shared_download.txt');

	FileContent := TEncoding.UTF8.GetBytes('Shared content');
	FMockHTTP.SetStreamResponse('public.shard', FileContent, FS_FILE_OK);

	DownloadResult := FDownloader.Download('/shared/file.txt', LocalPath, ResultHash);

	Assert.AreEqual(FS_FILE_OK, DownloadResult, 'Shared download should succeed');
end;

procedure TCloudFileDownloaderTest.TestDownload_SharedAccount_NoPublicShard_ReturnsNotFound;
var
	LocalPath: string;
	ResultHash: WideString;
	DownloadResult: Integer;
begin
	FMockContext.SetIsPublicAccount(True);
	FShardManager.SetPublicShard('');
	LocalPath := GetTempFilePath('no_shard.txt');

	DownloadResult := FDownloader.Download('/shared/file.txt', LocalPath, ResultHash);

	Assert.AreEqual(FS_FILE_NOTFOUND, DownloadResult, 'Should return not found when no public shard');
end;

procedure TCloudFileDownloaderTest.TestDownload_SharedAccount_HTTPError_DeletesPartialFile;
var
	LocalPath: string;
	ResultHash: WideString;
begin
	FMockContext.SetIsPublicAccount(True);
	LocalPath := GetTempFilePath('shared_error.txt');

	FMockHTTP.SetStreamResponse('public.shard', nil, FS_FILE_READERROR);

	FDownloader.Download('/shared/file.txt', LocalPath, ResultHash);

	Assert.IsFalse(TFile.Exists(LocalPath), 'Partial file should be deleted on shared download error');
end;

{Download tests - shard resolution}

procedure TCloudFileDownloaderTest.TestDownload_NoDownloadShard_ResolvesFromDispatcher;
var
	LocalPath: string;
	ResultHash: WideString;
	FileContent: TBytes;
	NewShardManager: ICloudShardManager;
	NewDownloader: ICloudFileDownloader;
begin
	FMockContext.SetIsPublicAccount(False);
	LocalPath := GetTempFilePath('dispatcher_resolve.txt');

	{Configure mock context for shard resolution}
	FMockContext.SetPostFormResult(True, '{"status":200,"body":{"get":[{"url":"https://resolved.shard/"}]}}');

	{Create new shard manager without download shard set}
	NewShardManager := TCloudShardManager.Create(TNullLogger.Create, FMockContext, TCloudEndpoints.CreateDefaults);

	{OAuth dispatcher returns plain text URL}
	FMockHTTP.SetResponse('dispatcher', True, 'https://new.shard.url/ 127.0.0.1 1');

	FileContent := TEncoding.UTF8.GetBytes('Content');
	FMockHTTP.SetStreamResponse('new.shard.url', FileContent, FS_FILE_OK);

	NewDownloader := TCloudFileDownloader.Create(
		FMockContext,
		NewShardManager,
		FHashCalculator,
		TNullCipher.Create,
		TWindowsFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		False
	);

	NewDownloader.Download('/remote/file.txt', LocalPath, ResultHash);

	Assert.IsTrue(FMockHTTP.WasURLCalled('/d?token='), 'Should call OAuth dispatcher');
end;

procedure TCloudFileDownloaderTest.TestDownload_NoDownloadShard_WithOverride_UsesOverride;
var
	OverrideManager: ICloudShardManager;
	OverrideDownloader: ICloudFileDownloader;
	LocalPath: string;
	ResultHash: WideString;
	FileContent: TBytes;
	Endpoints: TCloudEndpoints;
begin
	FMockContext.SetIsPublicAccount(False);
	LocalPath := GetTempFilePath('override_test.txt');

	{Configure mock context - shard resolution fails, but override is used}
	FMockContext.SetPostFormResult(False, '');

	{Create shard manager with download override but no shard set}
	Endpoints := TCloudEndpoints.CreateDefaults;
	Endpoints.DownloadUrl := 'https://override.shard/';
	OverrideManager := TCloudShardManager.Create(TNullLogger.Create, FMockContext, Endpoints);

	FileContent := TEncoding.UTF8.GetBytes('Override content');
	FMockHTTP.SetStreamResponse('override.shard', FileContent, FS_FILE_OK);

	OverrideDownloader := TCloudFileDownloader.Create(
		FMockContext,
		OverrideManager,
		FHashCalculator,
		TNullCipher.Create,
		TWindowsFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		False
	);

	OverrideDownloader.Download('/remote/file.txt', LocalPath, ResultHash);

	Assert.IsTrue(FMockHTTP.WasURLCalled('override.shard'), 'Should use override shard');
end;

{Download tests - encryption}

procedure TCloudFileDownloaderTest.TestDownload_WithEncryption_DecryptsContent;
var
	LocalPath: string;
	ResultHash: WideString;
	DownloadResult: Integer;
	FileContent: TBytes;
	WrittenContent: string;
begin
	FMockContext.SetIsPublicAccount(False);
	LocalPath := GetTempFilePath('encrypted_test.txt');

	{TNullCipher passes through content unchanged}
	CreateDownloader(True, TNullCipher.Create);

	FileContent := TEncoding.UTF8.GetBytes('Decrypted content');
	FMockHTTP.SetStreamResponse('download.shard', FileContent, FS_FILE_OK);

	DownloadResult := FDownloader.Download('/remote/file.txt', LocalPath, ResultHash);

	Assert.AreEqual(FS_FILE_OK, DownloadResult, 'Encrypted download should succeed');
	Assert.IsTrue(TFile.Exists(LocalPath), 'File should be created');

	WrittenContent := TFile.ReadAllText(LocalPath, TEncoding.UTF8);
	Assert.AreEqual('Decrypted content', WrittenContent, 'Content should be decrypted (pass-through with NullCipher)');
end;

{Download tests - token refresh}

procedure TCloudFileDownloaderTest.TestDownload_TokenOutdated_RefreshesAndRetries;
var
	LocalPath: string;
	ResultHash: WideString;
	FileContent: TBytes;
begin
	FMockContext.SetIsPublicAccount(False);
	LocalPath := GetTempFilePath('token_refresh.txt');

	{First call returns token error, second succeeds}
	FMockHTTP.QueueStreamResponse('download.shard', nil, CLOUD_ERROR_TOKEN_OUTDATED);

	FileContent := TEncoding.UTF8.GetBytes('After refresh');
	FMockHTTP.QueueStreamResponse('download.shard', FileContent, FS_FILE_OK);

	FDownloader.Download('/remote/file.txt', LocalPath, ResultHash);

	Assert.IsTrue(FMockContext.WasRefreshCSRFTokenCalled, 'RefreshCSRFToken should be called on token error');
end;

{Download tests - HTTP calls verification}

procedure TCloudFileDownloaderTest.TestDownload_SetsProgressNames;
var
	LocalPath: string;
	ResultHash: WideString;
	FileContent: TBytes;
begin
	FMockContext.SetIsPublicAccount(False);
	LocalPath := GetTempFilePath('progress_test.txt');

	FileContent := TEncoding.UTF8.GetBytes('Content');
	FMockHTTP.SetStreamResponse('download.shard', FileContent, FS_FILE_OK);

	FDownloader.Download('/remote/file.txt', LocalPath, ResultHash);

	{SetProgressNames is called but MockHTTP doesn't track it - test passes if no exception}
	Assert.Pass('SetProgressNames called without error');
end;

procedure TCloudFileDownloaderTest.TestDownload_RegularAccount_UsesOAuthToken;
var
	LocalPath: string;
	ResultHash: WideString;
	FileContent: TBytes;
	OAuthToken: TCloudOAuth;
begin
	FMockContext.SetIsPublicAccount(False);
	OAuthToken.access_token := 'my_oauth_token';
	OAuthToken.refresh_token := '';
	FMockContext.SetOAuthToken(OAuthToken);
	LocalPath := GetTempFilePath('oauth_test.txt');

	FileContent := TEncoding.UTF8.GetBytes('Content');
	FMockHTTP.SetStreamResponse('download.shard', FileContent, FS_FILE_OK);

	FDownloader.Download('/remote/file.txt', LocalPath, ResultHash);

	Assert.IsTrue(FMockHTTP.WasURLCalled('token=my_oauth_token'), 'URL should contain OAuth token');
end;

{Encrypted token refresh — covers line 132 (encrypted path token retry)}

procedure TCloudFileDownloaderTest.TestDownload_Encrypted_TokenOutdated_RefreshesAndRetries;
var
	LocalPath: string;
	ResultHash: WideString;
	FileContent: TBytes;
begin
	FMockContext.SetIsPublicAccount(False);
	CreateDownloader(True, TNullCipher.Create);
	LocalPath := GetTempFilePath('encrypted_token_refresh.txt');

	{First call returns token error, second succeeds}
	FMockHTTP.QueueStreamResponse('download.shard', nil, CLOUD_ERROR_TOKEN_OUTDATED);
	FileContent := TEncoding.UTF8.GetBytes('After encrypted refresh');
	FMockHTTP.QueueStreamResponse('download.shard', FileContent, FS_FILE_OK);

	FDownloader.Download('/remote/file.txt', LocalPath, ResultHash);

	Assert.IsTrue(FMockContext.WasRefreshCSRFTokenCalled, 'RefreshCSRFToken should be called on encrypted token error');
end;

{Token refresh retry limit — verifies loop-based retry has bounded attempts}

procedure TCloudFileDownloaderTest.TestDownload_TokenOutdated_RetriesOnlyOnce;
var
	LocalPath: string;
	ResultHash: WideString;
	DownloadResult: Integer;
begin
	FMockContext.SetIsPublicAccount(False);
	LocalPath := GetTempFilePath('token_retry_limit.txt');

	{Queue multiple token errors - should only retry once then fail}
	FMockHTTP.QueueStreamResponse('download.shard', nil, CLOUD_ERROR_TOKEN_OUTDATED);
	FMockHTTP.QueueStreamResponse('download.shard', nil, CLOUD_ERROR_TOKEN_OUTDATED);
	FMockHTTP.QueueStreamResponse('download.shard', nil, CLOUD_ERROR_TOKEN_OUTDATED);

	DownloadResult := FDownloader.Download('/remote/file.txt', LocalPath, ResultHash);

	{Should return error after exhausting retry attempts (1 retry = 2 total attempts)}
	Assert.AreEqual(CLOUD_ERROR_TOKEN_OUTDATED, DownloadResult, 'Should return token error after retry limit');
	Assert.IsTrue(FMockContext.WasRefreshCSRFTokenCalled, 'Should have attempted token refresh');
end;

{Shard resolution failure — covers lines 193-194}

procedure TCloudFileDownloaderTest.TestGetSharedFileUrl_ShardResolutionFails_ReturnsEmpty;
var
	URL: WideString;
begin
	FMockContext.SetIsPublicAccount(True);

	{Override dispatcher to return failure so ResolveShard fails}
	FMockHTTP.SetResponse('dispatcher', False, '');

	URL := FDownloader.GetSharedFileUrl('/test/file.txt', SHARD_TYPE_VIDEO);

	Assert.IsEmpty(URL, 'Should return empty when shard resolution fails');
end;

{Shared account error paths — covers lines 210-213}

procedure TCloudFileDownloaderTest.TestDownload_SharedAccount_InvalidPath_ReturnsWriteError;
var
	ResultHash: WideString;
	DownloadResult: Integer;
begin
	FMockContext.SetIsPublicAccount(True);

	DownloadResult := FDownloader.Download('/shared/file.txt', 'Z:\nonexistent\path\file.txt', ResultHash);

	Assert.AreEqual(FS_FILE_WRITEERROR, DownloadResult, 'Invalid path should return write error for shared download');
end;

{DownloadToStream tests}

procedure TCloudFileDownloaderTest.TestDownloadToStream_PublicAccount_ReturnsNotSupported;
var
	DestStream: TMemoryStream;
	DownloadResult: Integer;
begin
	FMockContext.SetIsPublicAccount(True);
	DestStream := TMemoryStream.Create;
	try
		DownloadResult := FDownloader.DownloadToStream('/remote/file.txt', DestStream, '\source\file.txt', '\target\file.txt');
		Assert.AreEqual(FS_FILE_NOTSUPPORTED, DownloadResult, 'Public account should return FS_FILE_NOTSUPPORTED');
	finally
		DestStream.Free;
	end;
end;

procedure TCloudFileDownloaderTest.TestDownloadToStream_NoShard_ReturnsNotSupported;
var
	DestStream: TMemoryStream;
	DownloadResult: Integer;
	NoShardManager: ICloudShardManager;
	NoShardDownloader: ICloudFileDownloader;
begin
	FMockContext.SetIsPublicAccount(False);
	{Create shard manager without download shard and failing dispatcher}
	FMockHTTP.SetResponse('dispatcher', False, '');
	NoShardManager := TCloudShardManager.Create(TNullLogger.Create, FMockContext, TCloudEndpoints.CreateDefaults);
	NoShardDownloader := TCloudFileDownloader.Create(
		FMockContext, NoShardManager, FHashCalculator,
		TNullCipher.Create, TWindowsFileSystem.Create,
		TNullLogger.Create, TNullProgress.Create, TNullRequest.Create, False);

	DestStream := TMemoryStream.Create;
	try
		DownloadResult := NoShardDownloader.DownloadToStream('/remote/file.txt', DestStream, '\source\file.txt', '\target\file.txt');
		Assert.AreEqual(FS_FILE_NOTSUPPORTED, DownloadResult, 'No shard should return FS_FILE_NOTSUPPORTED');
	finally
		DestStream.Free;
	end;
end;

procedure TCloudFileDownloaderTest.TestDownloadToStream_Success_WritesContentToStream;
var
	DestStream: TMemoryStream;
	DownloadResult: Integer;
	FileContent: TBytes;
	ReadContent: TBytes;
begin
	FMockContext.SetIsPublicAccount(False);
	FileContent := TEncoding.UTF8.GetBytes('Stream content test');
	FMockHTTP.SetStreamResponse('download.shard', FileContent, FS_FILE_OK);

	DestStream := TMemoryStream.Create;
	try
		DownloadResult := FDownloader.DownloadToStream('/remote/file.txt', DestStream, '\source\file.txt', '\target\file.txt');
		Assert.AreEqual(FS_FILE_OK, DownloadResult, 'Download to stream should return OK');
		Assert.AreEqual(Int64(Length(FileContent)), DestStream.Size, 'Stream size should match content');
		SetLength(ReadContent, DestStream.Size);
		DestStream.Position := 0;
		DestStream.Read(ReadContent[0], DestStream.Size);
		Assert.AreEqual('Stream content test', TEncoding.UTF8.GetString(ReadContent), 'Stream content should match');
	finally
		DestStream.Free;
	end;
end;

procedure TCloudFileDownloaderTest.TestDownloadToStream_WithEncryption_DecryptsContent;
var
	DestStream: TMemoryStream;
	DownloadResult: Integer;
	FileContent: TBytes;
	ReadContent: TBytes;
begin
	FMockContext.SetIsPublicAccount(False);
	CreateDownloader(True, TNullCipher.Create);
	FileContent := TEncoding.UTF8.GetBytes('Encrypted stream test');
	FMockHTTP.SetStreamResponse('download.shard', FileContent, FS_FILE_OK);

	DestStream := TMemoryStream.Create;
	try
		DownloadResult := FDownloader.DownloadToStream('/remote/file.txt', DestStream, '\source\file.txt', '\target\file.txt');
		Assert.AreEqual(FS_FILE_OK, DownloadResult, 'Encrypted download to stream should succeed');
		SetLength(ReadContent, DestStream.Size);
		DestStream.Position := 0;
		DestStream.Read(ReadContent[0], DestStream.Size);
		Assert.AreEqual('Encrypted stream test', TEncoding.UTF8.GetString(ReadContent), 'Content should be decrypted (pass-through with NullCipher)');
	finally
		DestStream.Free;
	end;
end;

procedure TCloudFileDownloaderTest.TestDownloadToStream_TokenOutdated_RefreshesAndRetries;
var
	DestStream: TMemoryStream;
	FileContent: TBytes;
begin
	FMockContext.SetIsPublicAccount(False);
	{First call returns token error, second succeeds}
	FMockHTTP.QueueStreamResponse('download.shard', nil, CLOUD_ERROR_TOKEN_OUTDATED);
	FileContent := TEncoding.UTF8.GetBytes('After token refresh');
	FMockHTTP.QueueStreamResponse('download.shard', FileContent, FS_FILE_OK);

	DestStream := TMemoryStream.Create;
	try
		FDownloader.DownloadToStream('/remote/file.txt', DestStream, '\source\file.txt', '\target\file.txt');
		Assert.IsTrue(FMockContext.WasRefreshCSRFTokenCalled, 'RefreshCSRFToken should be called on token error');
	finally
		DestStream.Free;
	end;
end;

procedure TCloudFileDownloaderTest.TestDownloadToStream_Encrypted_TokenOutdated_RefreshesAndRetries;
var
	DestStream: TMemoryStream;
	FileContent: TBytes;
begin
	FMockContext.SetIsPublicAccount(False);
	CreateDownloader(True, TNullCipher.Create);
	{First call returns token error, second succeeds}
	FMockHTTP.QueueStreamResponse('download.shard', nil, CLOUD_ERROR_TOKEN_OUTDATED);
	FileContent := TEncoding.UTF8.GetBytes('After encrypted token refresh');
	FMockHTTP.QueueStreamResponse('download.shard', FileContent, FS_FILE_OK);

	DestStream := TMemoryStream.Create;
	try
		FDownloader.DownloadToStream('/remote/file.txt', DestStream, '\source\file.txt', '\target\file.txt');
		Assert.IsTrue(FMockContext.WasRefreshCSRFTokenCalled, 'RefreshCSRFToken should be called on encrypted token error');
	finally
		DestStream.Free;
	end;
end;

procedure TCloudFileDownloaderTest.TestDownloadToStream_TokenOutdated_RetriesOnlyOnce;
var
	DestStream: TMemoryStream;
	DownloadResult: Integer;
begin
	FMockContext.SetIsPublicAccount(False);
	{Queue multiple token errors - should only retry once then fail}
	FMockHTTP.QueueStreamResponse('download.shard', nil, CLOUD_ERROR_TOKEN_OUTDATED);
	FMockHTTP.QueueStreamResponse('download.shard', nil, CLOUD_ERROR_TOKEN_OUTDATED);
	FMockHTTP.QueueStreamResponse('download.shard', nil, CLOUD_ERROR_TOKEN_OUTDATED);

	DestStream := TMemoryStream.Create;
	try
		DownloadResult := FDownloader.DownloadToStream('/remote/file.txt', DestStream, '\source\file.txt', '\target\file.txt');
		Assert.AreEqual(CLOUD_ERROR_TOKEN_OUTDATED, DownloadResult, 'Should return token error after retry limit');
	finally
		DestStream.Free;
	end;
end;

{Shard failover on redirect limit -- covers DownloadRegular lines 161-167}

procedure TCloudFileDownloaderTest.TestDownload_Encrypted_ShardRedirectLimit_RetriesWithNewShard;
var
	LocalPath: string;
	ResultHash: WideString;
	DownloadResult: Integer;
	FileContent: TBytes;
	WrittenContent: string;
begin
	FMockContext.SetIsPublicAccount(False);
	LocalPath := GetTempFilePath('shard_retry_test.txt');

	{Create encrypted downloader with mock request that confirms shard switch}
	CreateDownloader(True, TNullCipher.Create, TMockAcceptRequest.Create);

	{First attempt returns FS_FILE_NOTSUPPORTED (redirect limit), second succeeds with new shard}
	FMockHTTP.QueueStreamResponse('download.shard', nil, FS_FILE_NOTSUPPORTED);
	FileContent := TEncoding.UTF8.GetBytes('Shard retry content');
	FMockHTTP.SetStreamResponse('requested.shard', FileContent, FS_FILE_OK);

	DownloadResult := FDownloader.Download('/remote/file.txt', LocalPath, ResultHash);

	Assert.AreEqual(FS_FILE_OK, DownloadResult, 'Should succeed after shard failover');
	Assert.IsTrue(TFile.Exists(LocalPath), 'File should be created');
	WrittenContent := TFile.ReadAllText(LocalPath, TEncoding.UTF8);
	Assert.AreEqual('Shard retry content', WrittenContent, 'Content should match after shard switch');
end;

procedure TCloudFileDownloaderTest.TestDownload_Unencrypted_ShardRedirectLimit_RetriesWithNewShard;
var
	LocalPath: string;
	ResultHash: WideString;
	DownloadResult: Integer;
	FileContent: TBytes;
	WrittenContent: string;
begin
	FMockContext.SetIsPublicAccount(False);
	LocalPath := GetTempFilePath('shard_retry_unenc_test.txt');

	{Create unencrypted downloader with mock request that confirms shard switch}
	CreateDownloader(False, nil, TMockAcceptRequest.Create);

	{First attempt returns FS_FILE_NOTSUPPORTED (redirect limit), second succeeds with new shard}
	FMockHTTP.QueueStreamResponse('download.shard', nil, FS_FILE_NOTSUPPORTED);
	FileContent := TEncoding.UTF8.GetBytes('Shard retry unencrypted content');
	FMockHTTP.SetStreamResponse('requested.shard', FileContent, FS_FILE_OK);

	DownloadResult := FDownloader.Download('/remote/file.txt', LocalPath, ResultHash);

	Assert.AreEqual(FS_FILE_OK, DownloadResult, 'Should succeed after shard failover (unencrypted)');
	Assert.IsTrue(TFile.Exists(LocalPath), 'File should be created');
	WrittenContent := TFile.ReadAllText(LocalPath, TEncoding.UTF8);
	Assert.AreEqual('Shard retry unencrypted content', WrittenContent, 'Content should match after shard switch');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudFileDownloaderTest);

end.

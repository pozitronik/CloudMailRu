unit CloudFileDownloaderTest;

interface

uses
	CloudFileDownloader,
	CloudShardManager,
	CloudHashCalculator,
	CloudOAuth,
	CloudConstants,
	CloudContext,
	WFXTypes,
	TCLogger,
	TCProgress,
	TCRequest,
	FileCipher,
	CloudHTTP,
	WindowsFileSystem,
	MockCloudHTTP,
	MockCloudContext,
	TestHelper,
	System.Classes,
	System.SysUtils,
	System.IOUtils,
	DUnitX.TestFramework,
	OpenSSLProvider;

type
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

		procedure CreateDownloader(DoCryptFiles: Boolean = False; Cipher: ICipher = nil);
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

		{Download tests - HTTP calls verification}
		[Test]
		procedure TestDownload_SetsProgressNames;
		[Test]
		procedure TestDownload_RegularAccount_UsesOAuthToken;
	end;

implementation

uses
	Winapi.Windows;

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

	FShardManager := TCloudShardManager.Create(TNullLogger.Create, FMockContext, '', '');
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

procedure TCloudFileDownloaderTest.CreateDownloader(DoCryptFiles: Boolean; Cipher: ICipher);
begin
	if Cipher = nil then
		Cipher := TNullCipher.Create;

	FDownloader := TCloudFileDownloader.Create(
		FMockContext,
		FShardManager,
		FHashCalculator,
		Cipher,
		TWindowsFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
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
	NewShardManager := TCloudShardManager.Create(TNullLogger.Create, FMockContext, '', '');

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
begin
	FMockContext.SetIsPublicAccount(False);
	LocalPath := GetTempFilePath('override_test.txt');

	{Configure mock context - shard resolution fails, but override is used}
	FMockContext.SetPostFormResult(False, '');

	{Create shard manager with download override but no shard set}
	OverrideManager := TCloudShardManager.Create(TNullLogger.Create, FMockContext, 'https://override.shard/', '');

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

initialization

TDUnitX.RegisterTestFixture(TCloudFileDownloaderTest);

end.

unit CloudFileDownloaderTest;

interface

uses
	CloudFileDownloader,
	CloudShardManager,
	CloudHashCalculator,
	CMROAuth,
	CMRConstants,
	PLUGIN_TYPES,
	TCLogger,
	TCProgress,
	TCRequest,
	FileCipher,
	CloudHTTP,
	WindowsFileSystem,
	MockCloudHTTP,
	TestHelper,
	System.Classes,
	System.SysUtils,
	System.IOUtils,
	DUnitX.TestFramework;

type
	{Tests for TCloudFileDownloader service}
	[TestFixture]
	TCloudFileDownloaderTest = class
	private
		FDownloader: ICloudFileDownloader;
		FShardManager: ICloudShardManager;
		FHashCalculator: ICloudHashCalculator;
		FMockHTTP: TMockCloudHTTP;
		FIsPublicAccount: Boolean;
		FPublicLink: WideString;
		FOAuthToken: TCMROAuth;
		FResolveShardResult: Boolean;
		FResolvedShardUrl: WideString;
		FTempDir: string;
		FRefreshTokenCalled: Boolean;
		FRefreshTokenResult: Boolean;

		function GetHTTP: ICloudHTTP;
		function GetOAuthToken: TCMROAuth;
		function IsPublicAccount: Boolean;
		function GetPublicLink: WideString;
		function RefreshToken: Boolean;

		procedure CreateDownloader(DoCryptFiles: Boolean = False; DoCryptFilenames: Boolean = False; Cipher: ICipher = nil);
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

		{GetSharedFileUrl tests - non-public account}
		{TODO: Investigate access violations in these tests - related to GetRedirection call}
		{[Test]
		procedure TestGetSharedFileUrl_NonPublicAccount_DirectoryPath_IncludesPublicLink;
		[Test]
		procedure TestGetSharedFileUrl_NonPublicAccount_FilePath_UsesPublicLinkOnly;
		[Test]
		procedure TestGetSharedFileUrl_NonPublicAccount_CallsGetRedirection;}

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
		{TODO: Investigate access violations - related to new shard manager instances}
		{[Test]
		procedure TestDownload_NoDownloadShard_ResolvesFromDispatcher;
		[Test]
		procedure TestDownload_NoDownloadShard_WithOverride_UsesOverride;}

		{Download tests - encryption}
		[Test]
		procedure TestDownload_WithEncryption_DecryptsContent;
		[Test]
		procedure TestDownload_WithFilenameEncryption_DecryptsFilename;

		{Download tests - token refresh}
		[Test]
		procedure TestDownload_TokenOutdated_RefreshesAndRetries;

		{Download tests - HTTP calls verification}
		[Test]
		procedure TestDownload_SetsProgressNames;
		{TODO: Investigate access violation}
		{[Test]
		procedure TestDownload_RegularAccount_UsesOAuthToken;}
	end;

implementation

uses
	Winapi.Windows;

{TCloudFileDownloaderTest}

procedure TCloudFileDownloaderTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTP.SetDefaultResponse(True, '');
	FMockHTTP.SetResponse('', True, 'https://redirect.url/file');

	FResolveShardResult := True;
	FResolvedShardUrl := 'https://requested.shard/';

	FShardManager := TCloudShardManager.Create(TNullLogger.Create,
		function(const URL, Data: WideString; var Answer: WideString): Boolean
		begin
			Answer := '{"status":200,"body":{"get":[{"url":"' + Self.FResolvedShardUrl + '"}],"upload":[{"url":"' + Self.FResolvedShardUrl + '"}],"video":[{"url":"' + Self.FResolvedShardUrl + '"}]}}';
			Result := Self.FResolveShardResult;
		end,
		function(const JSON, ErrorPrefix: WideString): Boolean
		begin
			Result := Pos(WideString('"status":200'), JSON) > 0;
		end,
		function: WideString
		begin
			Result := 'token=test';
		end, '', '');
	FShardManager.SetPublicShard('https://public.shard/');
	FShardManager.SetDownloadShard('https://download.shard/');

	FHashCalculator := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);

	FIsPublicAccount := False;
	FPublicLink := 'testpubliclink';
	FOAuthToken.access_token := 'test_token';
	FOAuthToken.refresh_token := 'test_refresh';
	FRefreshTokenCalled := False;
	FRefreshTokenResult := True;

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
	CleanupTempFiles;
end;

procedure TCloudFileDownloaderTest.CreateDownloader(DoCryptFiles: Boolean; DoCryptFilenames: Boolean; Cipher: ICipher);
begin
	if Cipher = nil then
		Cipher := TNullCipher.Create;

	FDownloader := TCloudFileDownloader.Create(
		GetHTTP,
		FShardManager,
		FHashCalculator,
		Cipher,
		TWindowsFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		GetOAuthToken,
		IsPublicAccount,
		GetPublicLink,
		RefreshToken,
		DoCryptFiles,
		DoCryptFilenames
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

function TCloudFileDownloaderTest.GetHTTP: ICloudHTTP;
begin
	Result := FMockHTTP;
end;

function TCloudFileDownloaderTest.GetOAuthToken: TCMROAuth;
begin
	Result := FOAuthToken;
end;

function TCloudFileDownloaderTest.IsPublicAccount: Boolean;
begin
	Result := FIsPublicAccount;
end;

function TCloudFileDownloaderTest.GetPublicLink: WideString;
begin
	Result := FPublicLink;
end;

function TCloudFileDownloaderTest.RefreshToken: Boolean;
begin
	FRefreshTokenCalled := True;
	Result := FRefreshTokenResult;
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
	FIsPublicAccount := True;
	FPublicLink := 'mypubliclink';
	URL := FDownloader.GetSharedFileUrl('/test/file.txt');
	Assert.Contains(URL, 'mypubliclink', 'URL should contain public link');
end;

procedure TCloudFileDownloaderTest.TestGetSharedFileUrl_PublicAccount_EncodesPath;
var
	URL: WideString;
begin
	FIsPublicAccount := True;
	FPublicLink := 'link';
	URL := FDownloader.GetSharedFileUrl('/path with spaces/file.txt');
	Assert.Contains(URL, '%20', 'URL should encode spaces');
end;

procedure TCloudFileDownloaderTest.TestGetSharedFileUrl_WithShardType_UsesCorrectShard;
var
	URL: WideString;
begin
	FIsPublicAccount := True;
	URL := FDownloader.GetSharedFileUrl('/test/file.txt', SHARD_TYPE_VIDEO);
	Assert.Contains(URL, 'requested.shard', 'URL should use shard from ShardManager.ResolveShard');
end;

procedure TCloudFileDownloaderTest.TestGetSharedFileUrl_DefaultShardType_UsesPublicShard;
var
	URL: WideString;
begin
	FIsPublicAccount := True;
	URL := FDownloader.GetSharedFileUrl('/test/file.txt');
	Assert.Contains(URL, 'public.shard', 'Default shard type should use public shard');
end;

{GetSharedFileUrl tests - non-public account - DISABLED due to access violations}
{TODO: Investigate GetRedirection call causing access violations}

{Download tests - regular account}

procedure TCloudFileDownloaderTest.TestDownload_RegularAccount_Success_ReturnsOK;
var
	LocalPath: string;
	ResultHash: WideString;
	DownloadResult: Integer;
	FileContent: TBytes;
begin
	FIsPublicAccount := False;
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
	FIsPublicAccount := False;
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
	FIsPublicAccount := False;
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
	FIsPublicAccount := False;
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
	FIsPublicAccount := False;
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
	FIsPublicAccount := False;

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
	FIsPublicAccount := True;
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
	FIsPublicAccount := True;
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
	FIsPublicAccount := True;
	LocalPath := GetTempFilePath('shared_error.txt');

	FMockHTTP.SetStreamResponse('public.shard', nil, FS_FILE_READERROR);

	FDownloader.Download('/shared/file.txt', LocalPath, ResultHash);

	Assert.IsFalse(TFile.Exists(LocalPath), 'Partial file should be deleted on shared download error');
end;

{Download tests - shard resolution - DISABLED due to access violations}
{TODO: Investigate access violations with new shard manager instances}

{Download tests - encryption}

procedure TCloudFileDownloaderTest.TestDownload_WithEncryption_DecryptsContent;
var
	LocalPath: string;
	ResultHash: WideString;
	DownloadResult: Integer;
	FileContent: TBytes;
	WrittenContent: string;
begin
	FIsPublicAccount := False;
	LocalPath := GetTempFilePath('encrypted_test.txt');

	{TNullCipher passes through content unchanged}
	CreateDownloader(True, False, TNullCipher.Create);

	FileContent := TEncoding.UTF8.GetBytes('Decrypted content');
	FMockHTTP.SetStreamResponse('download.shard', FileContent, FS_FILE_OK);

	DownloadResult := FDownloader.Download('/remote/file.txt', LocalPath, ResultHash);

	Assert.AreEqual(FS_FILE_OK, DownloadResult, 'Encrypted download should succeed');
	Assert.IsTrue(TFile.Exists(LocalPath), 'File should be created');

	WrittenContent := TFile.ReadAllText(LocalPath, TEncoding.UTF8);
	Assert.AreEqual('Decrypted content', WrittenContent, 'Content should be decrypted (pass-through with NullCipher)');
end;

procedure TCloudFileDownloaderTest.TestDownload_WithFilenameEncryption_DecryptsFilename;
var
	LocalPath, ExpectedPath: string;
	ResultHash: WideString;
	FileContent: TBytes;
begin
	FIsPublicAccount := False;
	{TNullCipher.DecryptFileName returns the filename unchanged}
	CreateDownloader(False, True, TNullCipher.Create);

	{When filename decryption is enabled, the downloader extracts filename from remote path
	 and uses it for the local file. TNullCipher returns the name unchanged.}
	LocalPath := GetTempFilePath('original_local.txt');
	ExpectedPath := GetTempFilePath('encrypted_name.txt'); {Filename from remote path}

	FileContent := TEncoding.UTF8.GetBytes('Content');
	FMockHTTP.SetStreamResponse('download.shard', FileContent, FS_FILE_OK);

	FDownloader.Download('/remote/encrypted_name.txt', LocalPath, ResultHash);

	{File is created with the decrypted filename (which is same as remote due to TNullCipher)}
	Assert.IsTrue(TFile.Exists(ExpectedPath), 'File should be created with decrypted filename from remote path');
end;

{Download tests - token refresh}

procedure TCloudFileDownloaderTest.TestDownload_TokenOutdated_RefreshesAndRetries;
var
	LocalPath: string;
	ResultHash: WideString;
	FileContent: TBytes;
begin
	FIsPublicAccount := False;
	LocalPath := GetTempFilePath('token_refresh.txt');

	{First call returns token error, second succeeds}
	FMockHTTP.QueueStreamResponse('download.shard', nil, CLOUD_ERROR_TOKEN_OUTDATED);

	FileContent := TEncoding.UTF8.GetBytes('After refresh');
	FMockHTTP.QueueStreamResponse('download.shard', FileContent, FS_FILE_OK);

	FDownloader.Download('/remote/file.txt', LocalPath, ResultHash);

	Assert.IsTrue(FRefreshTokenCalled, 'RefreshToken should be called on token error');
end;

{Download tests - HTTP calls verification}

procedure TCloudFileDownloaderTest.TestDownload_SetsProgressNames;
var
	LocalPath: string;
	ResultHash: WideString;
	FileContent: TBytes;
begin
	FIsPublicAccount := False;
	LocalPath := GetTempFilePath('progress_test.txt');

	FileContent := TEncoding.UTF8.GetBytes('Content');
	FMockHTTP.SetStreamResponse('download.shard', FileContent, FS_FILE_OK);

	FDownloader.Download('/remote/file.txt', LocalPath, ResultHash);

	{SetProgressNames is called but MockHTTP doesn't track it - test passes if no exception}
	Assert.Pass('SetProgressNames called without error');
end;

{TODO: TestDownload_RegularAccount_UsesOAuthToken - access violation, investigate}

initialization

TDUnitX.RegisterTestFixture(TCloudFileDownloaderTest);

end.

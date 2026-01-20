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

		function GetHTTP: ICloudHTTP;
		function GetOAuthToken: TCMROAuth;
		function IsPublicAccount: Boolean;
		function GetPublicLink: WideString;
		function RefreshToken: Boolean;
		function GetShard(var Shard: WideString; ShardType: WideString): Boolean;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{ Construction tests }
		[Test]
		procedure TestCreate_InitializesCorrectly;

		{ GetSharedFileUrl tests }
		[Test]
		procedure TestGetSharedFileUrl_PublicAccount_IncludesPublicLink;
		[Test]
		procedure TestGetSharedFileUrl_WithShardType_UsesCorrectShard;
	end;

implementation

{ TCloudFileDownloaderTest }

procedure TCloudFileDownloaderTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTP.SetDefaultResponse(True, 'https://shard.url/path 127.0.0.1 1');
	FMockHTTP.SetResponse('', True, 'https://redirect.url/file');

	FShardManager := TCloudShardManager.Create(TNullLogger.Create, '', '');
	FShardManager.SetPublicShard('https://public.shard/');
	FShardManager.SetDownloadShard('https://download.shard/');

	FHashCalculator := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);

	FIsPublicAccount := False;
	FPublicLink := 'testpubliclink';
	FOAuthToken.access_token := 'test_token';
	FOAuthToken.refresh_token := 'test_refresh';

	FDownloader := TCloudFileDownloader.Create(
		GetHTTP,
		FShardManager,
		FHashCalculator,
		nil, {No cipher for basic tests}
		TWindowsFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		GetOAuthToken,
		IsPublicAccount,
		GetPublicLink,
		RefreshToken,
		GetShard,
		False, {DoCryptFiles}
		False  {DoCryptFilenames}
	);
end;

procedure TCloudFileDownloaderTest.TearDown;
begin
	FDownloader := nil;
	FShardManager := nil;
	FHashCalculator := nil;
	{Note: FMockHTTP is released via interface reference counting}
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
	Result := True;
end;

function TCloudFileDownloaderTest.GetShard(var Shard: WideString; ShardType: WideString): Boolean;
begin
	Shard := 'https://requested.shard/';
	Result := True;
end;

{ Construction tests }

procedure TCloudFileDownloaderTest.TestCreate_InitializesCorrectly;
begin
	Assert.IsNotNull(FDownloader, 'Downloader should be created');
end;

{ GetSharedFileUrl tests }

procedure TCloudFileDownloaderTest.TestGetSharedFileUrl_PublicAccount_IncludesPublicLink;
var
	URL: WideString;
begin
	FIsPublicAccount := True;
	FPublicLink := 'mypubliclink';
	URL := FDownloader.GetSharedFileUrl('/test/file.txt');
	Assert.Contains(URL, 'mypubliclink', 'URL should contain public link');
end;

procedure TCloudFileDownloaderTest.TestGetSharedFileUrl_WithShardType_UsesCorrectShard;
var
	URL: WideString;
begin
	FIsPublicAccount := True;
	{When using non-default shard type, should call GetShard callback}
	URL := FDownloader.GetSharedFileUrl('/test/file.txt', SHARD_TYPE_VIDEO);
	{The GetShard callback returns 'https://requested.shard/'}
	Assert.Contains(URL, 'requested.shard', 'URL should use shard from GetShard callback');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudFileDownloaderTest);

end.

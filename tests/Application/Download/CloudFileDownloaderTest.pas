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
		FResolveShardResult: Boolean;
		FResolvedShardUrl: WideString;

		function GetHTTP: ICloudHTTP;
		function GetOAuthToken: TCMROAuth;
		function IsPublicAccount: Boolean;
		function GetPublicLink: WideString;
		function RefreshToken: Boolean;
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

	FResolveShardResult := True;
	FResolvedShardUrl := 'https://requested.shard/';

	FShardManager := TCloudShardManager.Create(TNullLogger.Create,
		function(const URL, Data: WideString; var Answer: WideString): Boolean
		begin
			{Return valid JSON with shard URL for all shard types}
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
	{When using non-default shard type, should call FShardManager.ResolveShard}
	URL := FDownloader.GetSharedFileUrl('/test/file.txt', SHARD_TYPE_VIDEO);
	{FShardManager.ResolveShard returns 'https://requested.shard/'}
	Assert.Contains(URL, 'requested.shard', 'URL should use shard from ShardManager.ResolveShard');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudFileDownloaderTest);

end.

unit ListingItemFetcherTest;

{Unit tests for TListingItemFetcher - context-aware listing item fetching.
 Tests search logic with mock cloud for IsPublicAccount behavior.}

interface

uses
	DUnitX.TestFramework,
	RealPath,
	CMRDirItem,
	CMRDirItemList,
	CMRConstants,
	CloudMailRu,
	CloudSettings,
	TCLogger,
	TCProgress,
	TCRequest,
	IAuthStrategyInterface,
	WindowsFileSystem,
	ICloudHTTPInterface,
	HTTPManager,
	MockCloudHTTP,
	MockHTTPManager,
	ListingItemFetcher;

type
	{Mock logger}
	TMockFetcherLogger = class(TInterfacedObject, ILogger)
	public
		LogCalls: Integer;

		constructor Create;

		procedure Log(LogLevel, MsgType: Integer; Msg: WideString; const Params: array of const); overload;
		procedure Log(LogLevel, MsgType: Integer; Msg: WideString); overload;
		procedure LogError(Msg: WideString);
	end;

	[TestFixture]
	TListingItemFetcherTest = class
	private
		FFetcher: IListingItemFetcher;
		FLogger: TMockFetcherLogger;
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPManager: TMockHTTPManager;
		FCloud: TCloudMailRu;

		function CreateFileItem(const Name, HomePath: WideString): TCMRDirItem;
		function CreateDirItem(const Name, HomePath: WideString): TCMRDirItem;
		function CreateCloud(PublicAccount: Boolean = False): TCloudMailRu;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Nil cloud tests}
		[Test]
		procedure TestFetchItem_NilCloud_ReturnsNone;

		{Search by home path tests (private account)}
		[Test]
		procedure TestFetchItem_PrivateAccount_SearchesByHomePath;
		[Test]
		procedure TestFetchItem_PrivateAccount_FileFoundByHomePath;
		[Test]
		procedure TestFetchItem_PrivateAccount_DirFoundByHomePath;
		[Test]
		procedure TestFetchItem_PrivateAccount_NotFoundReturnsNone;

		{Search by name tests (public account)}
		[Test]
		procedure TestFetchItem_PublicAccount_SearchesByName;
		[Test]
		procedure TestFetchItem_PublicAccount_FileFoundByName;

		{UpdateListing tests}
		[Test]
		procedure TestFetchItem_UpdateListingFalse_DoesNotRefresh;
	end;

implementation

uses
	SysUtils;

{TMockFetcherLogger}

constructor TMockFetcherLogger.Create;
begin
	inherited Create;
	LogCalls := 0;
end;

procedure TMockFetcherLogger.Log(LogLevel, MsgType: Integer; Msg: WideString; const Params: array of const);
begin
	Inc(LogCalls);
end;

procedure TMockFetcherLogger.Log(LogLevel, MsgType: Integer; Msg: WideString);
begin
	Inc(LogCalls);
end;

procedure TMockFetcherLogger.LogError(Msg: WideString);
begin
	Inc(LogCalls);
end;

{TListingItemFetcherTest}

function TListingItemFetcherTest.CreateFileItem(const Name, HomePath: WideString): TCMRDirItem;
begin
	FillChar(Result, SizeOf(Result), 0);
	Result.name := Name;
	Result.home := HomePath;
	Result.type_ := TYPE_FILE;
end;

function TListingItemFetcherTest.CreateDirItem(const Name, HomePath: WideString): TCMRDirItem;
begin
	FillChar(Result, SizeOf(Result), 0);
	Result.name := Name;
	Result.home := HomePath;
	Result.type_ := TYPE_DIR;
end;

function TListingItemFetcherTest.CreateCloud(PublicAccount: Boolean): TCloudMailRu;
var
	Settings: TCloudSettings;
begin
	Settings := Default(TCloudSettings);
	Settings.AccountSettings.PublicAccount := PublicAccount;

	Result := TCloudMailRu.Create(
		Settings,
		FMockHTTPManager,
		TNullAuthStrategy.Create,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create);
end;

procedure TListingItemFetcherTest.Setup;
begin
	FLogger := TMockFetcherLogger.Create;
	FFetcher := TListingItemFetcher.Create(FLogger);
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
	FCloud := nil;
end;

procedure TListingItemFetcherTest.TearDown;
begin
	FCloud.Free;
	FFetcher := nil;
	FMockHTTPManager := nil;
	FMockHTTP := nil;
	FLogger := nil;
end;

{Nil cloud tests}

procedure TListingItemFetcherTest.TestFetchItem_NilCloud_ReturnsNone;
var
	Listing: TCMRDirItemList;
	Path: TRealPath;
	Item: TCMRDirItem;
begin
	SetLength(Listing, 1);
	Listing[0] := CreateFileItem('test.txt', '/test.txt');
	Path.FromPath('\account\test.txt');

	Item := FFetcher.FetchItem(Listing, Path, nil, False);

	Assert.IsTrue(Item.isNone, 'Should return None when cloud is nil');
end;

{Search by home path tests (private account)}

procedure TListingItemFetcherTest.TestFetchItem_PrivateAccount_SearchesByHomePath;
var
	Listing: TCMRDirItemList;
	Path: TRealPath;
	Item: TCMRDirItem;
begin
	{Private accounts search by home path, not by name}
	FCloud := CreateCloud(False);
	SetLength(Listing, 2);
	Listing[0] := CreateFileItem('other.txt', '/folder/other.txt');
	Listing[1] := CreateFileItem('test.txt', '/folder/test.txt');
	Path.FromPath('\account\folder\test.txt');

	Item := FFetcher.FetchItem(Listing, Path, FCloud, False);

	Assert.AreEqual('/folder/test.txt', string(Item.home), 'Should find by home path');
end;

procedure TListingItemFetcherTest.TestFetchItem_PrivateAccount_FileFoundByHomePath;
var
	Listing: TCMRDirItemList;
	Path: TRealPath;
	Item: TCMRDirItem;
begin
	FCloud := CreateCloud(False);
	SetLength(Listing, 1);
	Listing[0] := CreateFileItem('document.pdf', '/docs/document.pdf');
	Path.FromPath('\account\docs\document.pdf');

	Item := FFetcher.FetchItem(Listing, Path, FCloud, False);

	Assert.IsFalse(Item.isNone, 'Should find the file');
	Assert.IsTrue(Item.isFile, 'Should be a file');
	Assert.AreEqual('document.pdf', string(Item.name));
end;

procedure TListingItemFetcherTest.TestFetchItem_PrivateAccount_DirFoundByHomePath;
var
	Listing: TCMRDirItemList;
	Path: TRealPath;
	Item: TCMRDirItem;
begin
	FCloud := CreateCloud(False);
	SetLength(Listing, 1);
	Listing[0] := CreateDirItem('subfolder', '/parent/subfolder');
	Path.FromPath('\account\parent\subfolder');

	Item := FFetcher.FetchItem(Listing, Path, FCloud, False);

	Assert.IsFalse(Item.isNone, 'Should find the directory');
	Assert.IsTrue(Item.isDir, 'Should be a directory');
	Assert.AreEqual('subfolder', string(Item.name));
end;

procedure TListingItemFetcherTest.TestFetchItem_PrivateAccount_NotFoundReturnsNone;
var
	Listing: TCMRDirItemList;
	Path: TRealPath;
	Item: TCMRDirItem;
begin
	FCloud := CreateCloud(False);
	SetLength(Listing, 1);
	Listing[0] := CreateFileItem('other.txt', '/folder/other.txt');
	Path.FromPath('\account\folder\missing.txt');

	Item := FFetcher.FetchItem(Listing, Path, FCloud, False);

	Assert.IsTrue(Item.isNone, 'Should return None when item not found');
end;

{Search by name tests (public account)}

procedure TListingItemFetcherTest.TestFetchItem_PublicAccount_SearchesByName;
var
	Listing: TCMRDirItemList;
	Path: TRealPath;
	Item: TCMRDirItem;
begin
	{Public accounts search by name, not by home path}
	FCloud := CreateCloud(True);
	SetLength(Listing, 2);
	Listing[0] := CreateFileItem('first.txt', '/some/path/first.txt');
	Listing[1] := CreateFileItem('second.txt', '/another/path/second.txt');
	Path.FromPath('\public_account\second.txt');

	Item := FFetcher.FetchItem(Listing, Path, FCloud, False);

	Assert.AreEqual('second.txt', string(Item.name), 'Should find by name');
end;

procedure TListingItemFetcherTest.TestFetchItem_PublicAccount_FileFoundByName;
var
	Listing: TCMRDirItemList;
	Path: TRealPath;
	Item: TCMRDirItem;
begin
	FCloud := CreateCloud(True);
	SetLength(Listing, 1);
	Listing[0] := CreateFileItem('public_file.txt', '');
	Path.FromPath('\public_account\public_file.txt');

	Item := FFetcher.FetchItem(Listing, Path, FCloud, False);

	Assert.IsFalse(Item.isNone, 'Should find the file');
	Assert.AreEqual('public_file.txt', string(Item.name));
end;

{UpdateListing tests}

procedure TListingItemFetcherTest.TestFetchItem_UpdateListingFalse_DoesNotRefresh;
var
	Listing: TCMRDirItemList;
	Path: TRealPath;
	Item: TCMRDirItem;
begin
	{When UpdateListing is False and item not found, should not call cloud APIs}
	FCloud := CreateCloud(False);
	SetLength(Listing, 0);
	Path.FromPath('\account\folder\missing.txt');

	Item := FFetcher.FetchItem(Listing, Path, FCloud, False);

	Assert.IsTrue(Item.isNone, 'Should return None without refreshing');
	Assert.AreEqual(0, FMockHTTP.GetCallCount, 'Should not make any HTTP calls');
end;

initialization
	TDUnitX.RegisterTestFixture(TListingItemFetcherTest);

end.

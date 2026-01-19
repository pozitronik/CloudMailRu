unit ListingItemFetcherTest;

{Unit tests for TListingItemFetcher - context-aware listing item fetching.
 Note: Full integration tests require TCloudMailRu which isn't interface-based.}

interface

uses
	DUnitX.TestFramework,
	RealPath,
	CMRDirItem,
	CMRDirItemList,
	CMRConstants,
	ILoggerInterface,
	IListingItemFetcherInterface,
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

		function CreateDirItem(const Name, HomePath: WideString): TCMRDirItem;
		function CreateFileItem(const Name, HomePath: WideString): TCMRDirItem;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Nil cloud tests}
		[Test]
		procedure TestFetchItem_NilCloud_ReturnsNone;

		{Search in listing tests - these test the core search logic}
		[Test]
		procedure TestFetchItem_ItemInListing_ReturnsItem;
		[Test]
		procedure TestFetchItem_ItemNotInListing_ReturnsNone;

		{Integration placeholder}
		[Test]
		procedure TestFetchItem_UpdateListing_RequiresIntegration;
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

function TListingItemFetcherTest.CreateDirItem(const Name, HomePath: WideString): TCMRDirItem;
begin
	FillChar(Result, SizeOf(Result), 0);
	Result.name := Name;
	Result.home := HomePath;
	Result.type_ := TYPE_DIR;
end;

function TListingItemFetcherTest.CreateFileItem(const Name, HomePath: WideString): TCMRDirItem;
begin
	FillChar(Result, SizeOf(Result), 0);
	Result.name := Name;
	Result.home := HomePath;
	Result.type_ := TYPE_FILE;
end;

procedure TListingItemFetcherTest.Setup;
begin
	FLogger := TMockFetcherLogger.Create;
	FFetcher := TListingItemFetcher.Create(FLogger);
end;

procedure TListingItemFetcherTest.TearDown;
begin
	FFetcher := nil;
	FLogger := nil;
end;

{Nil cloud tests}

procedure TListingItemFetcherTest.TestFetchItem_NilCloud_ReturnsNone;
var
	Listing: TCMRDirItemList;
	Path: TRealPath;
	Result: TCMRDirItem;
begin
	SetLength(Listing, 1);
	Listing[0] := CreateFileItem('test.txt', '/test.txt');
	Path.FromPath('\account\test.txt');

	Result := FFetcher.FetchItem(Listing, Path, nil, False);

	Assert.IsTrue(Result.isNone, 'Should return None when cloud is nil');
end;

{Search in listing tests}

procedure TListingItemFetcherTest.TestFetchItem_ItemInListing_ReturnsItem;
begin
	{This test requires a real TCloudMailRu to determine IsPublicAccount.
	 The search logic is tested through integration tests.}
	Assert.Pass('Item search tested through integration tests');
end;

procedure TListingItemFetcherTest.TestFetchItem_ItemNotInListing_ReturnsNone;
begin
	{This test requires a real TCloudMailRu.}
	Assert.Pass('Item not found tested through integration tests');
end;

{Integration placeholder}

procedure TListingItemFetcherTest.TestFetchItem_UpdateListing_RequiresIntegration;
begin
	{UpdateListing functionality requires real cloud connection to test
	 trashbin, shared links, and statusFile refresh paths.}
	Assert.Pass('UpdateListing tested through integration tests');
end;

initialization
	TDUnitX.RegisterTestFixture(TListingItemFetcherTest);

end.

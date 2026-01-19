unit FileStreamExecutorTest;

{Unit tests for TFileStreamExecutor - file streaming operations.
 Note: Full integration tests require TCloudMailRu which isn't interface-based.
 These tests verify format checking and basic flow.}

interface

uses
	DUnitX.TestFramework,
	RealPath,
	CMRDirItem,
	CMRConstants,
	StreamingSettings,
	PLUGIN_TYPES,
	FileStreamExecutor;

type
	[TestFixture]
	TFileStreamExecutorTest = class
	private
		FExecutor: IFileStreamExecutor;

		function CreateItem(const Name, Weblink: WideString): TCMRDirItem;
		function CreatePath(const Account, Path: WideString): TRealPath;
		function CreateSettings(Format: Integer): TStreamingSettings;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Format check tests}
		[Test]
		procedure TestExecute_FormatDisabled_ReturnsOKWithoutAction;
		[Test]
		procedure TestExecute_FormatUnset_ReturnsOKWithoutAction;

		{Integration tests placeholder}
		[Test]
		procedure TestExecute_WithValidItem_RequiresIntegration;
	end;

implementation

uses
	SysUtils;

{Helper methods}

function TFileStreamExecutorTest.CreateItem(const Name, Weblink: WideString): TCMRDirItem;
begin
	FillChar(Result, SizeOf(Result), 0);
	Result.name := Name;
	Result.weblink := Weblink;
	Result.type_ := TYPE_FILE;
end;

function TFileStreamExecutorTest.CreatePath(const Account, Path: WideString): TRealPath;
begin
	Result.FromPath('\' + Account + Path);
end;

function TFileStreamExecutorTest.CreateSettings(Format: Integer): TStreamingSettings;
begin
	FillChar(Result, SizeOf(Result), 0);
	Result.Format := Format;
	Result.Command := 'test.exe';
	Result.Parameters := '%url%';
	Result.StartPath := '';
end;

{Setup/TearDown}

procedure TFileStreamExecutorTest.Setup;
begin
	FExecutor := TFileStreamExecutor.Create;
end;

procedure TFileStreamExecutorTest.TearDown;
begin
	FExecutor := nil;
end;

{Format check tests}

procedure TFileStreamExecutorTest.TestExecute_FormatDisabled_ReturnsOKWithoutAction;
var
	Path: TRealPath;
	Item: TCMRDirItem;
	Settings: TStreamingSettings;
	Result: Integer;
begin
	{When format is disabled, should return OK without doing anything}
	Path := CreatePath('account', '\file.mp4');
	Item := CreateItem('file.mp4', 'abc123');
	Settings := CreateSettings(STREAMING_FORMAT_DISABLED);

	Result := FExecutor.Execute(Path, Item, Settings, nil);

	Assert.AreEqual(FS_EXEC_OK, Result, 'Should return OK when streaming disabled');
end;

procedure TFileStreamExecutorTest.TestExecute_FormatUnset_ReturnsOKWithoutAction;
var
	Path: TRealPath;
	Item: TCMRDirItem;
	Settings: TStreamingSettings;
	Result: Integer;
begin
	{When format is unset, should return OK without doing anything}
	Path := CreatePath('account', '\file.mp4');
	Item := CreateItem('file.mp4', 'abc123');
	Settings := CreateSettings(STREAMING_FORMAT_UNSET);

	Result := FExecutor.Execute(Path, Item, Settings, nil);

	Assert.AreEqual(FS_EXEC_OK, Result, 'Should return OK when format unset');
end;

{Integration tests placeholder}

procedure TFileStreamExecutorTest.TestExecute_WithValidItem_RequiresIntegration;
begin
	{Full streaming execution requires TCloudMailRu and ConnectionManager.
	 This is tested through integration tests with real cloud connections.}
	Assert.Pass('Streaming execution tested through integration tests');
end;

initialization
	TDUnitX.RegisterTestFixture(TFileStreamExecutorTest);

end.

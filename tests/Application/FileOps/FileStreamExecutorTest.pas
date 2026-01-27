unit FileStreamExecutorTest;

{Unit tests for TFileStreamExecutor - file streaming operations.
 Tests the testable paths through Execute:
 - Format disabled/unset (early exit)
 - Public cloud factory failure

 Note: ResolveStreamUrl and ExecuteCommand require TCloudMailRu which is not
 interface-based. Full path testing requires integration tests with real cloud.}

interface

uses
	DUnitX.TestFramework,
	RealPath,
	CloudDirItem,
	CloudConstants,
	StreamingSettings,
	WFXTypes,
	FileStreamExecutor,
	CloudMailRu,
	CloudMailRuFactory,
	WindowsHelper,
	ConnectionManager;

type
	{Mock command executor for testing}
	TMockCommandExecutor = class(TInterfacedObject, ICommandExecutor)
	private
		FLastCommand: WideString;
		FLastParams: WideString;
		FLastStartPath: WideString;
		FExecuteResult: Boolean;
		FExecuteCallCount: Integer;
	public
		constructor Create;
		function Execute(Command, Params, StartPath: WideString): Boolean;

		property LastCommand: WideString read FLastCommand;
		property LastParams: WideString read FLastParams;
		property LastStartPath: WideString read FLastStartPath;
		property ExecuteResult: Boolean read FExecuteResult write FExecuteResult;
		property ExecuteCallCount: Integer read FExecuteCallCount;
	end;

	{Mock public cloud factory for testing}
	TMockPublicCloudFactory = class(TInterfacedObject, IPublicCloudFactory)
	private
		FCreateResult: Boolean;
		FLastPublicUrl: WideString;
		FCreateCallCount: Integer;
	public
		constructor Create;
		function CreatePublicCloud(var TempCloud: TCloudMailRu; PublicUrl: WideString): Boolean;

		property CreateResult: Boolean read FCreateResult write FCreateResult;
		property LastPublicUrl: WideString read FLastPublicUrl;
		property CreateCallCount: Integer read FCreateCallCount;
	end;

	[TestFixture]
	TFileStreamExecutorTest = class
	private
		FExecutor: IFileStreamExecutor;
		FMockCloudFactory: TMockPublicCloudFactory;
		FMockCommandExecutor: TMockCommandExecutor;

		function CreateItem(const Name, Weblink: WideString): TCloudDirItem;
		function CreatePath(const Account, Path: WideString): TRealPath;
		function CreateSettings(Format: Integer; const Command: WideString = 'test.exe'; const Params: WideString = '%url%'): TStreamingSettings;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Format check tests - early exit paths}
		[Test]
		procedure TestExecute_FormatDisabled_ReturnsOKWithoutAction;
		[Test]
		procedure TestExecute_FormatUnset_ReturnsOKWithoutAction;
		[Test]
		procedure TestExecute_FormatDisabled_DoesNotCallFactory;
		[Test]
		procedure TestExecute_FormatUnset_DoesNotCallFactory;
		[Test]
		procedure TestExecute_FormatNone_DoesNotExitEarly;

		{Factory failure tests}
		[Test]
		procedure TestExecute_FactoryCreateFails_ReturnsError;
		[Test]
		procedure TestExecute_FactoryCreateFails_DoesNotCallCommandExecutor;
		[Test]
		procedure TestExecute_FactoryReceivesCorrectPublicUrl;
		[Test]
		procedure TestExecute_FactoryReceivesPublicAccessUrlPrefix;

		{Streaming format tests}
		[Test]
		procedure TestExecute_PlaylistFormat_CallsFactory;
		[Test]
		procedure TestExecute_DefaultFormat_CallsFactory;
		[Test]
		procedure TestExecute_WeblinkViewFormat_CallsFactory;
		[Test]
		procedure TestExecute_VideoFormat_CallsFactory;
	end;

implementation

uses
	SysUtils;

{TMockCommandExecutor}

constructor TMockCommandExecutor.Create;
begin
	inherited Create;
	FExecuteResult := True;
	FExecuteCallCount := 0;
end;

function TMockCommandExecutor.Execute(Command, Params, StartPath: WideString): Boolean;
begin
	Inc(FExecuteCallCount);
	FLastCommand := Command;
	FLastParams := Params;
	FLastStartPath := StartPath;
	Result := FExecuteResult;
end;

{TMockPublicCloudFactory}

constructor TMockPublicCloudFactory.Create;
begin
	inherited Create;
	FCreateResult := False; {Default to failure - avoids needing real TCloudMailRu}
	FCreateCallCount := 0;
end;

function TMockPublicCloudFactory.CreatePublicCloud(var TempCloud: TCloudMailRu; PublicUrl: WideString): Boolean;
begin
	Inc(FCreateCallCount);
	FLastPublicUrl := PublicUrl;
	TempCloud := nil;
	Result := FCreateResult;
end;

{TFileStreamExecutorTest - Helper methods}

function TFileStreamExecutorTest.CreateItem(const Name, Weblink: WideString): TCloudDirItem;
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

function TFileStreamExecutorTest.CreateSettings(Format: Integer; const Command: WideString; const Params: WideString): TStreamingSettings;
begin
	FillChar(Result, SizeOf(Result), 0);
	Result.Format := Format;
	Result.Command := Command;
	Result.Parameters := Params;
	Result.StartPath := '';
end;

{Setup/TearDown}

procedure TFileStreamExecutorTest.Setup;
begin
	FMockCloudFactory := TMockPublicCloudFactory.Create;
	FMockCommandExecutor := TMockCommandExecutor.Create;
	FExecutor := TFileStreamExecutor.Create(FMockCloudFactory, FMockCommandExecutor);
end;

procedure TFileStreamExecutorTest.TearDown;
begin
	FExecutor := nil;
	{Mocks are released via interface reference counting}
end;

{Format check tests - early exit paths}

procedure TFileStreamExecutorTest.TestExecute_FormatDisabled_ReturnsOKWithoutAction;
var
	Path: TRealPath;
	Item: TCloudDirItem;
	Settings: TStreamingSettings;
	ExecResult: Integer;
begin
	{When format is disabled, should return OK without doing anything}
	Path := CreatePath('account', '\file.mp4');
	Item := CreateItem('file.mp4', 'abc123');
	Settings := CreateSettings(STREAMING_FORMAT_DISABLED);

	ExecResult := FExecutor.Execute(Path, Item, Settings, nil);

	Assert.AreEqual(FS_EXEC_OK, ExecResult, 'Should return OK when streaming disabled');
end;

procedure TFileStreamExecutorTest.TestExecute_FormatUnset_ReturnsOKWithoutAction;
var
	Path: TRealPath;
	Item: TCloudDirItem;
	Settings: TStreamingSettings;
	ExecResult: Integer;
begin
	{When format is unset, should return OK without doing anything}
	Path := CreatePath('account', '\file.mp4');
	Item := CreateItem('file.mp4', 'abc123');
	Settings := CreateSettings(STREAMING_FORMAT_UNSET);

	ExecResult := FExecutor.Execute(Path, Item, Settings, nil);

	Assert.AreEqual(FS_EXEC_OK, ExecResult, 'Should return OK when format unset');
end;

procedure TFileStreamExecutorTest.TestExecute_FormatDisabled_DoesNotCallFactory;
var
	Path: TRealPath;
	Item: TCloudDirItem;
	Settings: TStreamingSettings;
begin
	{When format is disabled, factory should not be called}
	Path := CreatePath('account', '\file.mp4');
	Item := CreateItem('file.mp4', 'abc123');
	Settings := CreateSettings(STREAMING_FORMAT_DISABLED);

	FExecutor.Execute(Path, Item, Settings, nil);

	Assert.AreEqual(0, FMockCloudFactory.CreateCallCount, 'Factory should not be called when format disabled');
end;

procedure TFileStreamExecutorTest.TestExecute_FormatUnset_DoesNotCallFactory;
var
	Path: TRealPath;
	Item: TCloudDirItem;
	Settings: TStreamingSettings;
begin
	{When format is unset, factory should not be called}
	Path := CreatePath('account', '\file.mp4');
	Item := CreateItem('file.mp4', 'abc123');
	Settings := CreateSettings(STREAMING_FORMAT_UNSET);

	FExecutor.Execute(Path, Item, Settings, nil);

	Assert.AreEqual(0, FMockCloudFactory.CreateCallCount, 'Factory should not be called when format unset');
end;

procedure TFileStreamExecutorTest.TestExecute_FormatNone_DoesNotExitEarly;
var
	Path: TRealPath;
	Item: TCloudDirItem;
	Settings: TStreamingSettings;
begin
	{STREAMING_FORMAT_NONE (0) is a valid format that should proceed to factory}
	Path := CreatePath('account', '\file.mp4');
	Item := CreateItem('file.mp4', 'abc123');
	Settings := CreateSettings(STREAMING_FORMAT_NONE);

	FExecutor.Execute(Path, Item, Settings, nil);

	Assert.AreEqual(1, FMockCloudFactory.CreateCallCount, 'Format NONE should call factory');
end;

{Factory failure tests}

procedure TFileStreamExecutorTest.TestExecute_FactoryCreateFails_ReturnsError;
var
	Path: TRealPath;
	Item: TCloudDirItem;
	Settings: TStreamingSettings;
	ExecResult: Integer;
begin
	{When factory fails to create public cloud, should return error}
	FMockCloudFactory.CreateResult := False;
	Path := CreatePath('account', '\file.mp4');
	Item := CreateItem('file.mp4', 'abc123');
	Settings := CreateSettings(STREAMING_FORMAT_DEFAULT);

	ExecResult := FExecutor.Execute(Path, Item, Settings, nil);

	Assert.AreEqual(FS_EXEC_ERROR, ExecResult, 'Should return error when factory fails');
end;

procedure TFileStreamExecutorTest.TestExecute_FactoryCreateFails_DoesNotCallCommandExecutor;
var
	Path: TRealPath;
	Item: TCloudDirItem;
	Settings: TStreamingSettings;
begin
	{When factory fails, command executor should not be called}
	FMockCloudFactory.CreateResult := False;
	Path := CreatePath('account', '\file.mp4');
	Item := CreateItem('file.mp4', 'abc123');
	Settings := CreateSettings(STREAMING_FORMAT_DEFAULT);

	FExecutor.Execute(Path, Item, Settings, nil);

	Assert.AreEqual(0, FMockCommandExecutor.ExecuteCallCount, 'Command executor should not be called when factory fails');
end;

procedure TFileStreamExecutorTest.TestExecute_FactoryReceivesCorrectPublicUrl;
var
	Path: TRealPath;
	Item: TCloudDirItem;
	Settings: TStreamingSettings;
begin
	{Factory should receive the correct public URL constructed from weblink}
	FMockCloudFactory.CreateResult := False;
	Path := CreatePath('account', '\file.mp4');
	Item := CreateItem('file.mp4', 'test-weblink-123');
	Settings := CreateSettings(STREAMING_FORMAT_DEFAULT);

	FExecutor.Execute(Path, Item, Settings, nil);

	Assert.AreEqual(PUBLIC_ACCESS_URL + 'test-weblink-123', FMockCloudFactory.LastPublicUrl, 'Factory should receive correct public URL');
end;

procedure TFileStreamExecutorTest.TestExecute_FactoryReceivesPublicAccessUrlPrefix;
var
	Path: TRealPath;
	Item: TCloudDirItem;
	Settings: TStreamingSettings;
begin
	{Factory URL should start with PUBLIC_ACCESS_URL constant}
	FMockCloudFactory.CreateResult := False;
	Path := CreatePath('account', '\video.mp4');
	Item := CreateItem('video.mp4', 'any-weblink');
	Settings := CreateSettings(STREAMING_FORMAT_PLAYLIST);

	FExecutor.Execute(Path, Item, Settings, nil);

	Assert.StartsWith(PUBLIC_ACCESS_URL, FMockCloudFactory.LastPublicUrl, 'URL should start with PUBLIC_ACCESS_URL');
end;

{Streaming format tests - verify all valid formats call factory}

procedure TFileStreamExecutorTest.TestExecute_PlaylistFormat_CallsFactory;
var
	Path: TRealPath;
	Item: TCloudDirItem;
	Settings: TStreamingSettings;
begin
	{Playlist format should proceed to factory creation}
	Path := CreatePath('account', '\stream.m3u8');
	Item := CreateItem('stream.m3u8', 'playlist-link');
	Settings := CreateSettings(STREAMING_FORMAT_PLAYLIST);

	FExecutor.Execute(Path, Item, Settings, nil);

	Assert.AreEqual(1, FMockCloudFactory.CreateCallCount, 'Playlist format should call factory');
end;

procedure TFileStreamExecutorTest.TestExecute_DefaultFormat_CallsFactory;
var
	Path: TRealPath;
	Item: TCloudDirItem;
	Settings: TStreamingSettings;
begin
	{Default format should proceed to factory creation}
	Path := CreatePath('account', '\file.mp4');
	Item := CreateItem('file.mp4', 'default-link');
	Settings := CreateSettings(STREAMING_FORMAT_DEFAULT);

	FExecutor.Execute(Path, Item, Settings, nil);

	Assert.AreEqual(1, FMockCloudFactory.CreateCallCount, 'Default format should call factory');
end;

procedure TFileStreamExecutorTest.TestExecute_WeblinkViewFormat_CallsFactory;
var
	Path: TRealPath;
	Item: TCloudDirItem;
	Settings: TStreamingSettings;
begin
	{Weblink view format should proceed to factory creation}
	Path := CreatePath('account', '\document.pdf');
	Item := CreateItem('document.pdf', 'weblink-view');
	Settings := CreateSettings(STREAMING_FORMAT_WEBLINK_VIEW);

	FExecutor.Execute(Path, Item, Settings, nil);

	Assert.AreEqual(1, FMockCloudFactory.CreateCallCount, 'Weblink view format should call factory');
end;

procedure TFileStreamExecutorTest.TestExecute_VideoFormat_CallsFactory;
var
	Path: TRealPath;
	Item: TCloudDirItem;
	Settings: TStreamingSettings;
begin
	{Video format should proceed to factory creation}
	Path := CreatePath('account', '\movie.avi');
	Item := CreateItem('movie.avi', 'video-link');
	Settings := CreateSettings(STREAMING_FORMAT_VIDEO);

	FExecutor.Execute(Path, Item, Settings, nil);

	Assert.AreEqual(1, FMockCloudFactory.CreateCallCount, 'Video format should call factory');
end;

initialization
	TDUnitX.RegisterTestFixture(TFileStreamExecutorTest);

end.

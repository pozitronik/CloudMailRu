unit OperationStatusContextBuilderTest;

{Tests for TOperationStatusContextBuilder.
 Verifies context is built correctly from path and settings.}

interface

uses
	System.Classes,
	DUnitX.TestFramework,
	OperationStatusContextBuilder,
	OperationLifecycleHandler,
	PluginSettingsManager,
	MockConnectionManager,
	RealPath,
	StreamingSettings;

type
	[TestFixture]
	TOperationStatusContextBuilderTest = class
	private
		FBuilder: IOperationStatusContextBuilder;
		FMockConnectionManager: TMockConnectionManager;
		FSettingsManager: IPluginSettingsManager;
	public
		[Setup]
		procedure Setup;

		[TearDown]
		procedure TearDown;

		[Test]
		procedure BuildContext_SetsOperationFromParameter;

		[Test]
		procedure BuildContext_WhenPathIsInAccount_SetsIsInAccountTrue;

		[Test]
		procedure BuildContext_WhenPathIsNotInAccount_SetsIsInAccountFalse;

		[Test]
		procedure BuildContext_GetsDescriptionsEnabledFromSettings;

		[Test]
		procedure BuildContext_GetsLogUserSpaceEnabledFromSettings;

		[Test]
		procedure BuildContext_WhenNotInAccount_SetsIsPublicAccountFalse;
	end;

implementation

uses
	SysUtils,
	PluginSettings,
	CloudMailRu,
	CloudSettings,
	AuthStrategy,
	WindowsFileSystem,
	TCLogger,
	TCProgress,
	TCRequest,
	TCHandler;

type
	{Test settings manager with configurable values}
	TTestPluginSettingsManager = class(TInterfacedObject, IPluginSettingsManager)
	private
		FSettings: TPluginSettings;
	public
		constructor Create(DescriptionEnabled, LogUserSpace: Boolean);
		function GetSettings: TPluginSettings;
		procedure SetSettings(Value: TPluginSettings);
		procedure Save;
		procedure SwitchProxyPasswordStorage;
		function GetStreamingSettings(const FileName: WideString): TStreamingSettings;
		procedure SetStreamingSettings(const FileName: WideString; StreamSettings: TStreamingSettings);
		procedure GetStreamingExtensionsList(ExtensionsList: TStrings);
		procedure RemoveStreamingExtension(const Extension: WideString);
		function GetAccountsIniFilePath: WideString;
		procedure Refresh;
	end;

constructor TTestPluginSettingsManager.Create(DescriptionEnabled, LogUserSpace: Boolean);
begin
	inherited Create;
	FSettings.DescriptionEnabled := DescriptionEnabled;
	FSettings.LogUserSpace := LogUserSpace;
end;

function TTestPluginSettingsManager.GetSettings: TPluginSettings;
begin
	Result := FSettings;
end;

procedure TTestPluginSettingsManager.SetSettings(Value: TPluginSettings);
begin
	FSettings := Value;
end;

procedure TTestPluginSettingsManager.Save;
begin
	{No-op}
end;

procedure TTestPluginSettingsManager.SwitchProxyPasswordStorage;
begin
	{No-op}
end;

function TTestPluginSettingsManager.GetStreamingSettings(const FileName: WideString): TStreamingSettings;
begin
	Result := Default(TStreamingSettings);
end;

procedure TTestPluginSettingsManager.SetStreamingSettings(const FileName: WideString; StreamSettings: TStreamingSettings);
begin
	{No-op}
end;

procedure TTestPluginSettingsManager.GetStreamingExtensionsList(ExtensionsList: TStrings);
begin
	ExtensionsList.Clear;
end;

procedure TTestPluginSettingsManager.RemoveStreamingExtension(const Extension: WideString);
begin
	{No-op}
end;

function TTestPluginSettingsManager.GetAccountsIniFilePath: WideString;
begin
	Result := EmptyWideStr;
end;

procedure TTestPluginSettingsManager.Refresh;
begin
	{No-op}
end;

procedure TOperationStatusContextBuilderTest.Setup;
begin
	FMockConnectionManager := TMockConnectionManager.Create;
	FSettingsManager := TTestPluginSettingsManager.Create(True, True);
	FBuilder := TOperationStatusContextBuilder.Create(FSettingsManager, FMockConnectionManager);
end;

procedure TOperationStatusContextBuilderTest.TearDown;
begin
	FBuilder := nil;
	FSettingsManager := nil;
	FMockConnectionManager := nil;
end;

procedure TOperationStatusContextBuilderTest.BuildContext_SetsOperationFromParameter;
var
	Path: TRealPath;
	Context: TOperationContext;
begin
	Path.FromPath('\');

	Context := FBuilder.BuildContext(Path, 42);

	Assert.AreEqual(42, Context.Operation, 'Operation should be set from parameter');
end;

procedure TOperationStatusContextBuilderTest.BuildContext_WhenPathIsInAccount_SetsIsInAccountTrue;
var
	Path: TRealPath;
	Context: TOperationContext;
	MockCloud: TCloudMailRu;
	Settings: TCloudSettings;
begin
	Path.FromPath('\account\folder');
	{Need to configure a cloud instance since BuildContext accesses it for IsPublicAccount}
	Settings := Default(TCloudSettings);
	MockCloud := TCloudMailRu.Create(Settings, nil, TNullAuthStrategy.Create,
		TNullFileSystem.Create, TNullLogger.Create, TNullProgress.Create, TNullRequest.Create, TNullTCHandler.Create);
	try
		FMockConnectionManager.SetCloud('account', MockCloud);

		Context := FBuilder.BuildContext(Path, 0);

		Assert.IsTrue(Context.IsInAccount, 'IsInAccount should be true for account paths');
	finally
		MockCloud.Free;
	end;
end;

procedure TOperationStatusContextBuilderTest.BuildContext_WhenPathIsNotInAccount_SetsIsInAccountFalse;
var
	Path: TRealPath;
	Context: TOperationContext;
begin
	Path.FromPath('\');

	Context := FBuilder.BuildContext(Path, 0);

	Assert.IsFalse(Context.IsInAccount, 'IsInAccount should be false for root path');
end;

procedure TOperationStatusContextBuilderTest.BuildContext_GetsDescriptionsEnabledFromSettings;
var
	Path: TRealPath;
	Context: TOperationContext;
	SettingsWithDescEnabled: IPluginSettingsManager;
	BuilderWithDesc: IOperationStatusContextBuilder;
begin
	SettingsWithDescEnabled := TTestPluginSettingsManager.Create(True, False);
	BuilderWithDesc := TOperationStatusContextBuilder.Create(SettingsWithDescEnabled, FMockConnectionManager);
	Path.FromPath('\');

	Context := BuilderWithDesc.BuildContext(Path, 0);

	Assert.IsTrue(Context.DescriptionsEnabled, 'DescriptionsEnabled should come from settings');
end;

procedure TOperationStatusContextBuilderTest.BuildContext_GetsLogUserSpaceEnabledFromSettings;
var
	Path: TRealPath;
	Context: TOperationContext;
	SettingsWithLog: IPluginSettingsManager;
	BuilderWithLog: IOperationStatusContextBuilder;
begin
	SettingsWithLog := TTestPluginSettingsManager.Create(False, True);
	BuilderWithLog := TOperationStatusContextBuilder.Create(SettingsWithLog, FMockConnectionManager);
	Path.FromPath('\');

	Context := BuilderWithLog.BuildContext(Path, 0);

	Assert.IsTrue(Context.LogUserSpaceEnabled, 'LogUserSpaceEnabled should come from settings');
end;

procedure TOperationStatusContextBuilderTest.BuildContext_WhenNotInAccount_SetsIsPublicAccountFalse;
var
	Path: TRealPath;
	Context: TOperationContext;
begin
	Path.FromPath('\');

	Context := FBuilder.BuildContext(Path, 0);

	Assert.IsFalse(Context.IsPublicAccount, 'IsPublicAccount should be false when not in account');
end;

initialization
	TDUnitX.RegisterTestFixture(TOperationStatusContextBuilderTest);

end.

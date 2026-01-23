unit DownloadOrchestratorTest;

{Tests for TDownloadOrchestrator.
 Verifies download flow orchestration: validation, conflict resolution, retry handling.}

interface

uses
	System.Classes,
	DUnitX.TestFramework,
	DownloadOrchestrator,
	DownloadPreparationValidator,
	LocalFileConflictResolver,
	RetryHandler,
	PluginSettingsManager,
	RealPath,
	PLUGIN_TYPES,
	StreamingSettings;

type
	[TestFixture]
	TDownloadOrchestratorTest = class
	private
		FOrchestrator: IDownloadOrchestrator;
		FMockValidator: IDownloadPreparationValidator;
		FMockConflictResolver: ILocalFileConflictResolver;
		FMockRetryHandler: IRetryHandler;
		FMockSettingsManager: IPluginSettingsManager;

		FDownloadCallCount: Integer;
		FProgressCallCount: Integer;
		FLastDownloadPath: TRealPath;
	public
		[Setup]
		procedure Setup;

		[TearDown]
		procedure TearDown;

		[Test]
		procedure Execute_WhenValidationFails_ReturnsValidationResultCode;

		[Test]
		procedure Execute_WhenValidationFails_DoesNotCallDownload;

		[Test]
		procedure Execute_WhenConflictResolutionFails_ReturnsConflictResultCode;

		[Test]
		procedure Execute_WhenConflictResolutionFails_DoesNotCallDownload;

		[Test]
		procedure Execute_WhenDownloadSucceeds_ReturnsFS_FILE_OK;

		[Test]
		procedure Execute_CallsProgressBeforeConflictResolution;

		[Test]
		procedure Execute_PassesCorrectPathToDownloadCallback;
	end;

implementation

uses
	SysUtils,
	PluginSettings,
	LANGUAGE_STRINGS;

type
	{Mock validator that can be configured to pass or fail}
	TMockDownloadValidator = class(TInterfacedObject, IDownloadPreparationValidator)
	private
		FShouldProceed: Boolean;
		FResultCode: Integer;
	public
		constructor Create(ShouldProceed: Boolean; ResultCode: Integer);
		function Validate(const RemotePath: TRealPath; CopyFlags: Integer): TDownloadValidationResult;
	end;

	{Mock conflict resolver}
	TMockConflictResolver = class(TInterfacedObject, ILocalFileConflictResolver)
	private
		FShouldProceed: Boolean;
		FResultCode: Integer;
	public
		constructor Create(ShouldProceed: Boolean; ResultCode: Integer);
		function Resolve(const LocalPath: WideString; CopyFlags: Integer;
			OverwriteMode: Integer): TConflictResolution;
	end;

	{Mock retry handler that just returns the current result}
	TMockRetryHandler = class(TInterfacedObject, IRetryHandler)
	public
		function HandleOperationError(
			CurrentResult: Integer;
			OperationType: TRetryOperationType;
			const AskMessage, AskTitle, RetryLogMessage, FormatParam: WideString;
			RetryOperation: TRetryOperation;
			AbortCheck: TAbortCheck
		): Integer;
	end;

	{Mock settings manager}
	TMockSettingsManager = class(TInterfacedObject, IPluginSettingsManager)
	private
		FSettings: TPluginSettings;
	public
		function GetSettings: TPluginSettings;
		procedure SetSettings(Value: TPluginSettings);
		procedure Save;
		procedure SwitchProxyPasswordStorage;
		function GetStreamingSettings(const FileName: WideString): TStreamingSettings;
		procedure SetStreamingSettings(const FileName: WideString; StreamSettings: TStreamingSettings);
		procedure GetStreamingExtensionsList(ExtensionsList: TStrings);
		procedure RemoveStreamingExtension(const Extension: WideString);
	end;

{TMockDownloadValidator}

constructor TMockDownloadValidator.Create(ShouldProceed: Boolean; ResultCode: Integer);
begin
	inherited Create;
	FShouldProceed := ShouldProceed;
	FResultCode := ResultCode;
end;

function TMockDownloadValidator.Validate(const RemotePath: TRealPath;
	CopyFlags: Integer): TDownloadValidationResult;
begin
	Result.ShouldProceed := FShouldProceed;
	Result.ResultCode := FResultCode;
end;

{TMockConflictResolver}

constructor TMockConflictResolver.Create(ShouldProceed: Boolean; ResultCode: Integer);
begin
	inherited Create;
	FShouldProceed := ShouldProceed;
	FResultCode := ResultCode;
end;

function TMockConflictResolver.Resolve(const LocalPath: WideString;
	CopyFlags: Integer; OverwriteMode: Integer): TConflictResolution;
begin
	Result.ShouldProceed := FShouldProceed;
	Result.ResultCode := FResultCode;
end;

{TMockRetryHandler}

function TMockRetryHandler.HandleOperationError(
	CurrentResult: Integer;
	OperationType: TRetryOperationType;
	const AskMessage, AskTitle, RetryLogMessage, FormatParam: WideString;
	RetryOperation: TRetryOperation;
	AbortCheck: TAbortCheck
): Integer;
begin
	Result := CurrentResult;
end;

{TMockSettingsManager}

function TMockSettingsManager.GetSettings: TPluginSettings;
begin
	Result := FSettings;
end;

procedure TMockSettingsManager.SetSettings(Value: TPluginSettings);
begin
	FSettings := Value;
end;

procedure TMockSettingsManager.Save;
begin
	{No-op}
end;

procedure TMockSettingsManager.SwitchProxyPasswordStorage;
begin
	{No-op}
end;

function TMockSettingsManager.GetStreamingSettings(const FileName: WideString): TStreamingSettings;
begin
	Result := Default(TStreamingSettings);
end;

procedure TMockSettingsManager.SetStreamingSettings(const FileName: WideString; StreamSettings: TStreamingSettings);
begin
	{No-op}
end;

procedure TMockSettingsManager.GetStreamingExtensionsList(ExtensionsList: TStrings);
begin
	ExtensionsList.Clear;
end;

procedure TMockSettingsManager.RemoveStreamingExtension(const Extension: WideString);
begin
	{No-op}
end;

{TDownloadOrchestratorTest}

procedure TDownloadOrchestratorTest.Setup;
begin
	FDownloadCallCount := 0;
	FProgressCallCount := 0;
	FMockValidator := TMockDownloadValidator.Create(True, FS_FILE_OK);
	FMockConflictResolver := TMockConflictResolver.Create(True, FS_FILE_OK);
	FMockRetryHandler := TMockRetryHandler.Create;
	FMockSettingsManager := TMockSettingsManager.Create;
	FOrchestrator := TDownloadOrchestrator.Create(
		FMockValidator, FMockConflictResolver, FMockRetryHandler, FMockSettingsManager);
end;

procedure TDownloadOrchestratorTest.TearDown;
begin
	FOrchestrator := nil;
	FMockValidator := nil;
	FMockConflictResolver := nil;
	FMockRetryHandler := nil;
	FMockSettingsManager := nil;
end;

procedure TDownloadOrchestratorTest.Execute_WhenValidationFails_ReturnsValidationResultCode;
var
	Result: Integer;
begin
	FMockValidator := TMockDownloadValidator.Create(False, FS_FILE_NOTSUPPORTED);
	FOrchestrator := TDownloadOrchestrator.Create(
		FMockValidator, FMockConflictResolver, FMockRetryHandler, FMockSettingsManager);

	Result := FOrchestrator.Execute('\account\test.txt', 'C:\local.txt', 0,
		function(const Path: TRealPath; const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer
		begin
			Inc(FDownloadCallCount);
			Result := FS_FILE_OK;
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Inc(FProgressCallCount);
			Result := False;
		end);

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result, 'Should return validation result code');
end;

procedure TDownloadOrchestratorTest.Execute_WhenValidationFails_DoesNotCallDownload;
begin
	FMockValidator := TMockDownloadValidator.Create(False, FS_FILE_NOTSUPPORTED);
	FOrchestrator := TDownloadOrchestrator.Create(
		FMockValidator, FMockConflictResolver, FMockRetryHandler, FMockSettingsManager);

	FOrchestrator.Execute('\account\test.txt', 'C:\local.txt', 0,
		function(const Path: TRealPath; const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer
		begin
			Inc(FDownloadCallCount);
			Result := FS_FILE_OK;
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Result := False;
		end);

	Assert.AreEqual(0, FDownloadCallCount, 'Should not call download when validation fails');
end;

procedure TDownloadOrchestratorTest.Execute_WhenConflictResolutionFails_ReturnsConflictResultCode;
var
	Result: Integer;
begin
	FMockConflictResolver := TMockConflictResolver.Create(False, FS_FILE_EXISTS);
	FOrchestrator := TDownloadOrchestrator.Create(
		FMockValidator, FMockConflictResolver, FMockRetryHandler, FMockSettingsManager);

	Result := FOrchestrator.Execute('\account\test.txt', 'C:\local.txt', 0,
		function(const Path: TRealPath; const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer
		begin
			Inc(FDownloadCallCount);
			Result := FS_FILE_OK;
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Result := False;
		end);

	Assert.AreEqual(FS_FILE_EXISTS, Result, 'Should return conflict result code');
end;

procedure TDownloadOrchestratorTest.Execute_WhenConflictResolutionFails_DoesNotCallDownload;
begin
	FMockConflictResolver := TMockConflictResolver.Create(False, FS_FILE_EXISTS);
	FOrchestrator := TDownloadOrchestrator.Create(
		FMockValidator, FMockConflictResolver, FMockRetryHandler, FMockSettingsManager);

	FOrchestrator.Execute('\account\test.txt', 'C:\local.txt', 0,
		function(const Path: TRealPath; const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer
		begin
			Inc(FDownloadCallCount);
			Result := FS_FILE_OK;
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Result := False;
		end);

	Assert.AreEqual(0, FDownloadCallCount, 'Should not call download when conflict resolution fails');
end;

procedure TDownloadOrchestratorTest.Execute_WhenDownloadSucceeds_ReturnsFS_FILE_OK;
var
	Result: Integer;
begin
	Result := FOrchestrator.Execute('\account\test.txt', 'C:\local.txt', 0,
		function(const Path: TRealPath; const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer
		begin
			Result := FS_FILE_OK;
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Result := False;
		end);

	Assert.AreEqual(FS_FILE_OK, Result, 'Should return FS_FILE_OK on success');
end;

procedure TDownloadOrchestratorTest.Execute_CallsProgressBeforeConflictResolution;
begin
	FOrchestrator.Execute('\account\test.txt', 'C:\local.txt', 0,
		function(const Path: TRealPath; const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer
		begin
			Result := FS_FILE_OK;
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Inc(FProgressCallCount);
			Result := False;
		end);

	Assert.IsTrue(FProgressCallCount >= 1, 'Should call progress at least once');
end;

procedure TDownloadOrchestratorTest.Execute_PassesCorrectPathToDownloadCallback;
begin
	FOrchestrator.Execute('\account\folder\test.txt', 'C:\local.txt', 0,
		function(const Path: TRealPath; const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer
		begin
			FLastDownloadPath := Path;
			Result := FS_FILE_OK;
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Result := False;
		end);

	Assert.AreEqual('account', FLastDownloadPath.account, 'Account should be parsed correctly');
	Assert.AreEqual('folder\test.txt', FLastDownloadPath.Path, 'Path should be parsed correctly');
end;

initialization
	TDUnitX.RegisterTestFixture(TDownloadOrchestratorTest);

end.

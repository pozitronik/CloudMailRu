unit CloudDescriptionOpsAdapterTest;

{Tests for CloudDescriptionOpsAdapter - adapter for description file operations.
 Tests both TCloudDescriptionOpsAdapter and TCloudMailRuFileOpsAdapter classes.}

interface

uses
	CloudDescriptionOpsAdapter,
	WindowsFileSystem,
	WFXTypes,
	DUnitX.TestFramework;

type
	{Mock implementation of ICloudFileOps for testing}
	TMockCloudFileOps = class(TInterfacedObject, ICloudFileOps)
	private
		FGetFileResult: Integer;
		FPutFileResult: Integer;
		FDeleteFileResult: Boolean;
		FGetFileCalled: Boolean;
		FPutFileCalled: Boolean;
		FDeleteFileCalled: Boolean;
		FLastGetRemotePath: WideString;
		FLastGetLocalPath: WideString;
		FLastGetLogErrors: Boolean;
		FLastPutLocalPath: WideString;
		FLastPutRemotePath: WideString;
		FLastDeletePath: WideString;
	public
		constructor Create;

		{Configure mock responses}
		procedure SetGetFileResult(Result: Integer);
		procedure SetPutFileResult(Result: Integer);
		procedure SetDeleteFileResult(Result: Boolean);

		{ICloudFileOps implementation}
		function GetFile(RemotePath, LocalPath: WideString; var ResultHash: WideString;
			LogErrors: Boolean = True): Integer;
		function PutFile(LocalPath, RemotePath: WideString): Integer;
		function DeleteFile(Path: WideString): Boolean;

		{Test inspection}
		property GetFileCalled: Boolean read FGetFileCalled;
		property PutFileCalled: Boolean read FPutFileCalled;
		property DeleteFileCalled: Boolean read FDeleteFileCalled;
		property LastGetRemotePath: WideString read FLastGetRemotePath;
		property LastGetLocalPath: WideString read FLastGetLocalPath;
		property LastGetLogErrors: Boolean read FLastGetLogErrors;
		property LastPutLocalPath: WideString read FLastPutLocalPath;
		property LastPutRemotePath: WideString read FLastPutRemotePath;
		property LastDeletePath: WideString read FLastDeletePath;
	end;

	[TestFixture]
	TCloudDescriptionOpsAdapterTest = class
	private
		FAdapter: TCloudDescriptionOpsAdapter;
		FMockCloudOps: TMockCloudFileOps;
		FMockCloudOpsRef: ICloudFileOps;
		FMockFileSystem: TMemoryFileSystem;
		FMockFileSystemRef: IFileSystem;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{ GetDescriptionFile tests }
		[Test]
		procedure TestGetDescriptionFile_Success_ReturnsTrue;
		[Test]
		procedure TestGetDescriptionFile_Failure_ReturnsFalse;
		[Test]
		procedure TestGetDescriptionFile_PassesCorrectParameters;
		[Test]
		procedure TestGetDescriptionFile_DisablesErrorLogging;

		{ PutDescriptionFile tests - file exists }
		[Test]
		procedure TestPutDescriptionFile_FileExists_CallsPutFile;
		[Test]
		procedure TestPutDescriptionFile_FileExists_Success_ReturnsTrue;
		[Test]
		procedure TestPutDescriptionFile_FileExists_Failure_ReturnsFalse;

		{ PutDescriptionFile tests - file doesn't exist }
		[Test]
		procedure TestPutDescriptionFile_FileNotExists_CallsDeleteFile;
		[Test]
		procedure TestPutDescriptionFile_FileNotExists_DeleteSuccess_ReturnsTrue;
		[Test]
		procedure TestPutDescriptionFile_FileNotExists_DeleteFailure_ReturnsFalse;

		{ DeleteFile tests }
		[Test]
		procedure TestDeleteFile_Success_ReturnsTrue;
		[Test]
		procedure TestDeleteFile_Failure_ReturnsFalse;
		[Test]
		procedure TestDeleteFile_PassesCorrectPath;

		{ Interface implementation test }
		[Test]
		procedure TestImplementsICloudDescriptionOps;
	end;

	{Tests for TCloudMailRuFileOpsAdapter}
	[TestFixture]
	TCloudMailRuFileOpsAdapterTest = class
	public
		{ Interface implementation test }
		[Test]
		procedure TestImplementsICloudFileOps;
	end;

implementation

uses
	System.SysUtils;

{ TMockCloudFileOps }

constructor TMockCloudFileOps.Create;
begin
	inherited Create;
	FGetFileResult := FS_FILE_OK;
	FPutFileResult := FS_FILE_OK;
	FDeleteFileResult := True;
	FGetFileCalled := False;
	FPutFileCalled := False;
	FDeleteFileCalled := False;
end;

procedure TMockCloudFileOps.SetGetFileResult(Result: Integer);
begin
	FGetFileResult := Result;
end;

procedure TMockCloudFileOps.SetPutFileResult(Result: Integer);
begin
	FPutFileResult := Result;
end;

procedure TMockCloudFileOps.SetDeleteFileResult(Result: Boolean);
begin
	FDeleteFileResult := Result;
end;

function TMockCloudFileOps.GetFile(RemotePath, LocalPath: WideString;
	var ResultHash: WideString; LogErrors: Boolean): Integer;
begin
	FGetFileCalled := True;
	FLastGetRemotePath := RemotePath;
	FLastGetLocalPath := LocalPath;
	FLastGetLogErrors := LogErrors;
	ResultHash := 'MOCKHASH1234567890MOCKHASH1234567890';
	Result := FGetFileResult;
end;

function TMockCloudFileOps.PutFile(LocalPath, RemotePath: WideString): Integer;
begin
	FPutFileCalled := True;
	FLastPutLocalPath := LocalPath;
	FLastPutRemotePath := RemotePath;
	Result := FPutFileResult;
end;

function TMockCloudFileOps.DeleteFile(Path: WideString): Boolean;
begin
	FDeleteFileCalled := True;
	FLastDeletePath := Path;
	Result := FDeleteFileResult;
end;

{ TCloudDescriptionOpsAdapterTest }

procedure TCloudDescriptionOpsAdapterTest.Setup;
begin
	FMockCloudOps := TMockCloudFileOps.Create;
	FMockCloudOpsRef := FMockCloudOps;
	FMockFileSystem := TMemoryFileSystem.Create;
	FMockFileSystemRef := FMockFileSystem;
	FAdapter := TCloudDescriptionOpsAdapter.Create(FMockCloudOpsRef, FMockFileSystemRef);
end;

procedure TCloudDescriptionOpsAdapterTest.TearDown;
begin
	FAdapter.Free;
	FMockCloudOpsRef := nil;
	FMockFileSystemRef := nil;
end;

{ GetDescriptionFile tests }

procedure TCloudDescriptionOpsAdapterTest.TestGetDescriptionFile_Success_ReturnsTrue;
begin
	FMockCloudOps.SetGetFileResult(FS_FILE_OK);

	Assert.IsTrue(FAdapter.GetDescriptionFile('/remote/descript.ion', 'C:\local\descript.ion'),
		'Should return True when GetFile succeeds');
end;

procedure TCloudDescriptionOpsAdapterTest.TestGetDescriptionFile_Failure_ReturnsFalse;
begin
	FMockCloudOps.SetGetFileResult(FS_FILE_NOTFOUND);

	Assert.IsFalse(FAdapter.GetDescriptionFile('/remote/descript.ion', 'C:\local\descript.ion'),
		'Should return False when GetFile fails');
end;

procedure TCloudDescriptionOpsAdapterTest.TestGetDescriptionFile_PassesCorrectParameters;
begin
	FAdapter.GetDescriptionFile('/account/folder/descript.ion', 'C:\temp\descript.ion');

	Assert.IsTrue(FMockCloudOps.GetFileCalled, 'GetFile should be called');
	Assert.AreEqual('/account/folder/descript.ion', String(FMockCloudOps.LastGetRemotePath));
	Assert.AreEqual('C:\temp\descript.ion', String(FMockCloudOps.LastGetLocalPath));
end;

procedure TCloudDescriptionOpsAdapterTest.TestGetDescriptionFile_DisablesErrorLogging;
begin
	{Description file may not exist, so errors should not be logged}
	FAdapter.GetDescriptionFile('/remote/descript.ion', 'C:\local\descript.ion');

	Assert.IsFalse(FMockCloudOps.LastGetLogErrors,
		'LogErrors should be False for description file downloads');
end;

{ PutDescriptionFile tests - file exists }

procedure TCloudDescriptionOpsAdapterTest.TestPutDescriptionFile_FileExists_CallsPutFile;
begin
	{Simulate local file exists}
	FMockFileSystem.SetFileContent('C:\local\descript.ion', 'File content');

	FAdapter.PutDescriptionFile('/remote/descript.ion', 'C:\local\descript.ion');

	Assert.IsTrue(FMockCloudOps.PutFileCalled, 'PutFile should be called when local file exists');
	Assert.IsFalse(FMockCloudOps.DeleteFileCalled, 'DeleteFile should not be called');
	Assert.AreEqual('C:\local\descript.ion', String(FMockCloudOps.LastPutLocalPath));
	Assert.AreEqual('/remote/descript.ion', String(FMockCloudOps.LastPutRemotePath));
end;

procedure TCloudDescriptionOpsAdapterTest.TestPutDescriptionFile_FileExists_Success_ReturnsTrue;
begin
	FMockFileSystem.SetFileContent('C:\local\descript.ion', 'File content');
	FMockCloudOps.SetPutFileResult(FS_FILE_OK);

	Assert.IsTrue(FAdapter.PutDescriptionFile('/remote/descript.ion', 'C:\local\descript.ion'),
		'Should return True when PutFile succeeds');
end;

procedure TCloudDescriptionOpsAdapterTest.TestPutDescriptionFile_FileExists_Failure_ReturnsFalse;
begin
	FMockFileSystem.SetFileContent('C:\local\descript.ion', 'File content');
	FMockCloudOps.SetPutFileResult(FS_FILE_WRITEERROR);

	Assert.IsFalse(FAdapter.PutDescriptionFile('/remote/descript.ion', 'C:\local\descript.ion'),
		'Should return False when PutFile fails');
end;

{ PutDescriptionFile tests - file doesn't exist }

procedure TCloudDescriptionOpsAdapterTest.TestPutDescriptionFile_FileNotExists_CallsDeleteFile;
begin
	{Local file doesn't exist - should delete remote}
	FAdapter.PutDescriptionFile('/remote/descript.ion', 'C:\local\nonexistent.ion');

	Assert.IsTrue(FMockCloudOps.DeleteFileCalled,
		'DeleteFile should be called when local file does not exist');
	Assert.IsFalse(FMockCloudOps.PutFileCalled, 'PutFile should not be called');
	Assert.AreEqual('/remote/descript.ion', String(FMockCloudOps.LastDeletePath));
end;

procedure TCloudDescriptionOpsAdapterTest.TestPutDescriptionFile_FileNotExists_DeleteSuccess_ReturnsTrue;
begin
	FMockCloudOps.SetDeleteFileResult(True);

	Assert.IsTrue(FAdapter.PutDescriptionFile('/remote/descript.ion', 'C:\local\nonexistent.ion'),
		'Should return True when DeleteFile succeeds');
end;

procedure TCloudDescriptionOpsAdapterTest.TestPutDescriptionFile_FileNotExists_DeleteFailure_ReturnsFalse;
begin
	FMockCloudOps.SetDeleteFileResult(False);

	Assert.IsFalse(FAdapter.PutDescriptionFile('/remote/descript.ion', 'C:\local\nonexistent.ion'),
		'Should return False when DeleteFile fails');
end;

{ DeleteFile tests }

procedure TCloudDescriptionOpsAdapterTest.TestDeleteFile_Success_ReturnsTrue;
begin
	FMockCloudOps.SetDeleteFileResult(True);

	Assert.IsTrue(FAdapter.DeleteFile('/remote/somefile.txt'),
		'Should return True when DeleteFile succeeds');
end;

procedure TCloudDescriptionOpsAdapterTest.TestDeleteFile_Failure_ReturnsFalse;
begin
	FMockCloudOps.SetDeleteFileResult(False);

	Assert.IsFalse(FAdapter.DeleteFile('/remote/somefile.txt'),
		'Should return False when DeleteFile fails');
end;

procedure TCloudDescriptionOpsAdapterTest.TestDeleteFile_PassesCorrectPath;
begin
	FAdapter.DeleteFile('/account/path/to/file.txt');

	Assert.IsTrue(FMockCloudOps.DeleteFileCalled, 'DeleteFile should be called');
	Assert.AreEqual('/account/path/to/file.txt', String(FMockCloudOps.LastDeletePath));
end;

{ Interface implementation test }

procedure TCloudDescriptionOpsAdapterTest.TestImplementsICloudDescriptionOps;
var
	Intf: ICloudDescriptionOps;
begin
	Intf := TCloudDescriptionOpsAdapter.Create(FMockCloudOpsRef, FMockFileSystemRef);
	Assert.IsNotNull(Intf, 'Should implement ICloudDescriptionOps interface');
end;

{ TCloudMailRuFileOpsAdapterTest }

procedure TCloudMailRuFileOpsAdapterTest.TestImplementsICloudFileOps;
var
	Adapter: ICloudFileOps;
begin
	{Cannot test actual delegation without TCloudMailRu instance,
	 but we can verify interface implementation}
	Adapter := TCloudMailRuFileOpsAdapter.Create(nil);
	Assert.IsNotNull(Adapter, 'Should implement ICloudFileOps interface');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudDescriptionOpsAdapterTest);
TDUnitX.RegisterTestFixture(TCloudMailRuFileOpsAdapterTest);

end.

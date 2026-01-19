unit CommandDispatcherTest;

{Unit tests for TCommandDispatcher - plugin command routing.}

interface

uses
	System.SysUtils,
	DUnitX.TestFramework,
	CommandDispatcher,
	PLUGIN_TYPES;

type
	[TestFixture]
	TCommandResultTest = class
	public
		{TCommandResult.OK tests}
		[Test]
		procedure TestOK_ReturnsExecOK;
		[Test]
		procedure TestOK_HasEmptySymlinkPath;

		{TCommandResult.Error tests}
		[Test]
		procedure TestError_ReturnsExecError;
		[Test]
		procedure TestError_HasEmptySymlinkPath;

		{TCommandResult.Symlink tests}
		[Test]
		procedure TestSymlink_ReturnsExecSymlink;
		[Test]
		procedure TestSymlink_StoresPath;
		[Test]
		procedure TestSymlink_PreservesPathWithBackslash;
		[Test]
		procedure TestSymlink_PreservesPathWithPostfix;
	end;

implementation

{TCommandResult.OK tests}

procedure TCommandResultTest.TestOK_ReturnsExecOK;
var
	R: TCommandResult;
begin
	R := TCommandResult.OK;
	Assert.AreEqual(FS_EXEC_OK, R.ResultCode);
end;

procedure TCommandResultTest.TestOK_HasEmptySymlinkPath;
var
	R: TCommandResult;
begin
	R := TCommandResult.OK;
	Assert.AreEqual('', R.SymlinkPath);
end;

{TCommandResult.Error tests}

procedure TCommandResultTest.TestError_ReturnsExecError;
var
	R: TCommandResult;
begin
	R := TCommandResult.Error;
	Assert.AreEqual(FS_EXEC_ERROR, R.ResultCode);
end;

procedure TCommandResultTest.TestError_HasEmptySymlinkPath;
var
	R: TCommandResult;
begin
	R := TCommandResult.Error;
	Assert.AreEqual('', R.SymlinkPath);
end;

{TCommandResult.Symlink tests}

procedure TCommandResultTest.TestSymlink_ReturnsExecSymlink;
var
	R: TCommandResult;
begin
	R := TCommandResult.Symlink('\test');
	Assert.AreEqual(FS_EXEC_SYMLINK, R.ResultCode);
end;

procedure TCommandResultTest.TestSymlink_StoresPath;
var
	R: TCommandResult;
begin
	R := TCommandResult.Symlink('\account\path');
	Assert.AreEqual('\account\path', R.SymlinkPath);
end;

procedure TCommandResultTest.TestSymlink_PreservesPathWithBackslash;
var
	R: TCommandResult;
begin
	R := TCommandResult.Symlink('\myaccount');
	Assert.AreEqual('\myaccount', R.SymlinkPath);
end;

procedure TCommandResultTest.TestSymlink_PreservesPathWithPostfix;
var
	R: TCommandResult;
begin
	R := TCommandResult.Symlink('\account.trash');
	Assert.AreEqual('\account.trash', R.SymlinkPath);
end;

initialization
	TDUnitX.RegisterTestFixture(TCommandResultTest);

end.

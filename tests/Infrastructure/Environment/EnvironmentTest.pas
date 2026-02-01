unit EnvironmentTest;

interface

uses
	Environment,
	DUnitX.TestFramework;

type

	[TestFixture]
	TMemoryEnvironmentTest = class
	private
		FEnv: TMemoryEnvironment;
	public
		[Setup]
		procedure Setup;

		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestGetEnvironmentVariable_ReturnsEmpty_WhenNotSet;

		[Test]
		procedure TestGetEnvironmentVariable_ReturnsValue_WhenSet;

		[Test]
		procedure TestGetEnvironmentVariable_UpdatesValue_WhenSetTwice;

		[Test]
		procedure TestGetModulePath_ReturnsEmpty_WhenNotSet;

		[Test]
		procedure TestGetModulePath_ReturnsValue_WhenSet;

		[Test]
		procedure TestFileExists_ReturnsFalse_WhenNotAdded;

		[Test]
		procedure TestFileExists_ReturnsTrue_WhenAdded;

		[Test]
		procedure TestDirectoryExists_ReturnsFalse_WhenNotAdded;

		[Test]
		procedure TestDirectoryExists_ReturnsTrue_WhenAdded;

		[Test]
		procedure TestIsDirectoryWriteable_ReturnsFalse_WhenNotAdded;

		[Test]
		procedure TestIsDirectoryWriteable_ReturnsTrue_WhenAdded;

		[Test]
		procedure TestCreateDirectory_AddsToExistingDirs;

		[Test]
		procedure TestClear_RemovesAllState;

		[Test]
		procedure TestAddExistingFile_IgnoresDuplicates;

		[Test]
		procedure TestAddExistingDirectory_IgnoresDuplicates;

		[Test]
		procedure TestAddWriteableDirectory_IgnoresDuplicates;
	end;

	[TestFixture]
	TNullEnvironmentTest = class
	private
		FEnv: TNullEnvironment;
	public
		[Setup]
		procedure Setup;

		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestGetEnvironmentVariable_ReturnsEmpty;

		[Test]
		procedure TestGetModulePath_ReturnsEmpty;

		[Test]
		procedure TestFileExists_ReturnsFalse;

		[Test]
		procedure TestDirectoryExists_ReturnsFalse;

		[Test]
		procedure TestIsDirectoryWriteable_ReturnsFalse;

		[Test]
		procedure TestCreateDirectory_DoesNothing;
	end;

implementation

{TMemoryEnvironmentTest}

procedure TMemoryEnvironmentTest.Setup;
begin
	FEnv := TMemoryEnvironment.Create;
end;

procedure TMemoryEnvironmentTest.TearDown;
begin
	FEnv.Free;
end;

procedure TMemoryEnvironmentTest.TestGetEnvironmentVariable_ReturnsEmpty_WhenNotSet;
begin
	Assert.AreEqual('', FEnv.GetEnvironmentVariable('NONEXISTENT'));
end;

procedure TMemoryEnvironmentTest.TestGetEnvironmentVariable_ReturnsValue_WhenSet;
begin
	FEnv.SetEnvironmentVariable('APPDATA', 'C:\Users\Test\AppData\Roaming');
	Assert.AreEqual('C:\Users\Test\AppData\Roaming', FEnv.GetEnvironmentVariable('APPDATA'));
end;

procedure TMemoryEnvironmentTest.TestGetEnvironmentVariable_UpdatesValue_WhenSetTwice;
begin
	FEnv.SetEnvironmentVariable('PATH', 'C:\First');
	FEnv.SetEnvironmentVariable('PATH', 'C:\Second');
	Assert.AreEqual('C:\Second', FEnv.GetEnvironmentVariable('PATH'));
end;

procedure TMemoryEnvironmentTest.TestGetModulePath_ReturnsEmpty_WhenNotSet;
begin
	Assert.AreEqual('', FEnv.GetModulePath);
end;

procedure TMemoryEnvironmentTest.TestGetModulePath_ReturnsValue_WhenSet;
begin
	FEnv.SetModulePath('C:\Program Files\MyApp\');
	Assert.AreEqual('C:\Program Files\MyApp\', FEnv.GetModulePath);
end;

procedure TMemoryEnvironmentTest.TestFileExists_ReturnsFalse_WhenNotAdded;
begin
	Assert.IsFalse(FEnv.FileExists('C:\nonexistent.txt'));
end;

procedure TMemoryEnvironmentTest.TestFileExists_ReturnsTrue_WhenAdded;
begin
	FEnv.AddExistingFile('C:\test.ini');
	Assert.IsTrue(FEnv.FileExists('C:\test.ini'));
end;

procedure TMemoryEnvironmentTest.TestDirectoryExists_ReturnsFalse_WhenNotAdded;
begin
	Assert.IsFalse(FEnv.DirectoryExists('C:\NonexistentDir'));
end;

procedure TMemoryEnvironmentTest.TestDirectoryExists_ReturnsTrue_WhenAdded;
begin
	FEnv.AddExistingDirectory('C:\ExistingDir\');
	Assert.IsTrue(FEnv.DirectoryExists('C:\ExistingDir\'));
end;

procedure TMemoryEnvironmentTest.TestIsDirectoryWriteable_ReturnsFalse_WhenNotAdded;
begin
	Assert.IsFalse(FEnv.IsDirectoryWriteable('C:\ReadOnlyDir\'));
end;

procedure TMemoryEnvironmentTest.TestIsDirectoryWriteable_ReturnsTrue_WhenAdded;
begin
	FEnv.AddWriteableDirectory('C:\WriteableDir\');
	Assert.IsTrue(FEnv.IsDirectoryWriteable('C:\WriteableDir\'));
end;

procedure TMemoryEnvironmentTest.TestCreateDirectory_AddsToExistingDirs;
begin
	Assert.IsFalse(FEnv.DirectoryExists('C:\NewDir\'));
	FEnv.CreateDirectory('C:\NewDir\');
	Assert.IsTrue(FEnv.DirectoryExists('C:\NewDir\'));
end;

procedure TMemoryEnvironmentTest.TestClear_RemovesAllState;
begin
	FEnv.SetEnvironmentVariable('TEST', 'value');
	FEnv.SetModulePath('C:\Path\');
	FEnv.AddExistingFile('C:\file.txt');
	FEnv.AddExistingDirectory('C:\dir\');
	FEnv.AddWriteableDirectory('C:\write\');

	FEnv.Clear;

	Assert.AreEqual('', FEnv.GetEnvironmentVariable('TEST'));
	Assert.AreEqual('', FEnv.GetModulePath);
	Assert.IsFalse(FEnv.FileExists('C:\file.txt'));
	Assert.IsFalse(FEnv.DirectoryExists('C:\dir\'));
	Assert.IsFalse(FEnv.IsDirectoryWriteable('C:\write\'));
end;

procedure TMemoryEnvironmentTest.TestAddExistingFile_IgnoresDuplicates;
begin
	FEnv.AddExistingFile('C:\test.txt');
	FEnv.AddExistingFile('C:\test.txt');
	Assert.IsTrue(FEnv.FileExists('C:\test.txt'));
end;

procedure TMemoryEnvironmentTest.TestAddExistingDirectory_IgnoresDuplicates;
begin
	FEnv.AddExistingDirectory('C:\test\');
	FEnv.AddExistingDirectory('C:\test\');
	Assert.IsTrue(FEnv.DirectoryExists('C:\test\'));
end;

procedure TMemoryEnvironmentTest.TestAddWriteableDirectory_IgnoresDuplicates;
begin
	FEnv.AddWriteableDirectory('C:\test\');
	FEnv.AddWriteableDirectory('C:\test\');
	Assert.IsTrue(FEnv.IsDirectoryWriteable('C:\test\'));
end;

{TNullEnvironmentTest}

procedure TNullEnvironmentTest.Setup;
begin
	FEnv := TNullEnvironment.Create;
end;

procedure TNullEnvironmentTest.TearDown;
begin
	FEnv.Free;
end;

procedure TNullEnvironmentTest.TestGetEnvironmentVariable_ReturnsEmpty;
begin
	Assert.AreEqual('', FEnv.GetEnvironmentVariable('APPDATA'));
	Assert.AreEqual('', FEnv.GetEnvironmentVariable('PATH'));
end;

procedure TNullEnvironmentTest.TestGetModulePath_ReturnsEmpty;
begin
	Assert.AreEqual('', FEnv.GetModulePath);
end;

procedure TNullEnvironmentTest.TestFileExists_ReturnsFalse;
begin
	Assert.IsFalse(FEnv.FileExists('C:\any\path\file.txt'));
end;

procedure TNullEnvironmentTest.TestDirectoryExists_ReturnsFalse;
begin
	Assert.IsFalse(FEnv.DirectoryExists('C:\any\directory\'));
end;

procedure TNullEnvironmentTest.TestIsDirectoryWriteable_ReturnsFalse;
begin
	Assert.IsFalse(FEnv.IsDirectoryWriteable('C:\any\directory\'));
end;

procedure TNullEnvironmentTest.TestCreateDirectory_DoesNothing;
begin
	FEnv.CreateDirectory('C:\test\');
	{TNullEnvironment does nothing - just verify no exception}
	Assert.IsFalse(FEnv.DirectoryExists('C:\test\'));
end;

initialization

TDUnitX.RegisterTestFixture(TMemoryEnvironmentTest);
TDUnitX.RegisterTestFixture(TNullEnvironmentTest);

end.

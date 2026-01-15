unit FileHelperTest;

interface

uses
	FileHelper,
	Classes,
	SysUtils,
	Windows,
	DUnitX.TestFramework;

type

	[TestFixture]
	TFileHelperTest = class
	private
		FTempDir: string;
		FTempFile: string;
		procedure CreateTempFile(Size: Integer);
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{ SizeOfFile tests }
		[Test]
		procedure TestSizeOfFileExisting;
		[Test]
		procedure TestSizeOfFileNonExistent;
		[Test]
		procedure TestSizeOfFileEmpty;
		[Test]
		procedure TestSizeOfFileLarge;

		{ IsWriteable tests }
		[Test]
		procedure TestIsWriteableValidDir;
		[Test]
		procedure TestIsWriteableInvalidDir;
		[Test]
		procedure TestIsWriteableWithCustomFileName;

		{ SetAllFileTime tests }
		[Test]
		procedure TestSetAllFileTimeModifiesTime;
	end;

implementation

{ Setup and TearDown }

procedure TFileHelperTest.Setup;
begin
	FTempDir := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) + 'FileHelperTest_' + IntToStr(GetCurrentThreadId);
	ForceDirectories(FTempDir);
	FTempFile := IncludeTrailingPathDelimiter(FTempDir) + 'testfile.tmp';
end;

procedure TFileHelperTest.TearDown;
begin
	if FileExists(FTempFile) then
		SysUtils.DeleteFile(FTempFile);
	if DirectoryExists(FTempDir) then
		RemoveDir(FTempDir);
end;

procedure TFileHelperTest.CreateTempFile(Size: Integer);
var
	F: TFileStream;
	Buffer: TBytes;
begin
	F := TFileStream.Create(FTempFile, fmCreate);
	try
		if Size > 0 then
		begin
			SetLength(Buffer, Size);
			FillChar(Buffer[0], Size, $AA);
			F.WriteBuffer(Buffer[0], Size);
		end;
	finally
		F.Free;
	end;
end;

{ SizeOfFile tests }

procedure TFileHelperTest.TestSizeOfFileExisting;
begin
	CreateTempFile(1024);
	Assert.AreEqual(Int64(1024), SizeOfFile(FTempFile));
end;

procedure TFileHelperTest.TestSizeOfFileNonExistent;
begin
	{ Non-existent file should return -1 }
	Assert.AreEqual(Int64(-1), SizeOfFile(FTempFile + '.nonexistent'));
end;

procedure TFileHelperTest.TestSizeOfFileEmpty;
begin
	CreateTempFile(0);
	Assert.AreEqual(Int64(0), SizeOfFile(FTempFile));
end;

procedure TFileHelperTest.TestSizeOfFileLarge;
begin
	{ Test with file larger than 32-bit size (use 100KB for speed) }
	CreateTempFile(102400);
	Assert.AreEqual(Int64(102400), SizeOfFile(FTempFile));
end;

{ IsWriteable tests }

procedure TFileHelperTest.TestIsWriteableValidDir;
begin
	{ Temp directory should be writable }
	Assert.IsTrue(IsWriteable(FTempDir));
end;

procedure TFileHelperTest.TestIsWriteableInvalidDir;
begin
	{ Non-existent directory should not be writable }
	Assert.IsFalse(IsWriteable('Z:\NonExistentDir\SubDir\'));
end;

procedure TFileHelperTest.TestIsWriteableWithCustomFileName;
begin
	{ Test with custom filename parameter }
	Assert.IsTrue(IsWriteable(FTempDir, 'custom_test.tmp'));
end;

{ SetAllFileTime tests }

procedure TFileHelperTest.TestSetAllFileTimeModifiesTime;
var
	FileTime: TFileTime;
	SystemTime: TSystemTime;
	Handle: THandle;
	CreationTime, LastAccessTime, LastWriteTime: TFileTime;
begin
	CreateTempFile(100);

	{ Set a specific date: 2020-01-15 12:30:00 }
	FillChar(SystemTime, SizeOf(SystemTime), 0);
	SystemTime.wYear := 2020;
	SystemTime.wMonth := 1;
	SystemTime.wDay := 15;
	SystemTime.wHour := 12;
	SystemTime.wMinute := 30;
	SystemTime.wSecond := 0;
	SystemTimeToFileTime(SystemTime, FileTime);

	SetAllFileTime(FTempFile, FileTime);

	{ Verify the time was set }
	Handle := CreateFile(PChar(FTempFile), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
	try
		Assert.IsTrue(Handle <> INVALID_HANDLE_VALUE);
		GetFileTime(Handle, @CreationTime, @LastAccessTime, @LastWriteTime);
		{ All three times should be set to the same value }
		Assert.AreEqual(FileTime.dwLowDateTime, LastWriteTime.dwLowDateTime);
		Assert.AreEqual(FileTime.dwHighDateTime, LastWriteTime.dwHighDateTime);
	finally
		CloseHandle(Handle);
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TFileHelperTest);

end.

unit CloudMailRuUploadTest;

{Tests for upload-related mock infrastructure.
 Tests PutFile responses, upload capture, and hash handling in mock HTTP.}

interface

uses
	MockCloudHTTP,
	MockHTTPManager,
	MockShardHelper,
	PLUGIN_TYPES,
	System.Classes,
	System.SysUtils,
	DUnitX.TestFramework;

type
	[TestFixture]
	TCloudMailRuUploadTest = class
	private
		FMockHTTP: TMockCloudHTTP;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{PutFile response tests}
		[Test]
		procedure TestPutFile_ReturnsConfiguredHash;
		[Test]
		procedure TestPutFile_ReturnsResultCode;
		[Test]
		procedure TestPutFile_QueuesHashesForMultipleUploads;

		{Upload capture tests}
		[Test]
		procedure TestPutFile_CapturesUploadedContent;
		[Test]
		procedure TestPutFile_CapturesFileName;
		[Test]
		procedure TestPutFile_CapturesMultipleUploads;
		[Test]
		procedure TestPutFile_GetCapturedUploadContent_FindsByPattern;

		{PostFile capture tests}
		[Test]
		procedure TestPostFile_CapturesUpload;
		[Test]
		procedure TestPostFile_ReturnsConfiguredHash;

		{Error simulation tests}
		[Test]
		procedure TestPutFile_SimulateError_ReturnsErrorCode;
		[Test]
		procedure TestPutFile_SimulateWriteError_ReturnsWriteError;

		{Binary content capture tests}
		[Test]
		procedure TestPutFile_BinaryContent_CapturedExactly;
		[Test]
		procedure TestPutFile_LargeContent_CapturedCorrectly;

		{Shard URL simulation tests}
		[Test]
		procedure TestPutFile_ToShardURL_Success;
		[Test]
		procedure TestPutFile_OAuth_DispatcherFormat;

		{Hash length validation tests}
		[Test]
		procedure TestPutFile_SHA1Hash_CorrectLength;
		[Test]
		procedure TestPutFile_QueuedHashes_ReturnSequentially;
	end;

implementation

const
	SHA1_HASH_40 = 'ABCD1234567890ABCD1234567890ABCD12345678';
	SHA1_HASH_2 = 'EFGH5678901234EFGH5678901234EFGH56789012';

{TCloudMailRuUploadTest}

procedure TCloudMailRuUploadTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
end;

procedure TCloudMailRuUploadTest.TearDown;
begin
	FMockHTTP := nil;
end;

{PutFile response tests}

procedure TCloudMailRuUploadTest.TestPutFile_ReturnsConfiguredHash;
var
	Stream: TMemoryStream;
	Answer: WideString;
begin
	FMockHTTP.SetPutFileResponse('https://upload.test.com/', SHA1_HASH_40);

	Stream := TMemoryStream.Create;
	try
		FMockHTTP.PutFile('https://upload.test.com/', 'test.txt', Stream, Answer);
		Assert.AreEqual(String(SHA1_HASH_40), String(Answer), 'Should return configured hash');
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuUploadTest.TestPutFile_ReturnsResultCode;
var
	Stream: TMemoryStream;
	Answer: WideString;
	ResultCode: Integer;
begin
	FMockHTTP.SetPutFileResponse('https://upload.test.com/', SHA1_HASH_40, FS_FILE_OK);

	Stream := TMemoryStream.Create;
	try
		ResultCode := FMockHTTP.PutFile('https://upload.test.com/', 'test.txt', Stream, Answer);
		Assert.AreEqual(FS_FILE_OK, ResultCode, 'Should return OK result code');
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuUploadTest.TestPutFile_QueuesHashesForMultipleUploads;
var
	Stream: TMemoryStream;
	Answer: WideString;
begin
	FMockHTTP.QueuePutFileResponse('https://upload.test.com/', SHA1_HASH_40);
	FMockHTTP.QueuePutFileResponse('https://upload.test.com/', SHA1_HASH_2);

	Stream := TMemoryStream.Create;
	try
		{First upload gets first hash}
		FMockHTTP.PutFile('https://upload.test.com/', 'file1.txt', Stream, Answer);
		Assert.AreEqual(String(SHA1_HASH_40), String(Answer), 'First upload should get first hash');

		{Second upload gets second hash}
		FMockHTTP.PutFile('https://upload.test.com/', 'file2.txt', Stream, Answer);
		Assert.AreEqual(String(SHA1_HASH_2), String(Answer), 'Second upload should get second hash');
	finally
		Stream.Free;
	end;
end;

{Upload capture tests}

procedure TCloudMailRuUploadTest.TestPutFile_CapturesUploadedContent;
var
	Stream: TMemoryStream;
	Content: TBytes;
	Answer: WideString;
	Captured: TBytes;
begin
	Content := TEncoding.UTF8.GetBytes('Upload content here');
	FMockHTTP.SetPutFileResponse('https://upload.test.com/', SHA1_HASH_40);

	Stream := TMemoryStream.Create;
	try
		Stream.Write(Content[0], Length(Content));
		Stream.Position := 0;

		FMockHTTP.PutFile('https://upload.test.com/', 'test.txt', Stream, Answer);

		Captured := FMockHTTP.GetLastUploadCapture.Content;
		Assert.AreEqual(Length(Content), Length(Captured), 'Captured content length should match');
		Assert.IsTrue(CompareMem(@Content[0], @Captured[0], Length(Content)), 'Captured content should match');
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuUploadTest.TestPutFile_CapturesFileName;
var
	Stream: TMemoryStream;
	Answer: WideString;
begin
	FMockHTTP.SetPutFileResponse('https://upload.test.com/', SHA1_HASH_40);

	Stream := TMemoryStream.Create;
	try
		FMockHTTP.PutFile('https://upload.test.com/', 'myfile.dat', Stream, Answer);

		Assert.AreEqual(String('myfile.dat'), String(FMockHTTP.GetLastUploadCapture.FileName), 'Should capture filename');
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuUploadTest.TestPutFile_CapturesMultipleUploads;
var
	Stream: TMemoryStream;
	Answer: WideString;
begin
	FMockHTTP.SetPutFileResponse('https://upload.test.com/', SHA1_HASH_40);

	Stream := TMemoryStream.Create;
	try
		FMockHTTP.PutFile('https://upload.test.com/', 'file1.txt', Stream, Answer);
		FMockHTTP.PutFile('https://upload.test.com/', 'file2.txt', Stream, Answer);
		FMockHTTP.PutFile('https://upload.test.com/', 'file3.txt', Stream, Answer);

		Assert.AreEqual(3, FMockHTTP.GetUploadCount, 'Should capture 3 uploads');
		Assert.AreEqual(String('file1.txt'), String(FMockHTTP.GetUploadCapture(0).FileName), 'First upload');
		Assert.AreEqual(String('file2.txt'), String(FMockHTTP.GetUploadCapture(1).FileName), 'Second upload');
		Assert.AreEqual(String('file3.txt'), String(FMockHTTP.GetUploadCapture(2).FileName), 'Third upload');
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuUploadTest.TestPutFile_GetCapturedUploadContent_FindsByPattern;
var
	Stream1, Stream2: TMemoryStream;
	Content1, Content2: TBytes;
	Answer: WideString;
	Captured: TBytes;
begin
	Content1 := TEncoding.UTF8.GetBytes('Content for chunk1');
	Content2 := TEncoding.UTF8.GetBytes('Content for chunk2');
	FMockHTTP.SetPutFileResponse('https://upload.test.com/', SHA1_HASH_40);

	Stream1 := TMemoryStream.Create;
	Stream2 := TMemoryStream.Create;
	try
		Stream1.Write(Content1[0], Length(Content1));
		Stream1.Position := 0;
		Stream2.Write(Content2[0], Length(Content2));
		Stream2.Position := 0;

		FMockHTTP.PutFile('https://upload.test.com/chunk1', 'chunk1.dat', Stream1, Answer);
		FMockHTTP.PutFile('https://upload.test.com/chunk2', 'chunk2.dat', Stream2, Answer);

		Captured := FMockHTTP.GetCapturedUploadContent('chunk2');
		Assert.AreEqual(Length(Content2), Length(Captured), 'Should find chunk2 content by pattern');
	finally
		Stream1.Free;
		Stream2.Free;
	end;
end;

{PostFile capture tests}

procedure TCloudMailRuUploadTest.TestPostFile_CapturesUpload;
var
	Stream: TMemoryStream;
	Content: TBytes;
	Answer: WideString;
begin
	Content := TEncoding.UTF8.GetBytes('PostFile content');
	FMockHTTP.SetPutFileResponse('https://post.test.com/', SHA1_HASH_40);

	Stream := TMemoryStream.Create;
	try
		Stream.Write(Content[0], Length(Content));
		Stream.Position := 0;

		FMockHTTP.PostFile('https://post.test.com/', 'posted.txt', Stream, Answer);

		Assert.AreEqual(1, FMockHTTP.GetUploadCount, 'Should capture PostFile upload');
		Assert.AreEqual(String('posted.txt'), String(FMockHTTP.GetLastUploadCapture.FileName), 'Should capture filename');
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuUploadTest.TestPostFile_ReturnsConfiguredHash;
var
	Stream: TMemoryStream;
	Answer: WideString;
begin
	FMockHTTP.SetPutFileResponse('https://post.test.com/', SHA1_HASH_40);

	Stream := TMemoryStream.Create;
	try
		FMockHTTP.PostFile('https://post.test.com/', 'test.txt', Stream, Answer);
		Assert.AreEqual(String(SHA1_HASH_40), String(Answer), 'PostFile should return configured hash');
	finally
		Stream.Free;
	end;
end;

{Error simulation tests}

procedure TCloudMailRuUploadTest.TestPutFile_SimulateError_ReturnsErrorCode;
var
	Stream: TMemoryStream;
	Answer: WideString;
	ResultCode: Integer;
begin
	FMockHTTP.SetPutFileResponse('https://upload.test.com/', '', FS_FILE_READERROR);

	Stream := TMemoryStream.Create;
	try
		ResultCode := FMockHTTP.PutFile('https://upload.test.com/', 'test.txt', Stream, Answer);
		Assert.AreEqual(FS_FILE_READERROR, ResultCode, 'Should return configured error code');
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuUploadTest.TestPutFile_SimulateWriteError_ReturnsWriteError;
var
	Stream: TMemoryStream;
	Answer: WideString;
	ResultCode: Integer;
begin
	FMockHTTP.SetPutFileResponse('https://upload.test.com/', '', FS_FILE_WRITEERROR);

	Stream := TMemoryStream.Create;
	try
		ResultCode := FMockHTTP.PutFile('https://upload.test.com/', 'test.txt', Stream, Answer);
		Assert.AreEqual(FS_FILE_WRITEERROR, ResultCode, 'Should return write error');
	finally
		Stream.Free;
	end;
end;

{Binary content capture tests}

procedure TCloudMailRuUploadTest.TestPutFile_BinaryContent_CapturedExactly;
var
	Stream: TMemoryStream;
	Content: TBytes;
	Answer: WideString;
	Captured: TBytes;
	i: Integer;
begin
	{Create binary content with byte values 0-255}
	SetLength(Content, 256);
	for i := 0 to 255 do
		Content[i] := Byte(i);

	FMockHTTP.SetPutFileResponse('https://upload.test.com/', SHA1_HASH_40);

	Stream := TMemoryStream.Create;
	try
		Stream.Write(Content[0], Length(Content));
		Stream.Position := 0;

		FMockHTTP.PutFile('https://upload.test.com/', 'binary.bin', Stream, Answer);

		Captured := FMockHTTP.GetLastUploadCapture.Content;
		Assert.AreEqual(Integer(256), Integer(Length(Captured)), 'Should capture 256 bytes');
		Assert.IsTrue(CompareMem(@Content[0], @Captured[0], 256), 'Binary content should be captured exactly');
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuUploadTest.TestPutFile_LargeContent_CapturedCorrectly;
var
	Stream: TMemoryStream;
	Content: TBytes;
	Answer: WideString;
	i: Integer;
const
	LARGE_SIZE = 1024 * 100; {100KB}
begin
	SetLength(Content, LARGE_SIZE);
	for i := 0 to LARGE_SIZE - 1 do
		Content[i] := Byte(i mod 256);

	FMockHTTP.SetPutFileResponse('https://upload.test.com/', SHA1_HASH_40);

	Stream := TMemoryStream.Create;
	try
		Stream.Write(Content[0], Length(Content));
		Stream.Position := 0;

		FMockHTTP.PutFile('https://upload.test.com/', 'large.bin', Stream, Answer);

		var Captured := FMockHTTP.GetLastUploadCapture.Content;
		Assert.AreEqual(Integer(LARGE_SIZE), Integer(Length(Captured)), 'Should capture large content');
	finally
		Stream.Free;
	end;
end;

{Shard URL simulation tests}

procedure TCloudMailRuUploadTest.TestPutFile_ToShardURL_Success;
var
	Stream: TMemoryStream;
	Answer: WideString;
	ResultCode: Integer;
begin
	{Simulate upload to shard URL (typical pattern)}
	FMockHTTP.SetPutFileResponse('https://c123.cloud.mail.ru/', SHA1_HASH_40);

	Stream := TMemoryStream.Create;
	try
		ResultCode := FMockHTTP.PutFile('https://c123.cloud.mail.ru/', 'file.txt', Stream, Answer);

		Assert.AreEqual(FS_FILE_OK, ResultCode, 'Upload to shard should succeed');
		Assert.AreEqual(String(SHA1_HASH_40), String(Answer), 'Should return hash');
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuUploadTest.TestPutFile_OAuth_DispatcherFormat;
var
	Response: WideString;
begin
	{OAuth upload dispatcher returns plain text: "URL IP COUNT"}
	Response := TMockShardHelper.CreateOAuthUploadDispatcherResponse('https://oauth.upload.test/');

	Assert.AreEqual(String('https://oauth.upload.test/ 127.0.0.1 1'), String(Response),
		'OAuth upload dispatcher format should be plain text');
end;

{Hash length validation tests}

procedure TCloudMailRuUploadTest.TestPutFile_SHA1Hash_CorrectLength;
var
	Stream: TMemoryStream;
	Answer: WideString;
begin
	FMockHTTP.SetPutFileResponse('https://upload.test.com/', SHA1_HASH_40);

	Stream := TMemoryStream.Create;
	try
		FMockHTTP.PutFile('https://upload.test.com/', 'test.txt', Stream, Answer);
		Assert.AreEqual(40, Length(Answer), 'SHA1 hash should be exactly 40 characters');
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuUploadTest.TestPutFile_QueuedHashes_ReturnSequentially;
var
	Stream: TMemoryStream;
	Answer: WideString;
begin
	FMockHTTP.QueuePutFileResponse('https://upload.test.com/', 'HASH1111111111111111111111111111111111');
	FMockHTTP.QueuePutFileResponse('https://upload.test.com/', 'HASH2222222222222222222222222222222222');
	FMockHTTP.QueuePutFileResponse('https://upload.test.com/', 'HASH3333333333333333333333333333333333');

	Stream := TMemoryStream.Create;
	try
		FMockHTTP.PutFile('https://upload.test.com/', 'file1.txt', Stream, Answer);
		Assert.IsTrue(Pos('HASH1', String(Answer)) > 0, 'First call should get HASH1');

		FMockHTTP.PutFile('https://upload.test.com/', 'file2.txt', Stream, Answer);
		Assert.IsTrue(Pos('HASH2', String(Answer)) > 0, 'Second call should get HASH2');

		FMockHTTP.PutFile('https://upload.test.com/', 'file3.txt', Stream, Answer);
		Assert.IsTrue(Pos('HASH3', String(Answer)) > 0, 'Third call should get HASH3');
	finally
		Stream.Free;
	end;
end;

initialization
	TDUnitX.RegisterTestFixture(TCloudMailRuUploadTest);

end.

unit CloudMailRuDownloadTest;

{Tests for download-related mock infrastructure.
 Tests stream responses, hash verification, and error handling in mock HTTP.}

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
	TCloudMailRuDownloadTest = class
	private
		FMockHTTP: TMockCloudHTTP;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Stream download tests via mock}
		[Test]
		procedure TestGetFile_StreamResponse_WritesContent;
		[Test]
		procedure TestGetFile_StreamResponse_ReturnsResultCode;
		[Test]
		procedure TestGetFile_NoStreamResponse_UsesFileResponse;
		[Test]
		procedure TestGetFile_QueuedResponses_ReturnSequentially;

		{Error simulation tests}
		[Test]
		procedure TestGetFile_SimulateError_ReturnsErrorCode;
		[Test]
		procedure TestGetFile_SimulateNotFound_ReturnsNotFound;

		{Binary content tests}
		[Test]
		procedure TestGetFile_BinaryContent_PreservedExactly;
		[Test]
		procedure TestGetFile_LargeContent_HandledCorrectly;

		{Shard URL simulation tests}
		[Test]
		procedure TestGetFile_FromShardURL_Success;
		[Test]
		procedure TestGetFile_OAuth_DispatcherFormat;
	end;

implementation

{TCloudMailRuDownloadTest}

procedure TCloudMailRuDownloadTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
end;

procedure TCloudMailRuDownloadTest.TearDown;
begin
	FMockHTTP := nil;
end;

{Stream download tests}

procedure TCloudMailRuDownloadTest.TestGetFile_StreamResponse_WritesContent;
var
	Content: TBytes;
	Stream: TMemoryStream;
	ReadBack: TBytes;
begin
	Content := TEncoding.UTF8.GetBytes('Downloaded file content');
	FMockHTTP.SetStreamResponse('https://download.test.com/', Content);

	Stream := TMemoryStream.Create;
	try
		FMockHTTP.GetFile('https://download.test.com/file.txt', Stream);

		SetLength(ReadBack, Stream.Size);
		Stream.Position := 0;
		Stream.Read(ReadBack[0], Stream.Size);

		Assert.AreEqual(Length(Content), Length(ReadBack), 'Downloaded content length should match');
		Assert.IsTrue(CompareMem(@Content[0], @ReadBack[0], Length(Content)), 'Content should match');
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuDownloadTest.TestGetFile_StreamResponse_ReturnsResultCode;
var
	Stream: TMemoryStream;
	ResultCode: Integer;
begin
	FMockHTTP.SetStreamResponse('https://download.test.com/', nil, FS_FILE_OK);

	Stream := TMemoryStream.Create;
	try
		ResultCode := FMockHTTP.GetFile('https://download.test.com/file.txt', Stream);
		Assert.AreEqual(FS_FILE_OK, ResultCode, 'Should return OK result code');
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuDownloadTest.TestGetFile_NoStreamResponse_UsesFileResponse;
var
	Content: TBytes;
	Stream: TMemoryStream;
	ReadBack: TBytes;
begin
	{Use SetFileResponse instead of SetStreamResponse}
	Content := TEncoding.UTF8.GetBytes('File response content');
	FMockHTTP.SetFileResponse('https://download.test.com/', Content);

	Stream := TMemoryStream.Create;
	try
		FMockHTTP.GetFile('https://download.test.com/file.txt', Stream);

		SetLength(ReadBack, Stream.Size);
		Stream.Position := 0;
		Stream.Read(ReadBack[0], Stream.Size);

		Assert.AreEqual(Length(Content), Length(ReadBack), 'Should use FileResponse fallback');
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuDownloadTest.TestGetFile_QueuedResponses_ReturnSequentially;
var
	Content1, Content2: TBytes;
	Stream: TMemoryStream;
	ReadBack: TBytes;
begin
	Content1 := TEncoding.UTF8.GetBytes('First chunk');
	Content2 := TEncoding.UTF8.GetBytes('Second chunk');

	FMockHTTP.QueueStreamResponse('https://download.test.com/', Content1);
	FMockHTTP.QueueStreamResponse('https://download.test.com/', Content2);

	Stream := TMemoryStream.Create;
	try
		{First request gets first response}
		FMockHTTP.GetFile('https://download.test.com/file.txt', Stream);
		SetLength(ReadBack, Stream.Size);
		Stream.Position := 0;
		Stream.Read(ReadBack[0], Stream.Size);
		Assert.AreEqual(Length(Content1), Length(ReadBack), 'First call should get first content');

		{Second request gets second response}
		Stream.Clear;
		FMockHTTP.GetFile('https://download.test.com/file.txt', Stream);
		SetLength(ReadBack, Stream.Size);
		Stream.Position := 0;
		Stream.Read(ReadBack[0], Stream.Size);
		Assert.AreEqual(Length(Content2), Length(ReadBack), 'Second call should get second content');
	finally
		Stream.Free;
	end;
end;

{Error simulation tests}

procedure TCloudMailRuDownloadTest.TestGetFile_SimulateError_ReturnsErrorCode;
var
	Stream: TMemoryStream;
	ResultCode: Integer;
begin
	FMockHTTP.SetStreamResponse('https://download.test.com/', nil, FS_FILE_READERROR);

	Stream := TMemoryStream.Create;
	try
		ResultCode := FMockHTTP.GetFile('https://download.test.com/file.txt', Stream);
		Assert.AreEqual(FS_FILE_READERROR, ResultCode, 'Should return read error');
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuDownloadTest.TestGetFile_SimulateNotFound_ReturnsNotFound;
var
	Stream: TMemoryStream;
	ResultCode: Integer;
begin
	FMockHTTP.SetStreamResponse('https://download.test.com/', nil, FS_FILE_NOTFOUND);

	Stream := TMemoryStream.Create;
	try
		ResultCode := FMockHTTP.GetFile('https://download.test.com/missing.txt', Stream);
		Assert.AreEqual(FS_FILE_NOTFOUND, ResultCode, 'Should return not found');
	finally
		Stream.Free;
	end;
end;

{Binary content tests}

procedure TCloudMailRuDownloadTest.TestGetFile_BinaryContent_PreservedExactly;
var
	Content: TBytes;
	Stream: TMemoryStream;
	ReadBack: TBytes;
	i: Integer;
begin
	{Create binary content with byte values 0-255}
	SetLength(Content, 256);
	for i := 0 to 255 do
		Content[i] := Byte(i);

	FMockHTTP.SetStreamResponse('https://download.test.com/', Content);

	Stream := TMemoryStream.Create;
	try
		FMockHTTP.GetFile('https://download.test.com/binary.bin', Stream);

		SetLength(ReadBack, Stream.Size);
		Stream.Position := 0;
		Stream.Read(ReadBack[0], Stream.Size);

		Assert.AreEqual(Integer(256), Integer(Length(ReadBack)), 'Should have 256 bytes');
		Assert.IsTrue(CompareMem(@Content[0], @ReadBack[0], 256), 'Binary content should be preserved exactly');
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuDownloadTest.TestGetFile_LargeContent_HandledCorrectly;
var
	Content: TBytes;
	Stream: TMemoryStream;
	i: Integer;
const
	LARGE_SIZE = 1024 * 100; {100KB}
begin
	SetLength(Content, LARGE_SIZE);
	for i := 0 to LARGE_SIZE - 1 do
		Content[i] := Byte(i mod 256);

	FMockHTTP.SetStreamResponse('https://download.test.com/', Content);

	Stream := TMemoryStream.Create;
	try
		var ResultCode := FMockHTTP.GetFile('https://download.test.com/large.bin', Stream);

		Assert.AreEqual(FS_FILE_OK, ResultCode, 'Large download should succeed');
		Assert.AreEqual(Int64(LARGE_SIZE), Stream.Size, 'Should download entire content');
	finally
		Stream.Free;
	end;
end;

{Shard URL simulation tests}

procedure TCloudMailRuDownloadTest.TestGetFile_FromShardURL_Success;
var
	Content: TBytes;
	Stream: TMemoryStream;
begin
	{Simulate download from shard URL (typical pattern)}
	Content := TEncoding.UTF8.GetBytes('File from shard');
	FMockHTTP.SetStreamResponse('https://cloclo123.cloud.mail.ru/', Content);

	Stream := TMemoryStream.Create;
	try
		var ResultCode := FMockHTTP.GetFile('https://cloclo123.cloud.mail.ru/attach/path/file.txt', Stream);

		Assert.AreEqual(FS_FILE_OK, ResultCode, 'Download from shard should succeed');
		Assert.IsTrue(Stream.Size > 0, 'Should have content');
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuDownloadTest.TestGetFile_OAuth_DispatcherFormat;
var
	Response: WideString;
begin
	{OAuth dispatcher returns plain text: "URL IP COUNT"}
	Response := TMockShardHelper.CreateOAuthDownloadDispatcherResponse('https://oauth.download.test/');

	Assert.AreEqual(String('https://oauth.download.test/ 127.0.0.1 1'), String(Response),
		'OAuth dispatcher format should be plain text');
end;

initialization
	TDUnitX.RegisterTestFixture(TCloudMailRuDownloadTest);

end.

unit MockCloudHTTPStreamTest;

{Tests for TMockCloudHTTP enhanced capabilities:
 - Stream response support for GetFile/PutFile
 - Response queue for multi-step operation testing
 - Upload capture for verification}

interface

uses
	MockCloudHTTP,
	PLUGIN_TYPES,
	System.Classes,
	System.SysUtils,
	DUnitX.TestFramework;

type
	[TestFixture]
	TMockCloudHTTPStreamTest = class
	private
		FMockHTTP: TMockCloudHTTP;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Stream response tests}
		[Test]
		procedure TestSetStreamResponse_WritesContentToStream;
		[Test]
		procedure TestSetStreamResponse_ReturnsConfiguredResultCode;
		[Test]
		procedure TestSetStreamResponse_EmptyContent_WritesNothing;
		[Test]
		procedure TestSetStreamResponse_BinaryContent_PreservedExactly;

		{PutFile response tests}
		[Test]
		procedure TestSetPutFileResponse_ReturnsHash;
		[Test]
		procedure TestSetPutFileResponse_ReturnsResultCode;
		[Test]
		procedure TestPutFile_CapturesUploadContent;
		[Test]
		procedure TestPutFile_CapturesFileName;
		[Test]
		procedure TestPostFile_CapturesUpload;

		{Upload capture inspection tests}
		[Test]
		procedure TestGetUploadCount_TracksMultipleUploads;
		[Test]
		procedure TestGetUploadCapture_ReturnsCorrectCapture;
		[Test]
		procedure TestGetLastUploadCapture_ReturnsLast;
		[Test]
		procedure TestGetCapturedUploadContent_FindsByURLPattern;

		{Response queue tests}
		[Test]
		procedure TestQueueResponse_ReturnsInOrder;
		[Test]
		procedure TestQueueResponse_MultipleCalls_DequeuedSequentially;
		[Test]
		procedure TestQueueResponse_FallsBackToSetResponse;
		[Test]
		procedure TestQueueStreamResponse_ReturnsInOrder;
		[Test]
		procedure TestQueuePutFileResponse_ReturnsHashesInOrder;
		[Test]
		procedure TestHasPendingResponses_TrueWhenQueued;
		[Test]
		procedure TestHasPendingResponses_FalseWhenEmpty;
		[Test]
		procedure TestClearQueues_RemovesPendingResponses;

		{Multi-step flow tests}
		[Test]
		procedure TestMultiStepFlow_ShardThenUpload;
		[Test]
		procedure TestMultiStepFlow_TokenRefreshRetry;
	end;

implementation

{TMockCloudHTTPStreamTest}

procedure TMockCloudHTTPStreamTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
end;

procedure TMockCloudHTTPStreamTest.TearDown;
begin
	FMockHTTP := nil;
end;

{Stream response tests}

procedure TMockCloudHTTPStreamTest.TestSetStreamResponse_WritesContentToStream;
var
	Content: TBytes;
	Stream: TMemoryStream;
	ReadBack: TBytes;
begin
	Content := TEncoding.UTF8.GetBytes('Test file content');
	FMockHTTP.SetStreamResponse('http://test.com/file', Content);

	Stream := TMemoryStream.Create;
	try
		FMockHTTP.GetFile('http://test.com/file', Stream);

		SetLength(ReadBack, Stream.Size);
		Stream.Position := 0;
		Stream.Read(ReadBack[0], Stream.Size);

		Assert.AreEqual(Length(Content), Length(ReadBack), 'Content length should match');
		Assert.IsTrue(CompareMem(@Content[0], @ReadBack[0], Length(Content)), 'Content should match');
	finally
		Stream.Free;
	end;
end;

procedure TMockCloudHTTPStreamTest.TestSetStreamResponse_ReturnsConfiguredResultCode;
var
	Stream: TMemoryStream;
	ResultCode: Integer;
begin
	FMockHTTP.SetStreamResponse('http://test.com/file', nil, FS_FILE_NOTFOUND);

	Stream := TMemoryStream.Create;
	try
		ResultCode := FMockHTTP.GetFile('http://test.com/file', Stream);
		Assert.AreEqual(FS_FILE_NOTFOUND, ResultCode, 'Should return configured result code');
	finally
		Stream.Free;
	end;
end;

procedure TMockCloudHTTPStreamTest.TestSetStreamResponse_EmptyContent_WritesNothing;
var
	Stream: TMemoryStream;
begin
	FMockHTTP.SetStreamResponse('http://test.com/empty', nil);

	Stream := TMemoryStream.Create;
	try
		FMockHTTP.GetFile('http://test.com/empty', Stream);
		Assert.AreEqual(Int64(0), Stream.Size, 'Stream should be empty');
	finally
		Stream.Free;
	end;
end;

procedure TMockCloudHTTPStreamTest.TestSetStreamResponse_BinaryContent_PreservedExactly;
var
	Content: TBytes;
	Stream: TMemoryStream;
	ReadBack: TBytes;
	i: Integer;
begin
	{Create binary content with all byte values}
	SetLength(Content, 256);
	for i := 0 to 255 do
		Content[i] := Byte(i);

	FMockHTTP.SetStreamResponse('http://test.com/binary', Content);

	Stream := TMemoryStream.Create;
	try
		FMockHTTP.GetFile('http://test.com/binary', Stream);

		SetLength(ReadBack, Stream.Size);
		Stream.Position := 0;
		Stream.Read(ReadBack[0], Stream.Size);

		Assert.AreEqual(Integer(256), Integer(Length(ReadBack)), 'Should have 256 bytes');
		for i := 0 to 255 do
			Assert.AreEqual(Integer(i), Integer(ReadBack[i]), Format('Byte %d should match', [i]));
	finally
		Stream.Free;
	end;
end;

{PutFile response tests}

procedure TMockCloudHTTPStreamTest.TestSetPutFileResponse_ReturnsHash;
var
	Stream: TMemoryStream;
	Answer: WideString;
begin
	FMockHTTP.SetPutFileResponse('http://upload.test.com', 'ABCD1234567890ABCD1234567890ABCD12345678');

	Stream := TMemoryStream.Create;
	try
		FMockHTTP.PutFile('http://upload.test.com', 'test.txt', Stream, Answer);
		Assert.AreEqual(String('ABCD1234567890ABCD1234567890ABCD12345678'), String(Answer), 'Should return configured hash');
	finally
		Stream.Free;
	end;
end;

procedure TMockCloudHTTPStreamTest.TestSetPutFileResponse_ReturnsResultCode;
var
	Stream: TMemoryStream;
	Answer: WideString;
	ResultCode: Integer;
begin
	FMockHTTP.SetPutFileResponse('http://upload.test.com', 'HASH', FS_FILE_WRITEERROR);

	Stream := TMemoryStream.Create;
	try
		ResultCode := FMockHTTP.PutFile('http://upload.test.com', 'test.txt', Stream, Answer);
		Assert.AreEqual(FS_FILE_WRITEERROR, ResultCode, 'Should return configured result code');
	finally
		Stream.Free;
	end;
end;

procedure TMockCloudHTTPStreamTest.TestPutFile_CapturesUploadContent;
var
	Stream: TMemoryStream;
	Content: TBytes;
	Answer: WideString;
	Captured: TBytes;
begin
	Content := TEncoding.UTF8.GetBytes('Upload content here');
	FMockHTTP.SetPutFileResponse('http://upload.test.com', 'HASH');

	Stream := TMemoryStream.Create;
	try
		Stream.Write(Content[0], Length(Content));
		Stream.Position := 0;

		FMockHTTP.PutFile('http://upload.test.com', 'test.txt', Stream, Answer);

		Captured := FMockHTTP.GetLastUploadCapture.Content;
		Assert.AreEqual(Length(Content), Length(Captured), 'Captured content length should match');
		Assert.IsTrue(CompareMem(@Content[0], @Captured[0], Length(Content)), 'Captured content should match');
	finally
		Stream.Free;
	end;
end;

procedure TMockCloudHTTPStreamTest.TestPutFile_CapturesFileName;
var
	Stream: TMemoryStream;
	Answer: WideString;
begin
	FMockHTTP.SetPutFileResponse('http://upload.test.com', 'HASH');

	Stream := TMemoryStream.Create;
	try
		FMockHTTP.PutFile('http://upload.test.com', 'myfile.dat', Stream, Answer);

		Assert.AreEqual(String('myfile.dat'), String(FMockHTTP.GetLastUploadCapture.FileName), 'Should capture filename');
	finally
		Stream.Free;
	end;
end;

procedure TMockCloudHTTPStreamTest.TestPostFile_CapturesUpload;
var
	Stream: TMemoryStream;
	Content: TBytes;
	Answer: WideString;
begin
	Content := TEncoding.UTF8.GetBytes('PostFile content');
	FMockHTTP.SetResponse('http://post.test.com', True, 'OK');

	Stream := TMemoryStream.Create;
	try
		Stream.Write(Content[0], Length(Content));
		Stream.Position := 0;

		FMockHTTP.PostFile('http://post.test.com', 'posted.txt', Stream, Answer);

		Assert.AreEqual(1, FMockHTTP.GetUploadCount, 'Should have one upload captured');
		Assert.AreEqual(String('posted.txt'), String(FMockHTTP.GetLastUploadCapture.FileName), 'Should capture filename');
	finally
		Stream.Free;
	end;
end;

{Upload capture inspection tests}

procedure TMockCloudHTTPStreamTest.TestGetUploadCount_TracksMultipleUploads;
var
	Stream: TMemoryStream;
	Answer: WideString;
begin
	FMockHTTP.SetPutFileResponse('http://upload.test.com', 'HASH');

	Stream := TMemoryStream.Create;
	try
		FMockHTTP.PutFile('http://upload.test.com/1', 'file1.txt', Stream, Answer);
		FMockHTTP.PutFile('http://upload.test.com/2', 'file2.txt', Stream, Answer);
		FMockHTTP.PutFile('http://upload.test.com/3', 'file3.txt', Stream, Answer);

		Assert.AreEqual(3, FMockHTTP.GetUploadCount, 'Should track 3 uploads');
	finally
		Stream.Free;
	end;
end;

procedure TMockCloudHTTPStreamTest.TestGetUploadCapture_ReturnsCorrectCapture;
var
	Stream: TMemoryStream;
	Answer: WideString;
begin
	FMockHTTP.SetPutFileResponse('http://upload.test.com', 'HASH');

	Stream := TMemoryStream.Create;
	try
		FMockHTTP.PutFile('http://upload.test.com', 'first.txt', Stream, Answer);
		FMockHTTP.PutFile('http://upload.test.com', 'second.txt', Stream, Answer);

		Assert.AreEqual(String('first.txt'), String(FMockHTTP.GetUploadCapture(0).FileName), 'Index 0 should be first');
		Assert.AreEqual(String('second.txt'), String(FMockHTTP.GetUploadCapture(1).FileName), 'Index 1 should be second');
	finally
		Stream.Free;
	end;
end;

procedure TMockCloudHTTPStreamTest.TestGetLastUploadCapture_ReturnsLast;
var
	Stream: TMemoryStream;
	Answer: WideString;
begin
	FMockHTTP.SetPutFileResponse('http://upload.test.com', 'HASH');

	Stream := TMemoryStream.Create;
	try
		FMockHTTP.PutFile('http://upload.test.com', 'first.txt', Stream, Answer);
		FMockHTTP.PutFile('http://upload.test.com', 'last.txt', Stream, Answer);

		Assert.AreEqual(String('last.txt'), String(FMockHTTP.GetLastUploadCapture.FileName), 'Should return last upload');
	finally
		Stream.Free;
	end;
end;

procedure TMockCloudHTTPStreamTest.TestGetCapturedUploadContent_FindsByURLPattern;
var
	Stream1, Stream2: TMemoryStream;
	Content1, Content2: TBytes;
	Answer: WideString;
	Captured: TBytes;
begin
	Content1 := TEncoding.UTF8.GetBytes('Content for chunk1');
	Content2 := TEncoding.UTF8.GetBytes('Content for chunk2');
	FMockHTTP.SetPutFileResponse('http://upload.test.com', 'HASH');

	Stream1 := TMemoryStream.Create;
	Stream2 := TMemoryStream.Create;
	try
		Stream1.Write(Content1[0], Length(Content1));
		Stream1.Position := 0;
		Stream2.Write(Content2[0], Length(Content2));
		Stream2.Position := 0;

		FMockHTTP.PutFile('http://upload.test.com/chunk1', 'chunk1.dat', Stream1, Answer);
		FMockHTTP.PutFile('http://upload.test.com/chunk2', 'chunk2.dat', Stream2, Answer);

		Captured := FMockHTTP.GetCapturedUploadContent('chunk2');
		Assert.AreEqual(Length(Content2), Length(Captured), 'Should find chunk2 content by pattern');
	finally
		Stream1.Free;
		Stream2.Free;
	end;
end;

{Response queue tests}

procedure TMockCloudHTTPStreamTest.TestQueueResponse_ReturnsInOrder;
var
	Answer: WideString;
	Progress: Boolean;
begin
	FMockHTTP.QueueResponse('http://api.test.com', True, 'Response 1');
	FMockHTTP.QueueResponse('http://api.test.com', True, 'Response 2');
	FMockHTTP.QueueResponse('http://api.test.com', True, 'Response 3');

	Progress := True;
	FMockHTTP.GetPage('http://api.test.com', Answer, Progress);
	Assert.AreEqual(String('Response 1'), String(Answer), 'First call should get first response');

	FMockHTTP.GetPage('http://api.test.com', Answer, Progress);
	Assert.AreEqual(String('Response 2'), String(Answer), 'Second call should get second response');

	FMockHTTP.GetPage('http://api.test.com', Answer, Progress);
	Assert.AreEqual(String('Response 3'), String(Answer), 'Third call should get third response');
end;

procedure TMockCloudHTTPStreamTest.TestQueueResponse_MultipleCalls_DequeuedSequentially;
var
	Answer: WideString;
	Progress: Boolean;
begin
	{Queue responses for sequential consumption}
	FMockHTTP.QueueResponse('http://api.test.com', True, 'First');
	FMockHTTP.QueueResponse('http://api.test.com', True, 'Second');

	Progress := True;
	FMockHTTP.GetPage('http://api.test.com', Answer, Progress);
	Assert.AreEqual(String('First'), String(Answer), 'Should dequeue first');

	Assert.IsTrue(FMockHTTP.HasPendingResponses('http://api.test.com'), 'Should still have pending');

	FMockHTTP.GetPage('http://api.test.com', Answer, Progress);
	Assert.AreEqual(String('Second'), String(Answer), 'Should dequeue second');

	Assert.IsFalse(FMockHTTP.HasPendingResponses('http://api.test.com'), 'Queue should be empty');
end;

procedure TMockCloudHTTPStreamTest.TestQueueResponse_FallsBackToSetResponse;
var
	Answer: WideString;
	Progress: Boolean;
begin
	{Set a default response and queue one response}
	FMockHTTP.SetResponse('http://api.test.com', True, 'Default');
	FMockHTTP.QueueResponse('http://api.test.com', True, 'Queued');

	Progress := True;

	{First call gets queued response}
	FMockHTTP.GetPage('http://api.test.com', Answer, Progress);
	Assert.AreEqual(String('Queued'), String(Answer), 'Should get queued response first');

	{Second call falls back to set response}
	FMockHTTP.GetPage('http://api.test.com', Answer, Progress);
	Assert.AreEqual(String('Default'), String(Answer), 'Should fall back to set response');
end;

procedure TMockCloudHTTPStreamTest.TestQueueStreamResponse_ReturnsInOrder;
var
	Stream: TMemoryStream;
	Content1, Content2, ReadBack: TBytes;
begin
	Content1 := TEncoding.UTF8.GetBytes('First chunk');
	Content2 := TEncoding.UTF8.GetBytes('Second chunk');

	FMockHTTP.QueueStreamResponse('http://download.test.com', Content1);
	FMockHTTP.QueueStreamResponse('http://download.test.com', Content2);

	Stream := TMemoryStream.Create;
	try
		{First call}
		FMockHTTP.GetFile('http://download.test.com', Stream);
		SetLength(ReadBack, Stream.Size);
		Stream.Position := 0;
		Stream.Read(ReadBack[0], Stream.Size);
		Assert.AreEqual(Length(Content1), Length(ReadBack), 'First call should get first content');

		{Second call}
		Stream.Clear;
		FMockHTTP.GetFile('http://download.test.com', Stream);
		SetLength(ReadBack, Stream.Size);
		Stream.Position := 0;
		Stream.Read(ReadBack[0], Stream.Size);
		Assert.AreEqual(Length(Content2), Length(ReadBack), 'Second call should get second content');
	finally
		Stream.Free;
	end;
end;

procedure TMockCloudHTTPStreamTest.TestQueuePutFileResponse_ReturnsHashesInOrder;
var
	Stream: TMemoryStream;
	Answer: WideString;
begin
	FMockHTTP.QueuePutFileResponse('http://upload.test.com', 'HASH1111111111111111111111111111111111');
	FMockHTTP.QueuePutFileResponse('http://upload.test.com', 'HASH2222222222222222222222222222222222');

	Stream := TMemoryStream.Create;
	try
		FMockHTTP.PutFile('http://upload.test.com', 'file1.txt', Stream, Answer);
		Assert.AreEqual(String('HASH1111111111111111111111111111111111'), String(Answer), 'First upload should get first hash');

		FMockHTTP.PutFile('http://upload.test.com', 'file2.txt', Stream, Answer);
		Assert.AreEqual(String('HASH2222222222222222222222222222222222'), String(Answer), 'Second upload should get second hash');
	finally
		Stream.Free;
	end;
end;

procedure TMockCloudHTTPStreamTest.TestHasPendingResponses_TrueWhenQueued;
begin
	FMockHTTP.QueueResponse('http://api.test.com', True, 'Response');

	Assert.IsTrue(FMockHTTP.HasPendingResponses('http://api.test.com'), 'Should have pending responses');
end;

procedure TMockCloudHTTPStreamTest.TestHasPendingResponses_FalseWhenEmpty;
begin
	Assert.IsFalse(FMockHTTP.HasPendingResponses('http://api.test.com'), 'Should not have pending responses');
end;

procedure TMockCloudHTTPStreamTest.TestClearQueues_RemovesPendingResponses;
begin
	FMockHTTP.QueueResponse('http://api.test.com', True, 'Response');
	FMockHTTP.QueueStreamResponse('http://download.test.com', nil);

	FMockHTTP.ClearQueues;

	Assert.IsFalse(FMockHTTP.HasPendingResponses('http://api.test.com'), 'Regular queue should be cleared');
	Assert.IsFalse(FMockHTTP.HasPendingResponses('http://download.test.com'), 'Stream queue should be cleared');
end;

{Multi-step flow tests}

procedure TMockCloudHTTPStreamTest.TestMultiStepFlow_ShardThenUpload;
var
	Stream: TMemoryStream;
	Answer: WideString;
	Content: TBytes;
const
	{Typical dispatcher response}
	JSON_DISPATCHER = '{"email":"test@mail.ru","body":{"upload":[{"url":"http://upload-shard.test.com/"}]},"status":200}';
begin
	{Setup: first call gets shard URL, second call uploads}
	FMockHTTP.QueueResponse('/dispatcher/', True, JSON_DISPATCHER);
	FMockHTTP.QueuePutFileResponse('http://upload-shard.test.com', 'ABCD1234567890ABCD1234567890ABCD12345678');

	Content := TEncoding.UTF8.GetBytes('File content to upload');

	{Step 1: Get shard}
	Assert.IsTrue(FMockHTTP.PostForm('https://cloud.mail.ru/api/v2/dispatcher/', '', Answer), 'Should get dispatcher response');
	Assert.IsTrue(Pos('upload-shard', String(Answer)) > 0, 'Response should contain shard URL');

	{Step 2: Upload to shard}
	Stream := TMemoryStream.Create;
	try
		Stream.Write(Content[0], Length(Content));
		Stream.Position := 0;

		var ResultCode := FMockHTTP.PutFile('http://upload-shard.test.com/', 'test.txt', Stream, Answer);

		Assert.AreEqual(FS_FILE_OK, ResultCode, 'Upload should succeed');
		Assert.AreEqual(String('ABCD1234567890ABCD1234567890ABCD12345678'), String(Answer), 'Should return hash');
	finally
		Stream.Free;
	end;
end;

procedure TMockCloudHTTPStreamTest.TestMultiStepFlow_TokenRefreshRetry;
var
	Answer: WideString;
	Progress: Boolean;
const
	JSON_TOKEN_ERROR = '{"email":"test@mail.ru","body":{"home":{"error":"token"}},"status":403}';
	JSON_TOKEN_SUCCESS = '{"token":"new_csrf_token"}';
	JSON_OPERATION_SUCCESS = '{"email":"test@mail.ru","body":{},"status":200}';
begin
	{Simulate: first call fails with token error, refresh succeeds, retry succeeds}
	FMockHTTP.QueueResponse('/api/v2/folder/add', True, JSON_TOKEN_ERROR);
	FMockHTTP.QueueResponse('/tokens/csrf', True, JSON_TOKEN_SUCCESS);
	FMockHTTP.QueueResponse('/api/v2/folder/add', True, JSON_OPERATION_SUCCESS);

	Progress := True;

	{First call - gets token error}
	FMockHTTP.GetPage('https://cloud.mail.ru/api/v2/folder/add', Answer, Progress);
	Assert.IsTrue(Pos('token', String(Answer)) > 0, 'First call should fail with token error');

	{Token refresh}
	FMockHTTP.GetPage('https://cloud.mail.ru/tokens/csrf', Answer, Progress);
	Assert.IsTrue(Pos('new_csrf_token', String(Answer)) > 0, 'Should get new token');

	{Retry - succeeds}
	FMockHTTP.GetPage('https://cloud.mail.ru/api/v2/folder/add', Answer, Progress);
	Assert.IsTrue(Pos('status":200', String(Answer)) > 0, 'Retry should succeed');
end;

initialization
	TDUnitX.RegisterTestFixture(TMockCloudHTTPStreamTest);

end.

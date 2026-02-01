unit CloudErrorMapperTest;

interface

uses
	CloudErrorMapper,
	CloudOperationResult,
	CloudConstants,
	WFXTypes,
	Logger,
	TestHelper,
	System.SysUtils,
	DUnitX.TestFramework;

type
	{Tests for TCloudErrorMapper static helper class}
	[TestFixture]
	TCloudErrorMapperTest = class
	public
		{ ToFsResult tests - success cases }
		[Test]
		procedure TestToFsResult_OK_ReturnsFileOK;
		[Test]
		procedure TestToFsResult_Exists_ReturnsFileExists;
		[Test]
		procedure TestToFsResult_Unknown_ReturnsNotSupported;

		{ ToFsResult tests - error cases mapped to WRITEERROR }
		[Test]
		procedure TestToFsResult_Required_ReturnsWriteError;
		[Test]
		procedure TestToFsResult_Invalid_ReturnsWriteError;
		[Test]
		procedure TestToFsResult_ReadOnly_ReturnsWriteError;
		[Test]
		procedure TestToFsResult_NameLengthExceeded_ReturnsWriteError;
		[Test]
		procedure TestToFsResult_Overquota_ReturnsWriteError;
		[Test]
		procedure TestToFsResult_NameTooLong_ReturnsWriteError;
		[Test]
		procedure TestToFsResult_UnknownCode_ReturnsWriteError;

		{ ToFsResult tests - JSON parsing }
		[Test]
		procedure TestToFsResult_JSON_Success;
		[Test]
		procedure TestToFsResult_JSON_Error;

		{ ToBoolean tests }
		[Test]
		procedure TestToBoolean_OK_ReturnsTrue;
		[Test]
		procedure TestToBoolean_Error_ReturnsFalse;
		[Test]
		procedure TestToBoolean_JSON_Success;
		[Test]
		procedure TestToBoolean_JSON_Error;

		{ ErrorCodeText tests }
		[Test]
		procedure TestErrorCodeText_Exists;
		[Test]
		procedure TestErrorCodeText_Required;
		[Test]
		procedure TestErrorCodeText_Invalid;
		[Test]
		procedure TestErrorCodeText_ReadOnly;
		[Test]
		procedure TestErrorCodeText_NameLengthExceeded;
		[Test]
		procedure TestErrorCodeText_Overquota;
		[Test]
		procedure TestErrorCodeText_NotExists;
		[Test]
		procedure TestErrorCodeText_Own;
		[Test]
		procedure TestErrorCodeText_NameTooLong;
		[Test]
		procedure TestErrorCodeText_VirusScanFail;
		[Test]
		procedure TestErrorCodeText_Owner;
		[Test]
		procedure TestErrorCodeText_Fahrenheit;
		[Test]
		procedure TestErrorCodeText_BadRequest;
		[Test]
		procedure TestErrorCodeText_TreesConflict;
		[Test]
		procedure TestErrorCodeText_UnprocessableEntry;
		[Test]
		procedure TestErrorCodeText_UserLimitExceeded;
		[Test]
		procedure TestErrorCodeText_ExportLimitExceeded;
		[Test]
		procedure TestErrorCodeText_NotAcceptable;
		[Test]
		procedure TestErrorCodeText_Unknown;

	end;

implementation

uses
	LanguageStrings;

{ Helper function to create TCloudOperationResult }
function MakeResult(Code: Integer; Status: Integer = 0): TCloudOperationResult;
begin
	Result.OperationResult := Code;
	Result.OperationStatus := Status;
end;

{ TCloudErrorMapperTest }

{ ToFsResult tests - success cases }

procedure TCloudErrorMapperTest.TestToFsResult_OK_ReturnsFileOK;
begin
	Assert.AreEqual(FS_FILE_OK, TCloudErrorMapper.ToFsResult(MakeResult(CLOUD_OPERATION_OK), TNullLogger.Create));
end;

procedure TCloudErrorMapperTest.TestToFsResult_Exists_ReturnsFileExists;
begin
	Assert.AreEqual(FS_FILE_EXISTS, TCloudErrorMapper.ToFsResult(MakeResult(CLOUD_ERROR_EXISTS), TNullLogger.Create));
end;

procedure TCloudErrorMapperTest.TestToFsResult_Unknown_ReturnsNotSupported;
begin
	Assert.AreEqual(FS_FILE_NOTSUPPORTED, TCloudErrorMapper.ToFsResult(MakeResult(CLOUD_ERROR_UNKNOWN), TNullLogger.Create));
end;

{ ToFsResult tests - error cases mapped to WRITEERROR }

procedure TCloudErrorMapperTest.TestToFsResult_Required_ReturnsWriteError;
begin
	Assert.AreEqual(FS_FILE_WRITEERROR, TCloudErrorMapper.ToFsResult(MakeResult(CLOUD_ERROR_REQUIRED), TNullLogger.Create));
end;

procedure TCloudErrorMapperTest.TestToFsResult_Invalid_ReturnsWriteError;
begin
	Assert.AreEqual(FS_FILE_WRITEERROR, TCloudErrorMapper.ToFsResult(MakeResult(CLOUD_ERROR_INVALID), TNullLogger.Create));
end;

procedure TCloudErrorMapperTest.TestToFsResult_ReadOnly_ReturnsWriteError;
begin
	Assert.AreEqual(FS_FILE_WRITEERROR, TCloudErrorMapper.ToFsResult(MakeResult(CLOUD_ERROR_READONLY), TNullLogger.Create));
end;

procedure TCloudErrorMapperTest.TestToFsResult_NameLengthExceeded_ReturnsWriteError;
begin
	Assert.AreEqual(FS_FILE_WRITEERROR, TCloudErrorMapper.ToFsResult(MakeResult(CLOUD_ERROR_NAME_LENGTH_EXCEEDED), TNullLogger.Create));
end;

procedure TCloudErrorMapperTest.TestToFsResult_Overquota_ReturnsWriteError;
begin
	Assert.AreEqual(FS_FILE_WRITEERROR, TCloudErrorMapper.ToFsResult(MakeResult(CLOUD_ERROR_OVERQUOTA), TNullLogger.Create));
end;

procedure TCloudErrorMapperTest.TestToFsResult_NameTooLong_ReturnsWriteError;
begin
	Assert.AreEqual(FS_FILE_WRITEERROR, TCloudErrorMapper.ToFsResult(MakeResult(CLOUD_ERROR_NAME_TOO_LONG), TNullLogger.Create));
end;

procedure TCloudErrorMapperTest.TestToFsResult_UnknownCode_ReturnsWriteError;
begin
	{ Any unhandled error code should return WRITEERROR }
	Assert.AreEqual(FS_FILE_WRITEERROR, TCloudErrorMapper.ToFsResult(MakeResult(9999), TNullLogger.Create));
end;

{ ToFsResult tests - JSON parsing }

procedure TCloudErrorMapperTest.TestToFsResult_JSON_Success;
const
	JSON = '{"status":200,"body":{}}';
begin
	Assert.AreEqual(FS_FILE_OK, TCloudErrorMapper.ToFsResult(JSON, TNullLogger.Create));
end;

procedure TCloudErrorMapperTest.TestToFsResult_JSON_Error;
const
	JSON = '{"status":406,"body":{"error":"exists"}}';
begin
	{ Status 406 maps to some error - verify it doesn't crash }
	var Result := TCloudErrorMapper.ToFsResult(JSON, TNullLogger.Create);
	Assert.IsTrue(Result in [FS_FILE_WRITEERROR, FS_FILE_EXISTS, FS_FILE_NOTSUPPORTED],
		'Should return an error code');
end;

{ ToBoolean tests }

procedure TCloudErrorMapperTest.TestToBoolean_OK_ReturnsTrue;
begin
	Assert.IsTrue(TCloudErrorMapper.ToBoolean(MakeResult(CLOUD_OPERATION_OK), TNullLogger.Create));
end;

procedure TCloudErrorMapperTest.TestToBoolean_Error_ReturnsFalse;
begin
	Assert.IsFalse(TCloudErrorMapper.ToBoolean(MakeResult(CLOUD_ERROR_EXISTS), TNullLogger.Create));
end;

procedure TCloudErrorMapperTest.TestToBoolean_JSON_Success;
const
	JSON = '{"status":200,"body":{}}';
begin
	Assert.IsTrue(TCloudErrorMapper.ToBoolean(JSON, TNullLogger.Create));
end;

procedure TCloudErrorMapperTest.TestToBoolean_JSON_Error;
const
	JSON = '{"status":406,"body":{"error":"exists"}}';
begin
	Assert.IsFalse(TCloudErrorMapper.ToBoolean(JSON, TNullLogger.Create));
end;

{ ErrorCodeText tests }

procedure TCloudErrorMapperTest.TestErrorCodeText_Exists;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_EXISTS, TCloudErrorMapper.ErrorCodeText(CLOUD_ERROR_EXISTS));
end;

procedure TCloudErrorMapperTest.TestErrorCodeText_Required;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_REQUIRED, TCloudErrorMapper.ErrorCodeText(CLOUD_ERROR_REQUIRED));
end;

procedure TCloudErrorMapperTest.TestErrorCodeText_Invalid;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_INVALID, TCloudErrorMapper.ErrorCodeText(CLOUD_ERROR_INVALID));
end;

procedure TCloudErrorMapperTest.TestErrorCodeText_ReadOnly;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_READONLY, TCloudErrorMapper.ErrorCodeText(CLOUD_ERROR_READONLY));
end;

procedure TCloudErrorMapperTest.TestErrorCodeText_NameLengthExceeded;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_NAME_LENGTH_EXCEEDED, TCloudErrorMapper.ErrorCodeText(CLOUD_ERROR_NAME_LENGTH_EXCEEDED));
end;

procedure TCloudErrorMapperTest.TestErrorCodeText_Overquota;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_OVERQUOTA, TCloudErrorMapper.ErrorCodeText(CLOUD_ERROR_OVERQUOTA));
end;

procedure TCloudErrorMapperTest.TestErrorCodeText_NotExists;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_NOT_EXISTS, TCloudErrorMapper.ErrorCodeText(CLOUD_ERROR_NOT_EXISTS));
end;

procedure TCloudErrorMapperTest.TestErrorCodeText_Own;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_OWN, TCloudErrorMapper.ErrorCodeText(CLOUD_ERROR_OWN));
end;

procedure TCloudErrorMapperTest.TestErrorCodeText_NameTooLong;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_NAME_TOO_LONG, TCloudErrorMapper.ErrorCodeText(CLOUD_ERROR_NAME_TOO_LONG));
end;

procedure TCloudErrorMapperTest.TestErrorCodeText_VirusScanFail;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_VIRUS_SCAN_FAIL, TCloudErrorMapper.ErrorCodeText(CLOUD_ERROR_VIRUS_SCAN_FAIL));
end;

procedure TCloudErrorMapperTest.TestErrorCodeText_Owner;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_OWNER, TCloudErrorMapper.ErrorCodeText(CLOUD_ERROR_OWNER));
end;

procedure TCloudErrorMapperTest.TestErrorCodeText_Fahrenheit;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_FAHRENHEIT, TCloudErrorMapper.ErrorCodeText(CLOUD_ERROR_FAHRENHEIT));
end;

procedure TCloudErrorMapperTest.TestErrorCodeText_BadRequest;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_BAD_REQUEST, TCloudErrorMapper.ErrorCodeText(CLOUD_ERROR_BAD_REQUEST));
end;

procedure TCloudErrorMapperTest.TestErrorCodeText_TreesConflict;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_TREES_CONFLICT, TCloudErrorMapper.ErrorCodeText(CLOUD_ERROR_TREES_CONFLICT));
end;

procedure TCloudErrorMapperTest.TestErrorCodeText_UnprocessableEntry;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_UNPROCESSABLE_ENTRY, TCloudErrorMapper.ErrorCodeText(CLOUD_ERROR_UNPROCESSABLE_ENTRY));
end;

procedure TCloudErrorMapperTest.TestErrorCodeText_UserLimitExceeded;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_USER_LIMIT_EXCEEDED, TCloudErrorMapper.ErrorCodeText(CLOUD_ERROR_USER_LIMIT_EXCEEDED));
end;

procedure TCloudErrorMapperTest.TestErrorCodeText_ExportLimitExceeded;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED, TCloudErrorMapper.ErrorCodeText(CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED));
end;

procedure TCloudErrorMapperTest.TestErrorCodeText_NotAcceptable;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_NOT_ACCEPTABLE, TCloudErrorMapper.ErrorCodeText(CLOUD_ERROR_NOT_ACCEPTABLE));
end;

procedure TCloudErrorMapperTest.TestErrorCodeText_Unknown;
var
	ErrorText: WideString;
begin
	ErrorText := TCloudErrorMapper.ErrorCodeText(12345);
	Assert.IsTrue(Pos(WideString('12345'), ErrorText) > 0, 'Unknown error should include the error code in the message');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudErrorMapperTest);

end.

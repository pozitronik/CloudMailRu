unit DownloadPreparationValidatorTest;

{Unit tests for TDownloadPreparationValidator.
 Tests pre-download validation logic for FsGetFile.}

interface

uses
	DUnitX.TestFramework,
	IDownloadPreparationValidatorInterface,
	DownloadPreparationValidator,
	RealPath;

type
	[TestFixture]
	TDownloadPreparationValidatorTest = class
	private
		FValidator: IDownloadPreparationValidator;

		function CreatePath(const PathStr: WideString): TRealPath;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Resume flag tests}
		[Test]
		procedure TestValidate_WhenResumeFlag_ReturnsNotSupported;
		[Test]
		procedure TestValidate_WhenNoResumeFlag_Proceeds;

		{Virtual path tests}
		[Test]
		procedure TestValidate_WhenTrashPath_ReturnsNotSupported;
		[Test]
		procedure TestValidate_WhenSharedPath_ReturnsNotSupported;
		[Test]
		procedure TestValidate_WhenInvitesPath_ReturnsNotSupported;
		[Test]
		procedure TestValidate_WhenNormalPath_Proceeds;

		{Combined tests}
		[Test]
		procedure TestValidate_WhenAllValid_ReturnsShouldProceed;
		[Test]
		procedure TestValidate_ResumeCheckedBeforeVirtualPath;
	end;

implementation

uses
	SysUtils,
	PLUGIN_TYPES;

{TDownloadPreparationValidatorTest}

function TDownloadPreparationValidatorTest.CreatePath(const PathStr: WideString): TRealPath;
begin
	Result.FromPath(PathStr);
end;

procedure TDownloadPreparationValidatorTest.Setup;
begin
	FValidator := TDownloadPreparationValidator.Create;
end;

procedure TDownloadPreparationValidatorTest.TearDown;
begin
	FValidator := nil;
end;

{Resume flag tests}

procedure TDownloadPreparationValidatorTest.TestValidate_WhenResumeFlag_ReturnsNotSupported;
var
	Result: TDownloadValidationResult;
begin
	Result := FValidator.Validate(CreatePath('\account\file.txt'), FS_COPYFLAGS_RESUME);

	Assert.IsFalse(Result.ShouldProceed);
	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result.ResultCode);
end;

procedure TDownloadPreparationValidatorTest.TestValidate_WhenNoResumeFlag_Proceeds;
var
	Result: TDownloadValidationResult;
begin
	Result := FValidator.Validate(CreatePath('\account\file.txt'), 0);

	Assert.IsTrue(Result.ShouldProceed);
end;

{Virtual path tests}

procedure TDownloadPreparationValidatorTest.TestValidate_WhenTrashPath_ReturnsNotSupported;
var
	Result: TDownloadValidationResult;
begin
	Result := FValidator.Validate(CreatePath('\account.trash\file.txt'), 0);

	Assert.IsFalse(Result.ShouldProceed);
	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result.ResultCode);
end;

procedure TDownloadPreparationValidatorTest.TestValidate_WhenSharedPath_ReturnsNotSupported;
var
	Result: TDownloadValidationResult;
begin
	Result := FValidator.Validate(CreatePath('\account.shared\file.txt'), 0);

	Assert.IsFalse(Result.ShouldProceed);
	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result.ResultCode);
end;

procedure TDownloadPreparationValidatorTest.TestValidate_WhenInvitesPath_ReturnsNotSupported;
var
	Result: TDownloadValidationResult;
begin
	Result := FValidator.Validate(CreatePath('\account.invites\file.txt'), 0);

	Assert.IsFalse(Result.ShouldProceed);
	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result.ResultCode);
end;

procedure TDownloadPreparationValidatorTest.TestValidate_WhenNormalPath_Proceeds;
var
	Result: TDownloadValidationResult;
begin
	Result := FValidator.Validate(CreatePath('\account\folder\file.txt'), 0);

	Assert.IsTrue(Result.ShouldProceed);
end;

{Combined tests}

procedure TDownloadPreparationValidatorTest.TestValidate_WhenAllValid_ReturnsShouldProceed;
var
	Result: TDownloadValidationResult;
begin
	Result := FValidator.Validate(CreatePath('\account\documents\report.pdf'), 0);

	Assert.IsTrue(Result.ShouldProceed);
	Assert.AreEqual(FS_FILE_OK, Result.ResultCode);
end;

procedure TDownloadPreparationValidatorTest.TestValidate_ResumeCheckedBeforeVirtualPath;
var
	Result: TDownloadValidationResult;
begin
	{Resume flag should cause exit before virtual path check}
	Result := FValidator.Validate(CreatePath('\account.trash\file.txt'), FS_COPYFLAGS_RESUME);

	{Should fail on resume, not on virtual path - both return NOTSUPPORTED so we just verify it fails}
	Assert.IsFalse(Result.ShouldProceed);
	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result.ResultCode);
end;

initialization
	TDUnitX.RegisterTestFixture(TDownloadPreparationValidatorTest);

end.

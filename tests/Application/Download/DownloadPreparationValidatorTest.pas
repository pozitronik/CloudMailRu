unit DownloadPreparationValidatorTest;

{Unit tests for TDownloadPreparationValidator.
 Tests pre-download validation logic for FsGetFile.}

interface

uses
	DUnitX.TestFramework,
	DownloadPreparationValidator,
	MockSettingsManager,
	RealPath;

type
	[TestFixture]
	TDownloadPreparationValidatorTest = class
	private
		FMockSettings: TMockSettingsManager;
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

		{Metadata file copy tests}
		[Test]
		procedure TestValidate_DescriptionFile_CopyDisabled_SkipsWithOK;
		[Test]
		procedure TestValidate_DescriptionFile_CopyEnabled_Proceeds;
		[Test]
		procedure TestValidate_TimestampFile_CopyDisabled_SkipsWithOK;
		[Test]
		procedure TestValidate_TimestampFile_CopyEnabled_Proceeds;
		[Test]
		procedure TestValidate_RegularFile_CopyDisabled_Proceeds;
		[Test]
		procedure TestValidate_DescriptionFile_CaseInsensitive;
	end;

implementation

uses
	SysUtils,
	PluginSettings,
	WFXTypes;

{TDownloadPreparationValidatorTest}

function TDownloadPreparationValidatorTest.CreatePath(const PathStr: WideString): TRealPath;
begin
	Result.FromPath(PathStr);
end;

procedure TDownloadPreparationValidatorTest.Setup;
var
	Settings: TPluginSettings;
begin
	FMockSettings := TMockSettingsManager.Create;
	{Set default filenames matching real defaults}
	Settings := FMockSettings.GetSettings;
	Settings.DescriptionFileName := 'descript.ion';
	Settings.TimestampFileName := '.cloud_timestamps';
	FMockSettings.SetSettings(Settings);
	FValidator := TDownloadPreparationValidator.Create(FMockSettings);
end;

procedure TDownloadPreparationValidatorTest.TearDown;
begin
	FValidator := nil;
	FMockSettings := nil;
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

{Metadata file copy tests}

procedure TDownloadPreparationValidatorTest.TestValidate_DescriptionFile_CopyDisabled_SkipsWithOK;
var
	Result: TDownloadValidationResult;
begin
	{CopyDescriptionFiles defaults to False}
	Result := FValidator.Validate(CreatePath('\account\folder\descript.ion'), 0);

	Assert.IsFalse(Result.ShouldProceed, 'Should not proceed when copy is disabled');
	Assert.AreEqual(FS_FILE_OK, Result.ResultCode, 'Should return FS_FILE_OK for silent skip');
end;

procedure TDownloadPreparationValidatorTest.TestValidate_DescriptionFile_CopyEnabled_Proceeds;
var
	Result: TDownloadValidationResult;
begin
	FMockSettings.SetCopyDescriptionFiles(True);

	Result := FValidator.Validate(CreatePath('\account\folder\descript.ion'), 0);

	Assert.IsTrue(Result.ShouldProceed, 'Should proceed when copy is enabled');
	Assert.AreEqual(FS_FILE_OK, Result.ResultCode);
end;

procedure TDownloadPreparationValidatorTest.TestValidate_TimestampFile_CopyDisabled_SkipsWithOK;
var
	Result: TDownloadValidationResult;
begin
	{CopyTimestampFiles defaults to False}
	Result := FValidator.Validate(CreatePath('\account\folder\.cloud_timestamps'), 0);

	Assert.IsFalse(Result.ShouldProceed, 'Should not proceed when copy is disabled');
	Assert.AreEqual(FS_FILE_OK, Result.ResultCode, 'Should return FS_FILE_OK for silent skip');
end;

procedure TDownloadPreparationValidatorTest.TestValidate_TimestampFile_CopyEnabled_Proceeds;
var
	Result: TDownloadValidationResult;
begin
	FMockSettings.SetCopyTimestampFiles(True);

	Result := FValidator.Validate(CreatePath('\account\folder\.cloud_timestamps'), 0);

	Assert.IsTrue(Result.ShouldProceed, 'Should proceed when copy is enabled');
	Assert.AreEqual(FS_FILE_OK, Result.ResultCode);
end;

procedure TDownloadPreparationValidatorTest.TestValidate_RegularFile_CopyDisabled_Proceeds;
var
	Result: TDownloadValidationResult;
begin
	{Both Copy*Files default to False, but regular files should not be affected}
	Result := FValidator.Validate(CreatePath('\account\folder\document.txt'), 0);

	Assert.IsTrue(Result.ShouldProceed, 'Regular files should not be affected by metadata copy settings');
	Assert.AreEqual(FS_FILE_OK, Result.ResultCode);
end;

procedure TDownloadPreparationValidatorTest.TestValidate_DescriptionFile_CaseInsensitive;
var
	Result: TDownloadValidationResult;
begin
	{Verify case-insensitive matching of metadata filenames}
	Result := FValidator.Validate(CreatePath('\account\folder\DESCRIPT.ION'), 0);

	Assert.IsFalse(Result.ShouldProceed, 'Case-insensitive match should skip the file');
	Assert.AreEqual(FS_FILE_OK, Result.ResultCode);
end;

initialization
	TDUnitX.RegisterTestFixture(TDownloadPreparationValidatorTest);

end.

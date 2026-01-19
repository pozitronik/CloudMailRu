unit UploadPreparationValidatorTest;

{Unit tests for TUploadPreparationValidator.
 Tests pre-upload validation logic for FsPutFile.}

interface

uses
	DUnitX.TestFramework,
	IUploadPreparationValidatorInterface,
	UploadPreparationValidator,
	RealPath;

type
	[TestFixture]
	TUploadPreparationValidatorTest = class
	private
		FFileExists: Boolean;
		FValidator: IUploadPreparationValidator;

		function CreateValidator: IUploadPreparationValidator;
		function CreatePath(const PathStr: WideString): TRealPath;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{File existence tests}
		[Test]
		procedure TestValidate_WhenFileNotExists_ReturnsNotFound;
		[Test]
		procedure TestValidate_WhenFileExists_Proceeds;

		{Path validation tests}
		[Test]
		procedure TestValidate_WhenAccountEmpty_ReturnsNotSupported;
		[Test]
		procedure TestValidate_WhenVirtualPath_ReturnsNotSupported;
		[Test]
		procedure TestValidate_WhenValidPath_Proceeds;

		{Resume flag tests}
		[Test]
		procedure TestValidate_WhenResumeFlag_ReturnsNotSupported;

		{Conflict tests}
		[Test]
		procedure TestValidate_WhenExistsSameCaseNoOverwrite_ReturnsExists;
		[Test]
		procedure TestValidate_WhenExistsDifferentCaseNoOverwrite_ReturnsExists;
		[Test]
		procedure TestValidate_WhenExistsWithOverwrite_Proceeds;

		{Overwrite flag tests}
		[Test]
		procedure TestValidate_WhenOverwriteFlag_SetsRequiresOverwrite;
		[Test]
		procedure TestValidate_WhenNoOverwriteFlag_DoesNotSetRequiresOverwrite;

		{Combined validation tests}
		[Test]
		procedure TestValidate_WhenAllValid_ReturnsShouldProceed;
	end;

implementation

uses
	SysUtils,
	PLUGIN_TYPES;

{TUploadPreparationValidatorTest}

function TUploadPreparationValidatorTest.CreateValidator: IUploadPreparationValidator;
begin
	Result := TUploadPreparationValidator.Create(
		function(const Path: WideString): Boolean
		begin
			Result := FFileExists;
		end
	);
end;

function TUploadPreparationValidatorTest.CreatePath(const PathStr: WideString): TRealPath;
begin
	Result.FromPath(PathStr);
end;

procedure TUploadPreparationValidatorTest.Setup;
begin
	FFileExists := True; {Default to file exists}
	FValidator := CreateValidator;
end;

procedure TUploadPreparationValidatorTest.TearDown;
begin
	FValidator := nil;
end;

{File existence tests}

procedure TUploadPreparationValidatorTest.TestValidate_WhenFileNotExists_ReturnsNotFound;
var
	Result: TUploadValidationResult;
begin
	FFileExists := False;
	FValidator := CreateValidator;

	Result := FValidator.Validate('C:\nonexistent.txt', CreatePath('\account\file.txt'), 0);

	Assert.IsFalse(Result.ShouldProceed);
	Assert.AreEqual(FS_FILE_NOTFOUND, Result.ResultCode);
end;

procedure TUploadPreparationValidatorTest.TestValidate_WhenFileExists_Proceeds;
var
	Result: TUploadValidationResult;
begin
	FFileExists := True;
	FValidator := CreateValidator;

	Result := FValidator.Validate('C:\existing.txt', CreatePath('\account\file.txt'), 0);

	Assert.IsTrue(Result.ShouldProceed);
end;

{Path validation tests}

procedure TUploadPreparationValidatorTest.TestValidate_WhenAccountEmpty_ReturnsNotSupported;
var
	Result: TUploadValidationResult;
	EmptyPath: TRealPath;
begin
	EmptyPath.FromPath('\');

	Result := FValidator.Validate('C:\file.txt', EmptyPath, 0);

	Assert.IsFalse(Result.ShouldProceed);
	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result.ResultCode);
end;

procedure TUploadPreparationValidatorTest.TestValidate_WhenVirtualPath_ReturnsNotSupported;
var
	Result: TUploadValidationResult;
begin
	Result := FValidator.Validate('C:\file.txt', CreatePath('\account.trash\file.txt'), 0);

	Assert.IsFalse(Result.ShouldProceed);
	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result.ResultCode);
end;

procedure TUploadPreparationValidatorTest.TestValidate_WhenValidPath_Proceeds;
var
	Result: TUploadValidationResult;
begin
	Result := FValidator.Validate('C:\file.txt', CreatePath('\account\folder\file.txt'), 0);

	Assert.IsTrue(Result.ShouldProceed);
end;

{Resume flag tests}

procedure TUploadPreparationValidatorTest.TestValidate_WhenResumeFlag_ReturnsNotSupported;
var
	Result: TUploadValidationResult;
begin
	Result := FValidator.Validate('C:\file.txt', CreatePath('\account\file.txt'), FS_COPYFLAGS_RESUME);

	Assert.IsFalse(Result.ShouldProceed);
	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result.ResultCode);
end;

{Conflict tests}

procedure TUploadPreparationValidatorTest.TestValidate_WhenExistsSameCaseNoOverwrite_ReturnsExists;
var
	Result: TUploadValidationResult;
begin
	Result := FValidator.Validate('C:\file.txt', CreatePath('\account\file.txt'), FS_COPYFLAGS_EXISTS_SAMECASE);

	Assert.IsFalse(Result.ShouldProceed);
	Assert.AreEqual(FS_FILE_EXISTS, Result.ResultCode);
end;

procedure TUploadPreparationValidatorTest.TestValidate_WhenExistsDifferentCaseNoOverwrite_ReturnsExists;
var
	Result: TUploadValidationResult;
begin
	Result := FValidator.Validate('C:\file.txt', CreatePath('\account\file.txt'), FS_COPYFLAGS_EXISTS_DIFFERENTCASE);

	Assert.IsFalse(Result.ShouldProceed);
	Assert.AreEqual(FS_FILE_EXISTS, Result.ResultCode);
end;

procedure TUploadPreparationValidatorTest.TestValidate_WhenExistsWithOverwrite_Proceeds;
var
	Result: TUploadValidationResult;
begin
	Result := FValidator.Validate('C:\file.txt', CreatePath('\account\file.txt'),
		FS_COPYFLAGS_EXISTS_SAMECASE or FS_COPYFLAGS_OVERWRITE);

	Assert.IsTrue(Result.ShouldProceed);
end;

{Overwrite flag tests}

procedure TUploadPreparationValidatorTest.TestValidate_WhenOverwriteFlag_SetsRequiresOverwrite;
var
	Result: TUploadValidationResult;
begin
	Result := FValidator.Validate('C:\file.txt', CreatePath('\account\file.txt'), FS_COPYFLAGS_OVERWRITE);

	Assert.IsTrue(Result.RequiresOverwrite);
end;

procedure TUploadPreparationValidatorTest.TestValidate_WhenNoOverwriteFlag_DoesNotSetRequiresOverwrite;
var
	Result: TUploadValidationResult;
begin
	Result := FValidator.Validate('C:\file.txt', CreatePath('\account\file.txt'), 0);

	Assert.IsFalse(Result.RequiresOverwrite);
end;

{Combined validation tests}

procedure TUploadPreparationValidatorTest.TestValidate_WhenAllValid_ReturnsShouldProceed;
var
	Result: TUploadValidationResult;
begin
	FFileExists := True;
	FValidator := CreateValidator;

	Result := FValidator.Validate('C:\valid.txt', CreatePath('\account\folder\file.txt'), 0);

	Assert.IsTrue(Result.ShouldProceed);
	Assert.AreEqual(FS_FILE_OK, Result.ResultCode);
	Assert.IsFalse(Result.RequiresOverwrite);
end;

initialization
	TDUnitX.RegisterTestFixture(TUploadPreparationValidatorTest);

end.

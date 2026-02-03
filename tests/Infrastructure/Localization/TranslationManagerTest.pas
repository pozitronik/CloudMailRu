unit TranslationManagerTest;

{Tests for TTranslationManager - .lng file loading, parsing, and application.
	Uses TMemoryFileSystem for full isolation from disk I/O.}

interface

uses
	TranslationManager,
	FileSystem,
	DUnitX.TestFramework;

type
	[TestFixture]
	TTranslationManagerTest = class
	private
		FFileSystem: TMemoryFileSystem;
		FFileSystemRef: IFileSystem;
		FManager: TTranslationManager;

		{Shorthand for setting .lng file content in memory}
		procedure SetLngFile(const Name, Content: WideString);
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{ Apply tests }
		[Test]
		procedure TestApply_ValidFile_ChangesStrings;
		[Test]
		procedure TestApply_PartialFile_OnlyChangesSpecifiedKeys;
		[Test]
		procedure TestApply_MalformedLine_ReturnsError;
		[Test]
		procedure TestApply_FileNotFound_ReturnsError;
		[Test]
		procedure TestApply_EmptyFile_DefaultsUnchanged;
		[Test]
		procedure TestApply_ResetsBeforeApplying;

		{ Reset tests }
		[Test]
		procedure TestReset_RestoresDefaults;

		{ GetAvailableTranslations tests }
		[Test]
		procedure TestGetAvailableTranslations_MultipleFiles;
		[Test]
		procedure TestGetAvailableTranslations_EmptyDir;

		{ ValidateFile tests }
		[Test]
		procedure TestValidateFile_Valid_ReturnsTrue;
		[Test]
		procedure TestValidateFile_MalformedLines_ReturnsFalse;
		[Test]
		procedure TestValidateFile_NotFound_ReturnsFalse;

		{ Compound strings tests }
		[Test]
		procedure TestCompoundStrings_TranslatedDirectly;
		[Test]
		procedure TestCompoundStrings_RestoredAfterReset;

		{ Parsing edge cases }
		[Test]
		procedure TestCaseInsensitiveKeys;
		[Test]
		procedure TestUnknownKeys_Ignored;
		[Test]
		procedure TestCommentLines_Skipped;
		[Test]
		procedure TestEmptyLines_Skipped;
		[Test]
		procedure TestQuotedValues_StrippedCorrectly;
		[Test]
		procedure TestValueWithEquals_ParsedCorrectly;
		[Test]
		procedure TestEscapeSequence_NewlineConverted;

		{ ReadTranslationName tests }
		[Test]
		procedure TestReadTranslationName_ReturnsValue;
		[Test]
		procedure TestReadTranslationName_MissingKey_ReturnsEmpty;
		[Test]
		procedure TestReadTranslationName_FileNotFound_ReturnsEmpty;

		{ TRANSLATION_NAME integration }
		[Test]
		procedure TestTranslationName_AppliedViaRegistry;
	end;

implementation

uses
	SysUtils,
	LanguageStrings;

const
	LANG_DIR = 'C:\plugin\language\';

{ TTranslationManagerTest }

procedure TTranslationManagerTest.Setup;
begin
	{Ensure clean state before each test}
	InitializeDefaults;

	FFileSystem := TMemoryFileSystem.Create;
	FFileSystemRef := FFileSystem;
	FManager := TTranslationManager.Create(FFileSystemRef, LANG_DIR);
end;

procedure TTranslationManagerTest.TearDown;
begin
	FManager.Free;
	FFileSystemRef := nil;

	{Restore defaults so other test fixtures are not affected}
	InitializeDefaults;
end;

procedure TTranslationManagerTest.SetLngFile(const Name, Content: WideString);
begin
	FFileSystem.SetFileContent(LANG_DIR + Name + '.lng', Content);
end;

{ Apply tests }

procedure TTranslationManagerTest.TestApply_ValidFile_ChangesStrings;
var
	ErrorMsg: WideString;
begin
	SetLngFile('russian', 'DONE="Готово"' + sLineBreak + 'OK="Ок"');

	Assert.IsTrue(FManager.Apply('russian', ErrorMsg));
	Assert.AreEqual('', String(ErrorMsg));
	Assert.AreEqual('Готово', String(DONE));
	Assert.AreEqual('Ок', String(OK));
end;

procedure TTranslationManagerTest.TestApply_PartialFile_OnlyChangesSpecifiedKeys;
var
	ErrorMsg: WideString;
	OriginalCancel: WideString;
begin
	OriginalCancel := CANCEL;
	SetLngFile('partial', 'DONE="Terminado"');

	Assert.IsTrue(FManager.Apply('partial', ErrorMsg));
	Assert.AreEqual('Terminado', String(DONE));
	Assert.AreEqual(String(OriginalCancel), String(CANCEL), 'Unspecified keys should keep defaults');
end;

procedure TTranslationManagerTest.TestApply_MalformedLine_ReturnsError;
var
	ErrorMsg: WideString;
begin
	SetLngFile('bad', 'DONE="Готово"' + sLineBreak + 'THIS IS INVALID');

	Assert.IsFalse(FManager.Apply('bad', ErrorMsg));
	Assert.Contains(ErrorMsg, 'Malformed line 2');
end;

procedure TTranslationManagerTest.TestApply_FileNotFound_ReturnsError;
var
	ErrorMsg: WideString;
begin
	Assert.IsFalse(FManager.Apply('nonexistent', ErrorMsg));
	Assert.Contains(ErrorMsg, 'not found');
end;

procedure TTranslationManagerTest.TestApply_EmptyFile_DefaultsUnchanged;
var
	ErrorMsg: WideString;
	OriginalDone: WideString;
begin
	OriginalDone := DONE;
	SetLngFile('empty', '');

	Assert.IsTrue(FManager.Apply('empty', ErrorMsg));
	Assert.AreEqual(String(OriginalDone), String(DONE), 'Empty file should leave defaults unchanged');
end;

procedure TTranslationManagerTest.TestApply_ResetsBeforeApplying;
var
	ErrorMsg: WideString;
begin
	{Apply first translation that changes DONE}
	SetLngFile('first', 'DONE="First"');
	FManager.Apply('first', ErrorMsg);
	Assert.AreEqual('First', String(DONE));

	{Apply second translation that does NOT include DONE --
		DONE should revert to default because Apply resets first}
	SetLngFile('second', 'OK="Second"');
	FManager.Apply('second', ErrorMsg);
	Assert.AreEqual('Done', String(DONE), 'DONE should be reset to default');
	Assert.AreEqual('Second', String(OK));
end;

{ Reset tests }

procedure TTranslationManagerTest.TestReset_RestoresDefaults;
var
	ErrorMsg: WideString;
	OriginalDone: WideString;
begin
	OriginalDone := DONE;
	SetLngFile('test', 'DONE="Changed"');
	FManager.Apply('test', ErrorMsg);
	Assert.AreEqual('Changed', String(DONE));

	FManager.Reset;
	Assert.AreEqual(String(OriginalDone), String(DONE));
end;

{ GetAvailableTranslations tests }

procedure TTranslationManagerTest.TestGetAvailableTranslations_MultipleFiles;
var
	Translations: TArray<WideString>;
begin
	SetLngFile('english', '# English');
	SetLngFile('russian', '# Russian');
	SetLngFile('german', '# German');

	Translations := FManager.GetAvailableTranslations;
	Assert.AreEqual(Integer(3), Integer(Length(Translations)));
end;

procedure TTranslationManagerTest.TestGetAvailableTranslations_EmptyDir;
var
	Translations: TArray<WideString>;
begin
	Translations := FManager.GetAvailableTranslations;
	Assert.AreEqual(Integer(0), Integer(Length(Translations)));
end;

{ ValidateFile tests }

procedure TTranslationManagerTest.TestValidateFile_Valid_ReturnsTrue;
var
	ErrorMsg: WideString;
begin
	SetLngFile('valid', '# Comment line' + sLineBreak +
		'DONE="Готово"' + sLineBreak +
		'OK="Ок"');

	Assert.IsTrue(FManager.ValidateFile('valid', ErrorMsg));
	Assert.AreEqual('', String(ErrorMsg));
end;

procedure TTranslationManagerTest.TestValidateFile_MalformedLines_ReturnsFalse;
var
	ErrorMsg: WideString;
begin
	SetLngFile('invalid', 'DONE="Ok"' + sLineBreak + 'BAD LINE WITHOUT EQUALS');

	Assert.IsFalse(FManager.ValidateFile('invalid', ErrorMsg));
	Assert.Contains(ErrorMsg, 'Malformed line 2');
end;

procedure TTranslationManagerTest.TestValidateFile_NotFound_ReturnsFalse;
var
	ErrorMsg: WideString;
begin
	Assert.IsFalse(FManager.ValidateFile('missing', ErrorMsg));
	Assert.Contains(ErrorMsg, 'not found');
end;

{ Compound strings tests }

procedure TTranslationManagerTest.TestCompoundStrings_TranslatedDirectly;
var
	ErrorMsg: WideString;
begin
	SetLngFile('compound', 'ERR_CLONE_FILE_ASK="Clone error: %s\nContinue?"');

	Assert.IsTrue(FManager.Apply('compound', ErrorMsg), 'Apply failed: ' + String(ErrorMsg));
	Assert.AreEqual('Clone error: %s' + sLineBreak + 'Continue?', String(ERR_CLONE_FILE_ASK));
end;

procedure TTranslationManagerTest.TestCompoundStrings_RestoredAfterReset;
var
	ErrorMsg: WideString;
	OriginalCompound: WideString;
begin
	OriginalCompound := ERR_CLONE_FILE_ASK;

	SetLngFile('compound', 'CONTINUE_ASK="Продолжить?"');
	FManager.Apply('compound', ErrorMsg);

	FManager.Reset;
	Assert.AreEqual(String(OriginalCompound), String(ERR_CLONE_FILE_ASK));
end;

{ Parsing edge cases }

procedure TTranslationManagerTest.TestCaseInsensitiveKeys;
var
	ErrorMsg: WideString;
begin
	SetLngFile('casetest', 'done="Lowercase key"');

	Assert.IsTrue(FManager.Apply('casetest', ErrorMsg));
	Assert.AreEqual('Lowercase key', String(DONE));
end;

procedure TTranslationManagerTest.TestUnknownKeys_Ignored;
var
	ErrorMsg: WideString;
begin
	SetLngFile('unknown', 'FUTURE_KEY="Some value"' + sLineBreak + 'DONE="Ok"');

	Assert.IsTrue(FManager.Apply('unknown', ErrorMsg));
	Assert.AreEqual('Ok', String(DONE));
end;

procedure TTranslationManagerTest.TestCommentLines_Skipped;
var
	ErrorMsg: WideString;
	OriginalDone: WideString;
begin
	OriginalDone := DONE;
	SetLngFile('comments', '# This is a comment' + sLineBreak +
		'# Another comment' + sLineBreak +
		'## Also a comment');

	Assert.IsTrue(FManager.Apply('comments', ErrorMsg));
	Assert.AreEqual(String(OriginalDone), String(DONE), 'Only comments should not change anything');
end;

procedure TTranslationManagerTest.TestEmptyLines_Skipped;
var
	ErrorMsg: WideString;
begin
	SetLngFile('blanks', '' + sLineBreak +
		'DONE="Changed"' + sLineBreak +
		'' + sLineBreak +
		'   ' + sLineBreak +
		'OK="Also changed"');

	Assert.IsTrue(FManager.Apply('blanks', ErrorMsg));
	Assert.AreEqual('Changed', String(DONE));
	Assert.AreEqual('Also changed', String(OK));
end;

procedure TTranslationManagerTest.TestQuotedValues_StrippedCorrectly;
var
	ErrorMsg: WideString;
begin
	SetLngFile('quotes', 'DONE="Value with ""inner"" quotes"');

	Assert.IsTrue(FManager.Apply('quotes', ErrorMsg));
	{Outer quotes stripped, inner content preserved as-is}
	Assert.AreEqual('Value with ""inner"" quotes', String(DONE));
end;

procedure TTranslationManagerTest.TestValueWithEquals_ParsedCorrectly;
var
	ErrorMsg: WideString;
begin
	{Value contains = sign, only first = is the delimiter}
	SetLngFile('equals', 'DONE="A=B=C"');

	Assert.IsTrue(FManager.Apply('equals', ErrorMsg));
	Assert.AreEqual('A=B=C', String(DONE));
end;

procedure TTranslationManagerTest.TestEscapeSequence_NewlineConverted;
var
	ErrorMsg: WideString;
begin
	SetLngFile('escapes', 'DONE="Line1\nLine2\nLine3"');

	Assert.IsTrue(FManager.Apply('escapes', ErrorMsg));
	Assert.AreEqual('Line1' + sLineBreak + 'Line2' + sLineBreak + 'Line3', String(DONE));
end;

{ ReadTranslationName tests }

procedure TTranslationManagerTest.TestReadTranslationName_ReturnsValue;
begin
	SetLngFile('named', 'TRANSLATION_NAME="Deutsch"' + sLineBreak + 'DONE="Fertig"');

	Assert.AreEqual('Deutsch', String(FManager.ReadTranslationName('named')));
end;

procedure TTranslationManagerTest.TestReadTranslationName_MissingKey_ReturnsEmpty;
begin
	SetLngFile('noname', 'DONE="Fertig"');

	Assert.AreEqual('', String(FManager.ReadTranslationName('noname')));
end;

procedure TTranslationManagerTest.TestReadTranslationName_FileNotFound_ReturnsEmpty;
begin
	Assert.AreEqual('', String(FManager.ReadTranslationName('nonexistent')));
end;

{ TRANSLATION_NAME integration }

procedure TTranslationManagerTest.TestTranslationName_AppliedViaRegistry;
var
	ErrorMsg: WideString;
begin
	SetLngFile('withname', 'TRANSLATION_NAME="Test Language"' + sLineBreak + 'DONE="Ok"');

	Assert.IsTrue(FManager.Apply('withname', ErrorMsg));
	Assert.AreEqual('Test Language', String(TRANSLATION_NAME));
	Assert.AreEqual('Ok', String(DONE));
end;

initialization

TDUnitX.RegisterTestFixture(TTranslationManagerTest);

end.

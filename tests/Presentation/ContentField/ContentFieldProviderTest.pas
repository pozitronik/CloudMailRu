unit ContentFieldProviderTest;

interface

uses
	Windows, SysUtils, AnsiStrings,
	CMRDirItem, CMRConstants, PLUGIN_TYPES,
	ContentFieldProvider,
	DUnitX.TestFramework;

type
	[TestFixture]
	TContentFieldProviderTest = class
	private
		FProvider: IContentFieldProvider;
		function CreateTestItem: TCMRDirItem;
		function CreateTestItemFolder: TCMRDirItem;
		function CreateTestItemShared: TCMRDirItem;
		function CreateTestItemTrash: TCMRDirItem;
		function CreateEmptyContext: TContentFieldContext;
	public
		[Setup]
		procedure Setup;

		[TearDown]
		procedure TearDown;

		{ GetSupportedField tests }
		[Test]
		procedure TestGetSupportedField_FirstField;
		[Test]
		procedure TestGetSupportedField_LastField;
		[Test]
		procedure TestGetSupportedField_AllFieldsHaveNames;
		[Test]
		procedure TestGetSupportedField_InvalidIndex_Negative;
		[Test]
		procedure TestGetSupportedField_InvalidIndex_TooLarge;
		[Test]
		procedure TestGetFieldCount;

		{ String field tests }
		[Test]
		procedure TestGetValue_NameField;
		[Test]
		procedure TestGetValue_KindField;
		[Test]
		procedure TestGetValue_WeblinkField;
		[Test]
		procedure TestGetValue_TypeField;
		[Test]
		procedure TestGetValue_HomeField;
		[Test]
		procedure TestGetValue_HashField;
		[Test]
		procedure TestGetValue_VirusScanField;

		{ Numeric field tests }
		[Test]
		procedure TestGetValue_SizeField;
		[Test]
		procedure TestGetValue_GrevField_SharedItem;
		[Test]
		procedure TestGetValue_RevField_SharedItem;
		[Test]
		procedure TestGetValue_FoldersCountField_Folder;
		[Test]
		procedure TestGetValue_FilesCountField_Folder;

		{ DateTime field tests }
		[Test]
		procedure TestGetValue_MtimeField;

		{ Conditional field tests - mtime=0 required }
		[Test]
		procedure TestGetValue_TreeField_SharedItem;
		[Test]
		procedure TestGetValue_TreeField_RegularItem_ReturnsNoSuchField;
		[Test]
		procedure TestGetValue_GrevField_RegularItem_ReturnsNoSuchField;
		[Test]
		procedure TestGetValue_RevField_RegularItem_ReturnsNoSuchField;

		{ Conditional field tests - mtime<>0 required }
		[Test]
		procedure TestGetValue_MtimeField_SharedItem_ReturnsNoSuchField;

		{ Conditional field tests - folder only }
		[Test]
		procedure TestGetValue_FoldersCountField_File_ReturnsNoSuchField;
		[Test]
		procedure TestGetValue_FilesCountField_File_ReturnsNoSuchField;

		{ Description field tests }
		[Test]
		procedure TestGetValue_DescriptionField_Enabled;
		[Test]
		procedure TestGetValue_DescriptionField_Disabled;
		[Test]
		procedure TestGetValue_DescriptionField_AccountRoot;

		{ Trash field tests }
		[Test]
		procedure TestGetValue_DeletedAtField_TrashItem;
		[Test]
		procedure TestGetValue_DeletedAtField_RegularItem_ReturnsNoSuchField;
		[Test]
		procedure TestGetValue_DeletedFromField_TrashItem;
		[Test]
		procedure TestGetValue_DeletedFromField_RegularItem_ReturnsNoSuchField;
		[Test]
		procedure TestGetValue_DeletedByField_TrashItem;
		[Test]
		procedure TestGetValue_DeletedByField_RegularItem_ReturnsNoSuchField;

		{ Account root tests }
		[Test]
		procedure TestGetValue_AccountRoot_OnlyDescriptionSupported;
		[Test]
		procedure TestGetValue_AccountRoot_OtherFields_ReturnsNoSuchField;

		{ Invalid index test }
		[Test]
		procedure TestGetValue_InvalidIndex_ReturnsNoSuchField;
	end;

implementation

procedure TContentFieldProviderTest.Setup;
begin
	FProvider := TContentFieldProvider.Create;
end;

procedure TContentFieldProviderTest.TearDown;
begin
	FProvider := nil;
end;

function TContentFieldProviderTest.CreateEmptyContext: TContentFieldContext;
begin
	Result.DescriptionsEnabled := False;
	Result.FileDescription := '';
	Result.AccountDescription := '';
	Result.IsAccountRoot := False;
end;

function TContentFieldProviderTest.CreateTestItem: TCMRDirItem;
begin
	Result.tree := '';
	Result.name := 'test_file.txt';
	Result.visible_name := 'test_file.txt';
	Result.grev := 0;
	Result.size := 12345;
	Result.kind := 'file';
	Result.weblink := 'https://cloud.mail.ru/public/abc123';
	Result.rev := 0;
	Result.type_ := TYPE_FILE;
	Result.home := '/test_file.txt';
	Result.mtime := 1704067200; { 2024-01-01 00:00:00 UTC }
	Result.hash := 'ABCDEF1234567890';
	Result.virus_scan := 'pass';
	Result.folders_count := 0;
	Result.files_count := 0;
	Result.deleted_at := 0;
	Result.deleted_from := '';
	Result.deleted_by := 0;
end;

function TContentFieldProviderTest.CreateTestItemFolder: TCMRDirItem;
begin
	Result := CreateTestItem;
	Result.name := 'test_folder';
	Result.type_ := TYPE_DIR;
	Result.kind := 'folder';
	Result.size := 0;
	Result.hash := '';
	Result.folders_count := 5;
	Result.files_count := 10;
end;

function TContentFieldProviderTest.CreateTestItemShared: TCMRDirItem;
begin
	Result := CreateTestItem;
	Result.mtime := 0; { Shared items have mtime=0 }
	Result.tree := '/shared_tree';
	Result.grev := 42;
	Result.rev := 100;
end;

function TContentFieldProviderTest.CreateTestItemTrash: TCMRDirItem;
begin
	Result := CreateTestItem;
	Result.deleted_at := 1704153600; { 2024-01-02 00:00:00 UTC }
	Result.deleted_from := '/original/path/file.txt';
	Result.deleted_by := 123456;
end;

{ GetSupportedField tests }

procedure TContentFieldProviderTest.TestGetSupportedField_FirstField;
var
	FieldName: array[0..255] of AnsiChar;
	FieldType: Integer;
begin
	FieldType := FProvider.GetSupportedField(0, @FieldName[0], 256);

	Assert.AreEqual('tree', string(FieldName));
	Assert.AreEqual(ft_stringw, FieldType);
end;

procedure TContentFieldProviderTest.TestGetSupportedField_LastField;
var
	FieldName: array[0..255] of AnsiChar;
	FieldType: Integer;
begin
	FieldType := FProvider.GetSupportedField(17, @FieldName[0], 256);

	Assert.AreEqual('deleted_by', string(FieldName));
	Assert.AreEqual(ft_stringw, FieldType);
end;

procedure TContentFieldProviderTest.TestGetSupportedField_AllFieldsHaveNames;
var
	FieldName: array[0..255] of AnsiChar;
	I: Integer;
begin
	for I := 0 to FProvider.GetFieldCount - 1 do
	begin
		FillChar(FieldName, SizeOf(FieldName), 0);
		FProvider.GetSupportedField(I, @FieldName[0], 256);
		Assert.IsTrue(System.AnsiStrings.StrLen(FieldName) > 0, Format('Field %d has empty name', [I]));
	end;
end;

procedure TContentFieldProviderTest.TestGetSupportedField_InvalidIndex_Negative;
var
	FieldName: array[0..255] of AnsiChar;
begin
	Assert.AreEqual(ft_nomorefields, FProvider.GetSupportedField(-1, @FieldName[0], 256));
end;

procedure TContentFieldProviderTest.TestGetSupportedField_InvalidIndex_TooLarge;
var
	FieldName: array[0..255] of AnsiChar;
begin
	Assert.AreEqual(ft_nomorefields, FProvider.GetSupportedField(100, @FieldName[0], 256));
end;

procedure TContentFieldProviderTest.TestGetFieldCount;
begin
	Assert.AreEqual(18, FProvider.GetFieldCount);
end;

{ String field tests }

procedure TContentFieldProviderTest.TestGetValue_NameField;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: array[0..255] of WideChar;
	FieldType: Integer;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;

	FieldType := FProvider.GetValue(1, Item, @Value[0], Context);

	Assert.AreEqual(ft_stringw, FieldType);
	Assert.AreEqual('test_file.txt', string(Value));
end;

procedure TContentFieldProviderTest.TestGetValue_KindField;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: array[0..255] of WideChar;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;

	FProvider.GetValue(4, Item, @Value[0], Context);

	Assert.AreEqual('file', string(Value));
end;

procedure TContentFieldProviderTest.TestGetValue_WeblinkField;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: array[0..255] of WideChar;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;

	FProvider.GetValue(5, Item, @Value[0], Context);

	Assert.AreEqual('https://cloud.mail.ru/public/abc123', string(Value));
end;

procedure TContentFieldProviderTest.TestGetValue_TypeField;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: array[0..255] of WideChar;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;

	FProvider.GetValue(7, Item, @Value[0], Context);

	Assert.AreEqual(TYPE_FILE, string(Value));
end;

procedure TContentFieldProviderTest.TestGetValue_HomeField;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: array[0..255] of WideChar;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;

	FProvider.GetValue(8, Item, @Value[0], Context);

	Assert.AreEqual('/test_file.txt', string(Value));
end;

procedure TContentFieldProviderTest.TestGetValue_HashField;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: array[0..255] of WideChar;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;

	FProvider.GetValue(10, Item, @Value[0], Context);

	Assert.AreEqual('ABCDEF1234567890', string(Value));
end;

procedure TContentFieldProviderTest.TestGetValue_VirusScanField;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: array[0..255] of WideChar;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;

	FProvider.GetValue(11, Item, @Value[0], Context);

	Assert.AreEqual('pass', string(Value));
end;

{ Numeric field tests }

procedure TContentFieldProviderTest.TestGetValue_SizeField;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: Int64;
	FieldType: Integer;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;

	FieldType := FProvider.GetValue(3, Item, @Value, Context);

	Assert.AreEqual(ft_numeric_64, FieldType);
	Assert.AreEqual(Int64(12345), Value);
end;

procedure TContentFieldProviderTest.TestGetValue_GrevField_SharedItem;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: Integer;
	FieldType: Integer;
begin
	Item := CreateTestItemShared;
	Context := CreateEmptyContext;

	FieldType := FProvider.GetValue(2, Item, @Value, Context);

	Assert.AreEqual(ft_numeric_32, FieldType);
	Assert.AreEqual(42, Value);
end;

procedure TContentFieldProviderTest.TestGetValue_RevField_SharedItem;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: Integer;
	FieldType: Integer;
begin
	Item := CreateTestItemShared;
	Context := CreateEmptyContext;

	FieldType := FProvider.GetValue(6, Item, @Value, Context);

	Assert.AreEqual(ft_numeric_32, FieldType);
	Assert.AreEqual(100, Value);
end;

procedure TContentFieldProviderTest.TestGetValue_FoldersCountField_Folder;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: Integer;
	FieldType: Integer;
begin
	Item := CreateTestItemFolder;
	Context := CreateEmptyContext;

	FieldType := FProvider.GetValue(12, Item, @Value, Context);

	Assert.AreEqual(ft_numeric_32, FieldType);
	Assert.AreEqual(5, Value);
end;

procedure TContentFieldProviderTest.TestGetValue_FilesCountField_Folder;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: Integer;
	FieldType: Integer;
begin
	Item := CreateTestItemFolder;
	Context := CreateEmptyContext;

	FieldType := FProvider.GetValue(13, Item, @Value, Context);

	Assert.AreEqual(ft_numeric_32, FieldType);
	Assert.AreEqual(10, Value);
end;

{ DateTime field tests }

procedure TContentFieldProviderTest.TestGetValue_MtimeField;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: TFileTime;
	FieldType: Integer;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;

	FieldType := FProvider.GetValue(9, Item, @Value, Context);

	Assert.AreEqual(ft_datetime, FieldType);
	Assert.IsTrue((Value.dwHighDateTime <> 0) or (Value.dwLowDateTime <> 0));
end;

{ Conditional field tests - mtime=0 required }

procedure TContentFieldProviderTest.TestGetValue_TreeField_SharedItem;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: array[0..255] of WideChar;
	FieldType: Integer;
begin
	Item := CreateTestItemShared;
	Context := CreateEmptyContext;

	FieldType := FProvider.GetValue(0, Item, @Value[0], Context);

	Assert.AreEqual(ft_stringw, FieldType);
	Assert.AreEqual('/shared_tree', string(Value));
end;

procedure TContentFieldProviderTest.TestGetValue_TreeField_RegularItem_ReturnsNoSuchField;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: array[0..255] of WideChar;
begin
	Item := CreateTestItem; { Has mtime <> 0 }
	Context := CreateEmptyContext;

	Assert.AreEqual(ft_nosuchfield, FProvider.GetValue(0, Item, @Value[0], Context));
end;

procedure TContentFieldProviderTest.TestGetValue_GrevField_RegularItem_ReturnsNoSuchField;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: Integer;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;

	Assert.AreEqual(ft_nosuchfield, FProvider.GetValue(2, Item, @Value, Context));
end;

procedure TContentFieldProviderTest.TestGetValue_RevField_RegularItem_ReturnsNoSuchField;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: Integer;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;

	Assert.AreEqual(ft_nosuchfield, FProvider.GetValue(6, Item, @Value, Context));
end;

{ Conditional field tests - mtime<>0 required }

procedure TContentFieldProviderTest.TestGetValue_MtimeField_SharedItem_ReturnsNoSuchField;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: TFileTime;
begin
	Item := CreateTestItemShared; { Has mtime = 0 }
	Context := CreateEmptyContext;

	Assert.AreEqual(ft_nosuchfield, FProvider.GetValue(9, Item, @Value, Context));
end;

{ Conditional field tests - folder only }

procedure TContentFieldProviderTest.TestGetValue_FoldersCountField_File_ReturnsNoSuchField;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: Integer;
begin
	Item := CreateTestItem; { type_ = TYPE_FILE }
	Context := CreateEmptyContext;

	Assert.AreEqual(ft_nosuchfield, FProvider.GetValue(12, Item, @Value, Context));
end;

procedure TContentFieldProviderTest.TestGetValue_FilesCountField_File_ReturnsNoSuchField;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: Integer;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;

	Assert.AreEqual(ft_nosuchfield, FProvider.GetValue(13, Item, @Value, Context));
end;

{ Description field tests }

procedure TContentFieldProviderTest.TestGetValue_DescriptionField_Enabled;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: array[0..255] of WideChar;
	FieldType: Integer;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;
	Context.DescriptionsEnabled := True;
	Context.FileDescription := 'This is a test file description';

	FieldType := FProvider.GetValue(14, Item, @Value[0], Context);

	Assert.AreEqual(ft_stringw, FieldType);
	Assert.AreEqual('This is a test file description', string(Value));
end;

procedure TContentFieldProviderTest.TestGetValue_DescriptionField_Disabled;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: array[0..255] of WideChar;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;
	Context.DescriptionsEnabled := False;

	FProvider.GetValue(14, Item, @Value[0], Context);

	Assert.AreEqual('<disabled>', string(Value));
end;

procedure TContentFieldProviderTest.TestGetValue_DescriptionField_AccountRoot;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: array[0..255] of WideChar;
	FieldType: Integer;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;
	Context.IsAccountRoot := True;
	Context.AccountDescription := 'My account description';

	FieldType := FProvider.GetValue(14, Item, @Value[0], Context);

	Assert.AreEqual(ft_stringw, FieldType);
	Assert.AreEqual('My account description', string(Value));
end;

{ Trash field tests }

procedure TContentFieldProviderTest.TestGetValue_DeletedAtField_TrashItem;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: TFileTime;
	FieldType: Integer;
begin
	Item := CreateTestItemTrash;
	Context := CreateEmptyContext;

	FieldType := FProvider.GetValue(15, Item, @Value, Context);

	Assert.AreEqual(ft_datetime, FieldType);
	Assert.IsTrue((Value.dwHighDateTime <> 0) or (Value.dwLowDateTime <> 0));
end;

procedure TContentFieldProviderTest.TestGetValue_DeletedAtField_RegularItem_ReturnsNoSuchField;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: TFileTime;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;

	Assert.AreEqual(ft_nosuchfield, FProvider.GetValue(15, Item, @Value, Context));
end;

procedure TContentFieldProviderTest.TestGetValue_DeletedFromField_TrashItem;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: array[0..255] of WideChar;
	FieldType: Integer;
begin
	Item := CreateTestItemTrash;
	Context := CreateEmptyContext;

	FieldType := FProvider.GetValue(16, Item, @Value[0], Context);

	Assert.AreEqual(ft_stringw, FieldType);
	Assert.AreEqual('/original/path/file.txt', string(Value));
end;

procedure TContentFieldProviderTest.TestGetValue_DeletedFromField_RegularItem_ReturnsNoSuchField;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: array[0..255] of WideChar;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;

	Assert.AreEqual(ft_nosuchfield, FProvider.GetValue(16, Item, @Value[0], Context));
end;

procedure TContentFieldProviderTest.TestGetValue_DeletedByField_TrashItem;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: array[0..255] of WideChar;
	FieldType: Integer;
begin
	Item := CreateTestItemTrash;
	Context := CreateEmptyContext;

	FieldType := FProvider.GetValue(17, Item, @Value[0], Context);

	Assert.AreEqual(ft_stringw, FieldType);
	Assert.AreEqual('123456', string(Value));
end;

procedure TContentFieldProviderTest.TestGetValue_DeletedByField_RegularItem_ReturnsNoSuchField;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: array[0..255] of WideChar;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;

	Assert.AreEqual(ft_nosuchfield, FProvider.GetValue(17, Item, @Value[0], Context));
end;

{ Account root tests }

procedure TContentFieldProviderTest.TestGetValue_AccountRoot_OnlyDescriptionSupported;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: array[0..255] of WideChar;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;
	Context.IsAccountRoot := True;
	Context.AccountDescription := 'Account desc';

	Assert.AreEqual(ft_stringw, FProvider.GetValue(14, Item, @Value[0], Context));
end;

procedure TContentFieldProviderTest.TestGetValue_AccountRoot_OtherFields_ReturnsNoSuchField;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: array[0..255] of WideChar;
	I: Integer;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;
	Context.IsAccountRoot := True;

	{ All fields except 14 (description) should return ft_nosuchfield for account root }
	for I := 0 to FProvider.GetFieldCount - 1 do
	begin
		if I <> 14 then
			Assert.AreEqual(ft_nosuchfield, FProvider.GetValue(I, Item, @Value[0], Context),
				Format('Field %d should return ft_nosuchfield for account root', [I]));
	end;
end;

{ Invalid index test }

procedure TContentFieldProviderTest.TestGetValue_InvalidIndex_ReturnsNoSuchField;
var
	Item: TCMRDirItem;
	Context: TContentFieldContext;
	Value: array[0..255] of WideChar;
begin
	Item := CreateTestItem;
	Context := CreateEmptyContext;

	Assert.AreEqual(ft_nosuchfield, FProvider.GetValue(100, Item, @Value[0], Context));
	Assert.AreEqual(ft_nosuchfield, FProvider.GetValue(-1, Item, @Value[0], Context));
end;

initialization
	TDUnitX.RegisterTestFixture(TContentFieldProviderTest);

end.

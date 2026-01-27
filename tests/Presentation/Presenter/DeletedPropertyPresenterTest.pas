unit DeletedPropertyPresenterTest;

interface

uses
	DUnitX.TestFramework,
	DeletedPropertyPresenter,
	CloudDirItemList,
	CloudDirItem,
	CloudConstants,
	SettingsConstants,
	LanguageStrings,
	StringHelper,
	DateUtils,
	System.SysUtils;

type
	{Mock view for testing DeletedPropertyPresenter}
	TMockDeletedPropertyView = class(TInterfacedObject, IDeletedPropertyView)
	public
		Caption: WideString;
		Name: WideString;
		DeletedFrom: WideString;
		DeletedAt: WideString;
		DeletedBy: WideString;
		Size: WideString;
		RestoreEnabled: Boolean;
		RestoreAllEnabled: Boolean;
		EmptyEnabled: Boolean;

		{IDeletedPropertyView implementation}
		procedure SetCaption(ACaption: WideString);
		procedure SetItemName(AName: WideString);
		procedure SetDeletedFrom(APath: WideString);
		procedure SetDeletedAt(ADateTime: WideString);
		procedure SetDeletedBy(AUserId: WideString);
		procedure SetSize(ASize: WideString);
		procedure SetRestoreEnabled(AEnabled: Boolean);
		procedure SetRestoreAllEnabled(AEnabled: Boolean);
		procedure SetEmptyEnabled(AEnabled: Boolean);
	end;

	[TestFixture]
	TDeletedPropertyPresenterTest = class
	private
		FMockView: TMockDeletedPropertyView;
		FPresenter: TDeletedPropertyPresenter;

		function CreateTestItem(AName: WideString; ASize: Int64; ADeletedFrom: WideString = '/original/path'): TCloudDirItem;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure Initialize_EmptyItems_SetsNameToEmpty;

		[Test]
		procedure Initialize_EmptyItems_SetsCaptionWithAccountName;

		[Test]
		procedure Initialize_EmptyItems_DisablesAllButtons;

		[Test]
		procedure Initialize_SingleItem_SetsNameFromItem;

		[Test]
		procedure Initialize_SingleItem_SetsDeletedFromPath;

		[Test]
		procedure Initialize_SingleItem_SetsFormattedSize;

		[Test]
		procedure Initialize_SingleItem_SetsCaptionWithItemName;

		[Test]
		procedure Initialize_SingleItem_DisablesRestoreAll;

		[Test]
		procedure Initialize_MultipleItems_SetsNameToMultiple;

		[Test]
		procedure Initialize_MultipleItems_CalculatesSummarySize;

		[Test]
		procedure Initialize_MultipleItems_SetsUnsetValues;

		[Test]
		procedure Initialize_TrashDir_SetsCaptionWithTrashPostfix;

		[Test]
		procedure Initialize_TrashDir_DisablesRestore;

		[Test]
		procedure Initialize_TrashDir_EnablesRestoreAll;

		[Test]
		procedure Items_Property_ReturnsInitializedItems;

		[Test]
		procedure IsTrashDir_Property_ReturnsInitializedValue;

		[Test]
		procedure AccountName_Property_ReturnsInitializedValue;
	end;

implementation

{TMockDeletedPropertyView}

procedure TMockDeletedPropertyView.SetCaption(ACaption: WideString);
begin
	Caption := ACaption;
end;

procedure TMockDeletedPropertyView.SetItemName(AName: WideString);
begin
	Name := AName;
end;

procedure TMockDeletedPropertyView.SetDeletedFrom(APath: WideString);
begin
	DeletedFrom := APath;
end;

procedure TMockDeletedPropertyView.SetDeletedAt(ADateTime: WideString);
begin
	DeletedAt := ADateTime;
end;

procedure TMockDeletedPropertyView.SetDeletedBy(AUserId: WideString);
begin
	DeletedBy := AUserId;
end;

procedure TMockDeletedPropertyView.SetSize(ASize: WideString);
begin
	Size := ASize;
end;

procedure TMockDeletedPropertyView.SetRestoreEnabled(AEnabled: Boolean);
begin
	RestoreEnabled := AEnabled;
end;

procedure TMockDeletedPropertyView.SetRestoreAllEnabled(AEnabled: Boolean);
begin
	RestoreAllEnabled := AEnabled;
end;

procedure TMockDeletedPropertyView.SetEmptyEnabled(AEnabled: Boolean);
begin
	EmptyEnabled := AEnabled;
end;

{TDeletedPropertyPresenterTest}

function TDeletedPropertyPresenterTest.CreateTestItem(AName: WideString; ASize: Int64; ADeletedFrom: WideString): TCloudDirItem;
begin
	FillChar(Result, SizeOf(Result), 0);
	Result.name := AName;
	Result.size := ASize;
	Result.deleted_from := ADeletedFrom;
	Result.deleted_at := DateTimeToUnix(Now);
	Result.deleted_by := 12345;
end;

procedure TDeletedPropertyPresenterTest.Setup;
begin
	FMockView := TMockDeletedPropertyView.Create;
	FPresenter := TDeletedPropertyPresenter.Create(FMockView);
end;

procedure TDeletedPropertyPresenterTest.TearDown;
begin
	FPresenter.Free;
	{FMockView is reference counted}
end;

procedure TDeletedPropertyPresenterTest.Initialize_EmptyItems_SetsNameToEmpty;
var
	Items: TCloudDirItemList;
begin
	SetLength(Items, 0);

	FPresenter.Initialize(Items, False, 'TestAccount');

	Assert.AreEqual(EMPTY, FMockView.Name);
end;

procedure TDeletedPropertyPresenterTest.Initialize_EmptyItems_SetsCaptionWithAccountName;
var
	Items: TCloudDirItemList;
begin
	SetLength(Items, 0);

	FPresenter.Initialize(Items, False, 'TestAccount');

	Assert.AreEqual(Format(ACCOUNT_TRASH, ['TestAccount']), FMockView.Caption);
end;

procedure TDeletedPropertyPresenterTest.Initialize_EmptyItems_DisablesAllButtons;
var
	Items: TCloudDirItemList;
begin
	SetLength(Items, 0);

	FPresenter.Initialize(Items, False, 'TestAccount');

	Assert.IsFalse(FMockView.RestoreEnabled);
	Assert.IsFalse(FMockView.RestoreAllEnabled);
	Assert.IsFalse(FMockView.EmptyEnabled);
end;

procedure TDeletedPropertyPresenterTest.Initialize_SingleItem_SetsNameFromItem;
var
	Items: TCloudDirItemList;
begin
	SetLength(Items, 1);
	Items[0] := CreateTestItem('DeletedFile.txt', 1024);

	FPresenter.Initialize(Items, False, 'TestAccount');

	Assert.AreEqual('DeletedFile.txt', FMockView.Name);
end;

procedure TDeletedPropertyPresenterTest.Initialize_SingleItem_SetsDeletedFromPath;
var
	Items: TCloudDirItemList;
begin
	SetLength(Items, 1);
	Items[0] := CreateTestItem('DeletedFile.txt', 1024, '/original/folder');

	FPresenter.Initialize(Items, False, 'TestAccount');

	Assert.AreEqual('/original/folder', FMockView.DeletedFrom);
end;

procedure TDeletedPropertyPresenterTest.Initialize_SingleItem_SetsFormattedSize;
var
	Items: TCloudDirItemList;
begin
	SetLength(Items, 1);
	Items[0] := CreateTestItem('DeletedFile.txt', 1024 * 1024);

	FPresenter.Initialize(Items, False, 'TestAccount');

	Assert.AreEqual(FormatSize(1024 * 1024, TYPE_BYTES), FMockView.Size);
end;

procedure TDeletedPropertyPresenterTest.Initialize_SingleItem_SetsCaptionWithItemName;
var
	Items: TCloudDirItemList;
begin
	SetLength(Items, 1);
	Items[0] := CreateTestItem('DeletedFile.txt', 1024);

	FPresenter.Initialize(Items, False, 'TestAccount');

	Assert.AreEqual(Format(DELETED_ITEM, ['DeletedFile.txt']), FMockView.Caption);
end;

procedure TDeletedPropertyPresenterTest.Initialize_SingleItem_DisablesRestoreAll;
var
	Items: TCloudDirItemList;
begin
	SetLength(Items, 1);
	Items[0] := CreateTestItem('DeletedFile.txt', 1024);

	FPresenter.Initialize(Items, False, 'TestAccount');

	Assert.IsFalse(FMockView.RestoreAllEnabled);
end;

procedure TDeletedPropertyPresenterTest.Initialize_MultipleItems_SetsNameToMultiple;
var
	Items: TCloudDirItemList;
begin
	SetLength(Items, 2);
	Items[0] := CreateTestItem('File1.txt', 1024);
	Items[1] := CreateTestItem('File2.txt', 2048);

	FPresenter.Initialize(Items, False, 'TestAccount');

	Assert.AreEqual(MULTIPLE_ITEMS, FMockView.Name);
end;

procedure TDeletedPropertyPresenterTest.Initialize_MultipleItems_CalculatesSummarySize;
var
	Items: TCloudDirItemList;
begin
	SetLength(Items, 2);
	Items[0] := CreateTestItem('File1.txt', 1024);
	Items[1] := CreateTestItem('File2.txt', 2048);

	FPresenter.Initialize(Items, False, 'TestAccount');

	Assert.AreEqual(FormatSize(3072, TYPE_BYTES), FMockView.Size);
end;

procedure TDeletedPropertyPresenterTest.Initialize_MultipleItems_SetsUnsetValues;
var
	Items: TCloudDirItemList;
begin
	SetLength(Items, 2);
	Items[0] := CreateTestItem('File1.txt', 1024);
	Items[1] := CreateTestItem('File2.txt', 2048);

	FPresenter.Initialize(Items, False, 'TestAccount');

	Assert.AreEqual(UNSET_ITEM, FMockView.DeletedFrom);
	Assert.AreEqual(UNSET_ITEM, FMockView.DeletedAt);
	Assert.AreEqual(UNSET_ITEM, FMockView.DeletedBy);
end;

procedure TDeletedPropertyPresenterTest.Initialize_TrashDir_SetsCaptionWithTrashPostfix;
var
	Items: TCloudDirItemList;
begin
	SetLength(Items, 1);
	Items[0] := CreateTestItem('File.txt', 1024);

	FPresenter.Initialize(Items, True, 'TestAccount');

	Assert.AreEqual('TestAccount' + TrashPostfix, FMockView.Caption);
end;

procedure TDeletedPropertyPresenterTest.Initialize_TrashDir_DisablesRestore;
var
	Items: TCloudDirItemList;
begin
	SetLength(Items, 1);
	Items[0] := CreateTestItem('File.txt', 1024);

	FPresenter.Initialize(Items, True, 'TestAccount');

	Assert.IsFalse(FMockView.RestoreEnabled);
end;

procedure TDeletedPropertyPresenterTest.Initialize_TrashDir_EnablesRestoreAll;
var
	Items: TCloudDirItemList;
begin
	SetLength(Items, 1);
	Items[0] := CreateTestItem('File.txt', 1024);

	FPresenter.Initialize(Items, True, 'TestAccount');

	Assert.IsTrue(FMockView.RestoreAllEnabled);
end;

procedure TDeletedPropertyPresenterTest.Items_Property_ReturnsInitializedItems;
var
	Items: TCloudDirItemList;
begin
	SetLength(Items, 1);
	Items[0] := CreateTestItem('File.txt', 1024);

	FPresenter.Initialize(Items, False, 'TestAccount');

	Assert.AreEqual(Integer(1), Integer(Length(FPresenter.Items)));
	Assert.AreEqual('File.txt', FPresenter.Items[0].name);
end;

procedure TDeletedPropertyPresenterTest.IsTrashDir_Property_ReturnsInitializedValue;
var
	Items: TCloudDirItemList;
begin
	SetLength(Items, 1);
	Items[0] := CreateTestItem('File.txt', 1024);

	FPresenter.Initialize(Items, True, 'TestAccount');

	Assert.IsTrue(FPresenter.IsTrashDir);
end;

procedure TDeletedPropertyPresenterTest.AccountName_Property_ReturnsInitializedValue;
var
	Items: TCloudDirItemList;
begin
	SetLength(Items, 0);

	FPresenter.Initialize(Items, False, 'MyAccount');

	Assert.AreEqual('MyAccount', FPresenter.AccountName);
end;

initialization
	TDUnitX.RegisterTestFixture(TDeletedPropertyPresenterTest);

end.

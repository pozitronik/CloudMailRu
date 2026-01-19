unit SharedItemActionHandlerTest;

{Unit tests for TSharedItemActionHandler.
 Tests symlink resolution and action routing for shared folder items.}

interface

uses
	DUnitX.TestFramework,
	SharedItemActionHandler,
	CMRDirItem,
	CMRDirItemList,
	RealPath;

type
	[TestFixture]
	TSharedItemActionHandlerTest = class
	private
		FHandler: ISharedItemActionHandler;

		function CreateFileItem(const Home, Name: WideString): TCMRDirItem;
		function CreateFolderItem(const Home, Name: WideString): TCMRDirItem;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Open action tests - symlink resolution}
		[Test]
		procedure TestHandleAction_OpenFile_ReturnsSymlinkToParentFolder;
		[Test]
		procedure TestHandleAction_OpenFolder_ReturnsSymlinkToFolder;
		[Test]
		procedure TestHandleAction_OpenItemNotFound_ReturnsNone;

		{Properties action tests}
		[Test]
		procedure TestHandleAction_PropertiesOnRoot_ReturnsAccountSettings;
		[Test]
		procedure TestHandleAction_PropertiesOnItem_ReturnsPropertyDialog;
		[Test]
		procedure TestHandleAction_PropertiesItemNotFound_ReturnsNone;

		{Factory method tests}
		[Test]
		procedure TestSharedItemActionResult_Symlink_SetsCorrectValues;
		[Test]
		procedure TestSharedItemActionResult_AccountSettings_SetsCorrectType;
		[Test]
		procedure TestSharedItemActionResult_PropertyDialog_SetsCorrectValues;
		[Test]
		procedure TestSharedItemActionResult_None_SetsCorrectType;

		{Edge cases}
		[Test]
		procedure TestHandleAction_OpenEmptyListing_ReturnsNone;
		[Test]
		procedure TestHandleAction_PropertiesEmptyListing_ReturnsNone;
	end;

implementation

uses
	SysUtils,
	CMRConstants;

procedure TSharedItemActionHandlerTest.Setup;
begin
	FHandler := TSharedItemActionHandler.Create;
end;

procedure TSharedItemActionHandlerTest.TearDown;
begin
	FHandler := nil;
end;

function TSharedItemActionHandlerTest.CreateFileItem(const Home, Name: WideString): TCMRDirItem;
begin
	Result := Result.None;
	Result.home := Home;
	Result.name := Name;
	Result.type_ := TYPE_FILE;
end;

function TSharedItemActionHandlerTest.CreateFolderItem(const Home, Name: WideString): TCMRDirItem;
begin
	Result := Result.None;
	Result.home := Home;
	Result.name := Name;
	Result.type_ := TYPE_DIR;
end;

{Open action tests}

procedure TSharedItemActionHandlerTest.TestHandleAction_OpenFile_ReturnsSymlinkToParentFolder;
var
	RealPath: TRealPath;
	Listing: TCMRDirItemList;
	ActionResult: TSharedItemActionResult;
begin
	{Setup: shared file at /documents/report.pdf}
	RealPath.FromPath('\account.shared\documents\report.pdf');
	SetLength(Listing, 1);
	Listing[0] := CreateFileItem('/documents/report.pdf', 'report.pdf');

	ActionResult := FHandler.HandleAction(RealPath, True, Listing);

	{For files, symlink should go to parent folder}
	Assert.AreEqual(satSymlink, ActionResult.ActionType);
	Assert.AreEqual('\account\documents\', ActionResult.SymlinkPath);
end;

procedure TSharedItemActionHandlerTest.TestHandleAction_OpenFolder_ReturnsSymlinkToFolder;
var
	RealPath: TRealPath;
	Listing: TCMRDirItemList;
	ActionResult: TSharedItemActionResult;
begin
	{Setup: shared folder at /projects/work}
	RealPath.FromPath('\account.shared\projects\work');
	SetLength(Listing, 1);
	Listing[0] := CreateFolderItem('/projects/work', 'work');

	ActionResult := FHandler.HandleAction(RealPath, True, Listing);

	{For folders, symlink should go to the folder itself}
	Assert.AreEqual(satSymlink, ActionResult.ActionType);
	Assert.AreEqual('\account\projects\work', ActionResult.SymlinkPath);
end;

procedure TSharedItemActionHandlerTest.TestHandleAction_OpenItemNotFound_ReturnsNone;
var
	RealPath: TRealPath;
	Listing: TCMRDirItemList;
	ActionResult: TSharedItemActionResult;
begin
	RealPath.FromPath('\account.shared\nonexistent');
	SetLength(Listing, 1);
	Listing[0] := CreateFileItem('/other/file.txt', 'file.txt');

	ActionResult := FHandler.HandleAction(RealPath, True, Listing);

	Assert.AreEqual(satNone, ActionResult.ActionType);
end;

{Properties action tests}

procedure TSharedItemActionHandlerTest.TestHandleAction_PropertiesOnRoot_ReturnsAccountSettings;
var
	RealPath: TRealPath;
	Listing: TCMRDirItemList;
	ActionResult: TSharedItemActionResult;
begin
	{Root of shared folder - isInAccountsList should be true}
	RealPath.FromPath('\account.shared');
	SetLength(Listing, 0);

	ActionResult := FHandler.HandleAction(RealPath, False, Listing);

	Assert.AreEqual(satAccountSettings, ActionResult.ActionType);
end;

procedure TSharedItemActionHandlerTest.TestHandleAction_PropertiesOnItem_ReturnsPropertyDialog;
var
	RealPath: TRealPath;
	Listing: TCMRDirItemList;
	ActionResult: TSharedItemActionResult;
begin
	RealPath.FromPath('\account.shared\documents\report.pdf');
	SetLength(Listing, 1);
	Listing[0] := CreateFileItem('/documents/report.pdf', 'report.pdf');

	ActionResult := FHandler.HandleAction(RealPath, False, Listing);

	Assert.AreEqual(satPropertyDialog, ActionResult.ActionType);
	Assert.AreEqual('/documents/report.pdf', ActionResult.CurrentItem.home);
end;

procedure TSharedItemActionHandlerTest.TestHandleAction_PropertiesItemNotFound_ReturnsNone;
var
	RealPath: TRealPath;
	Listing: TCMRDirItemList;
	ActionResult: TSharedItemActionResult;
begin
	RealPath.FromPath('\account.shared\nonexistent');
	SetLength(Listing, 1);
	Listing[0] := CreateFileItem('/other/file.txt', 'file.txt');

	ActionResult := FHandler.HandleAction(RealPath, False, Listing);

	Assert.AreEqual(satNone, ActionResult.ActionType);
end;

{Factory method tests}

procedure TSharedItemActionHandlerTest.TestSharedItemActionResult_Symlink_SetsCorrectValues;
var
	ActionResult: TSharedItemActionResult;
begin
	ActionResult := TSharedItemActionResult.Symlink('\account\folder\file.txt');

	Assert.AreEqual(satSymlink, ActionResult.ActionType);
	Assert.AreEqual('\account\folder\file.txt', ActionResult.SymlinkPath);
end;

procedure TSharedItemActionHandlerTest.TestSharedItemActionResult_AccountSettings_SetsCorrectType;
var
	ActionResult: TSharedItemActionResult;
begin
	ActionResult := TSharedItemActionResult.AccountSettings;

	Assert.AreEqual(satAccountSettings, ActionResult.ActionType);
end;

procedure TSharedItemActionHandlerTest.TestSharedItemActionResult_PropertyDialog_SetsCorrectValues;
var
	ActionResult: TSharedItemActionResult;
	Item: TCMRDirItem;
begin
	Item := CreateFileItem('/test/file.txt', 'file.txt');

	ActionResult := TSharedItemActionResult.PropertyDialog(Item);

	Assert.AreEqual(satPropertyDialog, ActionResult.ActionType);
	Assert.AreEqual('/test/file.txt', ActionResult.CurrentItem.home);
end;

procedure TSharedItemActionHandlerTest.TestSharedItemActionResult_None_SetsCorrectType;
var
	ActionResult: TSharedItemActionResult;
begin
	ActionResult := TSharedItemActionResult.None;

	Assert.AreEqual(satNone, ActionResult.ActionType);
end;

{Edge cases}

procedure TSharedItemActionHandlerTest.TestHandleAction_OpenEmptyListing_ReturnsNone;
var
	RealPath: TRealPath;
	Listing: TCMRDirItemList;
	ActionResult: TSharedItemActionResult;
begin
	RealPath.FromPath('\account.shared\somefile.txt');
	SetLength(Listing, 0);

	ActionResult := FHandler.HandleAction(RealPath, True, Listing);

	Assert.AreEqual(satNone, ActionResult.ActionType);
end;

procedure TSharedItemActionHandlerTest.TestHandleAction_PropertiesEmptyListing_ReturnsNone;
var
	RealPath: TRealPath;
	Listing: TCMRDirItemList;
	ActionResult: TSharedItemActionResult;
begin
	{Non-root path with empty listing}
	RealPath.FromPath('\account.shared\somefile.txt');
	SetLength(Listing, 0);

	ActionResult := FHandler.HandleAction(RealPath, False, Listing);

	Assert.AreEqual(satNone, ActionResult.ActionType);
end;

initialization
	TDUnitX.RegisterTestFixture(TSharedItemActionHandlerTest);

end.

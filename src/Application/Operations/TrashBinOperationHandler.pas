unit TrashBinOperationHandler;

{Handles trashbin operations (empty, restore single, restore all).
 Orchestrates dialog display and cloud operations based on user choice.}

interface

uses
	Windows,
	CMRDirItem,
	CMRDirItemList,
	CloudMailRu;

type
	{Callback type for showing trashbin properties dialog.
	 Returns: mrNo=Empty, mrYes=Restore single, mrYesToAll=Restore all, other=Cancel}
	TShowTrashPropertiesFunc = reference to function(ParentWindow: HWND;
		Items: TCMRDirItemList; IsTrashDir: Boolean; const AccountName: WideString): Integer;

	ITrashBinOperationHandler = interface
		['{C9D5E6F7-4A5B-6C7D-8E9F-0A1B2C3D4E5F}']

		{Executes trashbin operation based on dialog result.
		 @param ParentWindow Parent window handle for dialog
		 @param Cloud Cloud connection for the account
		 @param Listing Current trashbin listing
		 @param Item Current item (for single item operations)
		 @param IsTrashDir True if operating on main trashbin folder
		 @param AccountName Account name for dialog display
		 @param ShowDialog Callback to show the properties dialog
		 @return FS_EXEC_OK on success, FS_EXEC_ERROR on failure}
		function Execute(ParentWindow: HWND; Cloud: TCloudMailRu;
			var Listing: TCMRDirItemList; const Item: TCMRDirItem;
			IsTrashDir: Boolean; const AccountName: WideString;
			ShowDialog: TShowTrashPropertiesFunc): Integer;
	end;

	TTrashBinOperationHandler = class(TInterfacedObject, ITrashBinOperationHandler)
	public
		function Execute(ParentWindow: HWND; Cloud: TCloudMailRu;
			var Listing: TCMRDirItemList; const Item: TCMRDirItem;
			IsTrashDir: Boolean; const AccountName: WideString;
			ShowDialog: TShowTrashPropertiesFunc): Integer;
	end;

implementation

uses
	Controls,
	PLUGIN_TYPES;

function TTrashBinOperationHandler.Execute(ParentWindow: HWND; Cloud: TCloudMailRu;
	var Listing: TCMRDirItemList; const Item: TCMRDirItem;
	IsTrashDir: Boolean; const AccountName: WideString;
	ShowDialog: TShowTrashPropertiesFunc): Integer;
var
	DialogResult: Integer;
	CurrentItem: TCMRDirItem;
begin
	Result := FS_EXEC_OK;

	if not Assigned(Cloud) then
		Exit(FS_EXEC_ERROR);

	{Show dialog - for trash dir show all items, for single item show just that item}
	if IsTrashDir then
		DialogResult := ShowDialog(ParentWindow, Listing, True, AccountName)
	else
		DialogResult := ShowDialog(ParentWindow, [Item], False, '');

	case DialogResult of
		mrNo: {Empty trashbin}
			if not Cloud.trashbinEmpty then
				Exit(FS_EXEC_ERROR);
		mrYes: {Restore single item}
			if not Cloud.trashbinRestore(Item.deleted_from + Item.name, Item.rev) then
				Exit(FS_EXEC_ERROR);
		mrYesToAll: {Restore all items}
			for CurrentItem in Listing do
				if not Cloud.trashbinRestore(CurrentItem.deleted_from + CurrentItem.name, CurrentItem.rev) then
					Exit(FS_EXEC_ERROR);
	end;
end;

end.

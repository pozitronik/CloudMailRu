unit TrashBinOperationHandler;

{Handles trashbin operations: empty, restore single item, restore all.}

interface

uses
	Windows,
	CMRDirItem,
	CMRDirItemList,
	CloudMailRu,
	ITrashBinOperationHandlerInterface;

type
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

unit ITrashBinOperationHandlerInterface;

{Interface for handling trashbin operations (empty, restore single, restore all).
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

implementation

end.

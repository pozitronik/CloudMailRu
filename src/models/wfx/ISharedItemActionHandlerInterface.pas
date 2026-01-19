unit ISharedItemActionHandlerInterface;

{Interface for handling shared folder actions (open as symlink, show properties).
 Determines action type and provides necessary context for UI operations.}

interface

uses
	CMRDirItem,
	CMRDirItemList,
	RealPath;

type
	{Type of action to perform after handler processing}
	TSharedActionType = (
		satSymlink,         {Open action - resolved path for symlink navigation}
		satAccountSettings, {Properties on root - show account settings dialog}
		satPropertyDialog,  {Properties on item - show property dialog}
		satNone             {No action needed}
	);

	{Result of shared action handling - tells caller what to do}
	TSharedItemActionResult = record
		ActionType: TSharedActionType;
		SymlinkPath: WideString;    {For satSymlink: the resolved path to navigate to}
		CurrentItem: TCMRDirItem;   {For satPropertyDialog: the item to show properties for}

		class function Symlink(const APath: WideString): TSharedItemActionResult; static;
		class function AccountSettings: TSharedItemActionResult; static;
		class function PropertyDialog(const AItem: TCMRDirItem): TSharedItemActionResult; static;
		class function None: TSharedItemActionResult; static;
	end;

	ISharedItemActionHandler = interface
		['{C3D4E5F6-A7B8-9012-CDEF-345678901234}']
		{Handles shared folder action (open or properties).
		 @param RealPath The parsed path of the shared item
		 @param ActionOpen True for open (symlink), False for properties
		 @param CurrentListing Current directory listing to find item in
		 @returns TSharedItemActionResult describing what action the caller should take}
		function HandleAction(const RealPath: TRealPath; ActionOpen: Boolean;
			const CurrentListing: TCMRDirItemList): TSharedItemActionResult;
	end;

implementation

class function TSharedItemActionResult.Symlink(const APath: WideString): TSharedItemActionResult;
begin
	Result := Default(TSharedItemActionResult);
	Result.ActionType := satSymlink;
	Result.SymlinkPath := APath;
end;

class function TSharedItemActionResult.AccountSettings: TSharedItemActionResult;
begin
	Result := Default(TSharedItemActionResult);
	Result.ActionType := satAccountSettings;
end;

class function TSharedItemActionResult.PropertyDialog(const AItem: TCMRDirItem): TSharedItemActionResult;
begin
	Result := Default(TSharedItemActionResult);
	Result.ActionType := satPropertyDialog;
	Result.CurrentItem := AItem;
end;

class function TSharedItemActionResult.None: TSharedItemActionResult;
begin
	Result := Default(TSharedItemActionResult);
	Result.ActionType := satNone;
end;

end.

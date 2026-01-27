unit SharedItemActionHandler;

{Handles shared folder actions (open as symlink, show properties).
	Determines action type and provides necessary context for UI operations.}

interface

uses
	CloudDirItem,
	CloudDirItemList,
	RealPath;

type
	{Type of action to perform after handler processing}
	TSharedActionType = (satSymlink, {Open action - resolved path for symlink navigation}
		satAccountSettings, {Properties on root - show account settings dialog}
		satPropertyDialog, {Properties on item - show property dialog}
		satNone {No action needed}
		);

	{Result of shared action handling - tells caller what to do}
	TSharedItemActionResult = record
		ActionType: TSharedActionType;
		SymlinkPath: WideString; {For satSymlink: the resolved path to navigate to}
		CurrentItem: TCloudDirItem; {For satPropertyDialog: the item to show properties for}

		class function Symlink(const APath: WideString): TSharedItemActionResult; static;
		class function AccountSettings: TSharedItemActionResult; static;
		class function PropertyDialog(const AItem: TCloudDirItem): TSharedItemActionResult; static;
		class function None: TSharedItemActionResult; static;
	end;

	ISharedItemActionHandler = interface
		['{29A38D30-A19F-46C2-9C56-B0BFD6C0549F}']
		{Handles shared folder action (open or properties).
			@param RealPath The parsed path of the shared item
			@param ActionOpen True for open (symlink), False for properties
			@param CurrentListing Current directory listing to find item in
			@returns TSharedItemActionResult describing what action the caller should take}
		function HandleAction(const RealPath: TRealPath; ActionOpen: Boolean; const CurrentListing: TCloudDirItemList): TSharedItemActionResult;
	end;

	TSharedItemActionHandler = class(TInterfacedObject, ISharedItemActionHandler)
	public
		function HandleAction(const RealPath: TRealPath; ActionOpen: Boolean; const CurrentListing: TCloudDirItemList): TSharedItemActionResult;
	end;

implementation

uses
	SysUtils,
	CloudConstants,
	PathHelper;

class function TSharedItemActionResult.Symlink(const APath: WideString): TSharedItemActionResult;
begin
	Result := Default (TSharedItemActionResult);
	Result.ActionType := satSymlink;
	Result.SymlinkPath := APath;
end;

class function TSharedItemActionResult.AccountSettings: TSharedItemActionResult;
begin
	Result := Default (TSharedItemActionResult);
	Result.ActionType := satAccountSettings;
end;

class function TSharedItemActionResult.PropertyDialog(const AItem: TCloudDirItem): TSharedItemActionResult;
begin
	Result := Default (TSharedItemActionResult);
	Result.ActionType := satPropertyDialog;
	Result.CurrentItem := AItem;
end;

class function TSharedItemActionResult.None: TSharedItemActionResult;
begin
	Result := Default (TSharedItemActionResult);
	Result.ActionType := satNone;
end;

function TSharedItemActionHandler.HandleAction(const RealPath: TRealPath; ActionOpen: Boolean; const CurrentListing: TCloudDirItemList): TSharedItemActionResult;
var
	CurrentItem: TCloudDirItem;
	ResolvedPath: WideString;
begin
	if ActionOpen then
	begin
		{Open action - resolve shared item to its actual location (symlink)}
		CurrentItem := CurrentListing.FindByHomePath(RealPath.Path);
		if CurrentItem.IsNone then
			Exit(TSharedItemActionResult.None);

		{Build path: \account + original location}
		if CurrentItem.type_ = TYPE_FILE then
			ResolvedPath := '\' + RealPath.account + ExtractFilePath(UrlToPath(CurrentItem.home))
		else
			ResolvedPath := '\' + RealPath.account + UrlToPath(CurrentItem.home);

		Result := TSharedItemActionResult.Symlink(ResolvedPath);
	end else begin
		{Properties action - determine what dialog to show}
		if RealPath.isInAccountsList then
		begin
			{Root of shared folder - show account settings}
			Result := TSharedItemActionResult.AccountSettings;
		end else begin
			{Item in shared folder - show property dialog}
			CurrentItem := CurrentListing.FindByHomePath(RealPath.Path);
			if CurrentItem.IsNone then
				Exit(TSharedItemActionResult.None);
			Result := TSharedItemActionResult.PropertyDialog(CurrentItem);
		end;
	end;
end;

end.

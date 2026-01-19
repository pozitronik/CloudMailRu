unit SharedItemActionHandler;

{Handles shared folder actions - symlink resolution for open, action routing for properties.
 Extracts logic from ExecSharedAction to make it testable.}

interface

uses
	ISharedItemActionHandlerInterface,
	CMRDirItemList,
	RealPath;

type
	TSharedItemActionHandler = class(TInterfacedObject, ISharedItemActionHandler)
	public
		function HandleAction(const RealPath: TRealPath; ActionOpen: Boolean;
			const CurrentListing: TCMRDirItemList): TSharedItemActionResult;
	end;

implementation

uses
	SysUtils,
	CMRDirItem,
	CMRConstants,
	PathHelper;

function TSharedItemActionHandler.HandleAction(const RealPath: TRealPath; ActionOpen: Boolean;
	const CurrentListing: TCMRDirItemList): TSharedItemActionResult;
var
	CurrentItem: TCMRDirItem;
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
	end
	else
	begin
		{Properties action - determine what dialog to show}
		if RealPath.isInAccountsList then
		begin
			{Root of shared folder - show account settings}
			Result := TSharedItemActionResult.AccountSettings;
		end
		else
		begin
			{Item in shared folder - show property dialog}
			CurrentItem := CurrentListing.FindByHomePath(RealPath.Path);
			if CurrentItem.IsNone then
				Exit(TSharedItemActionResult.None);
			Result := TSharedItemActionResult.PropertyDialog(CurrentItem);
		end;
	end;
end;

end.

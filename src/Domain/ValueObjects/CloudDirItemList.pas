unit CloudDirItemList;

interface

uses
	System.Generics.Collections,
	SysUtils,
	CloudDirItem;

type
	TCloudDirItemList = TArray<TCloudDirItem>;

	TCloudDirItemListHelper = record helper for TCloudDirItemList
	public
		function FindByName(ItemName: WideString): TCloudDirItem;
		function FindByHomePath(HomePath: WideString): TCloudDirItem;
	end;

implementation

{TCloudDirItemList}

{It seems that this kind of search is preferrable somehow, but I forgot, why. So, I just keep the logic.}
function TCloudDirItemListHelper.FindByHomePath(HomePath: WideString): TCloudDirItem;
var
	CurrentItem: TCloudDirItem;
begin
	HomePath := '/' + StringReplace(HomePath, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]);
	for CurrentItem in self do
		if CurrentItem.home = HomePath then
			exit(CurrentItem);
	exit(CurrentItem.None);
end;

function TCloudDirItemListHelper.FindByName(ItemName: WideString): TCloudDirItem;
var
	CurrentItem: TCloudDirItem;
begin
	for CurrentItem in self do
		if CurrentItem.name = ItemName then
			exit(CurrentItem);
	exit(CurrentItem.None);
end;

end.

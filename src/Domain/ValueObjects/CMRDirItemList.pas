unit CMRDirItemList;

interface

uses
	System.Generics.Collections,
	SysUtils,
	CMRDirItem;

type
	TCMRDirItemList = TArray<TCMRDirItem>;

	TCMRDirItemListHelper = record helper for TCMRDirItemList
	public
		function FindByName(ItemName: WideString): TCMRDirItem;
		function FindByHomePath(HomePath: WideString): TCMRDirItem;
	end;

implementation

{TCMRDirItemList}

{It seems that this kind of search is preferrable somehow, but I forgot, why. So, I just keep the logic.}
function TCMRDirItemListHelper.FindByHomePath(HomePath: WideString): TCMRDirItem;
var
	CurrentItem: TCMRDirItem;
begin
	HomePath := '/' + StringReplace(HomePath, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]);
	for CurrentItem in self do
		if CurrentItem.home = HomePath then
			exit(CurrentItem);
	exit(CurrentItem.None);
end;

function TCMRDirItemListHelper.FindByName(ItemName: WideString): TCMRDirItem;
var
	CurrentItem: TCMRDirItem;
begin
	for CurrentItem in self do
		if CurrentItem.name = ItemName then
			exit(CurrentItem);
	exit(CurrentItem.None);
end;

end.

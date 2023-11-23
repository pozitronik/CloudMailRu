unit CloudMailRuDirListing;

interface

uses
	SysUtils,
	CloudMailRuDirListingItem;

type
	TCloudMailRuDirListing = TArray<TCloudMailRuDirListingItem>;

function FindListingItemByName(DirListing: TCloudMailRuDirListing; ItemName: WideString): TCloudMailRuDirListingItem;
function FindListingItemByHomePath(DirListing: TCloudMailRuDirListing; HomePath: WideString): TCloudMailRuDirListingItem;

implementation

function FindListingItemByName(DirListing: TCloudMailRuDirListing; ItemName: WideString): TCloudMailRuDirListingItem;
var
	CurrentItem: TCloudMailRuDirListingItem;
begin
	for CurrentItem in DirListing do
		if CurrentItem.name = ItemName then
			exit(CurrentItem);
	FillChar(CurrentItem, sizeof(CurrentItem), 0);
	exit(CurrentItem); // nothing found
end;

function FindListingItemByHomePath(DirListing: TCloudMailRuDirListing; HomePath: WideString): TCloudMailRuDirListingItem;
var
	CurrentItem: TCloudMailRuDirListingItem;
begin
	HomePath := '/' + StringReplace(HomePath, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]);
	for CurrentItem in DirListing do
		if CurrentItem.home = HomePath then
			exit(CurrentItem);
	FillChar(CurrentItem, sizeof(CurrentItem), 0);
	exit(CurrentItem); // nothing found
end;

end.

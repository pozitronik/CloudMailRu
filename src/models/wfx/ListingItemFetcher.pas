unit ListingItemFetcher;

{Context-aware listing item fetcher.
 Finds items in listing by path, refreshing from appropriate cloud source
 (trashbin, shared links, or direct status) if not found.}

interface

uses
	RealPath,
	CMRDirItem,
	CMRDirItemList,
	CloudMailRu,
	ILoggerInterface,
	IListingItemFetcherInterface;

type
	TListingItemFetcher = class(TInterfacedObject, IListingItemFetcher)
	private
		FLogger: ILogger;

		{Searches listing by appropriate key (home path or name)}
		function SearchListing(const Listing: TCMRDirItemList; const Path: TRealPath;
			IsPublicAccount: Boolean): TCMRDirItem;

		{Refreshes listing and searches again based on path context}
		function RefreshAndSearch(var Listing: TCMRDirItemList; const Path: TRealPath;
			Cloud: TCloudMailRu): TCMRDirItem;
	public
		constructor Create(Logger: ILogger);

		function FetchItem(var Listing: TCMRDirItemList; const Path: TRealPath;
			Cloud: TCloudMailRu; UpdateListing: Boolean): TCMRDirItem;
	end;

implementation

uses
	SysUtils,
	CMRConstants,
	LANGUAGE_STRINGS,
	PLUGIN_TYPES,
	PathHelper;

constructor TListingItemFetcher.Create(Logger: ILogger);
begin
	inherited Create;
	FLogger := Logger;
end;

function TListingItemFetcher.SearchListing(const Listing: TCMRDirItemList;
	const Path: TRealPath; IsPublicAccount: Boolean): TCMRDirItem;
begin
	//сначала попробуем найти поле в имеющемся списке
	if Path.HasHomePath and not IsPublicAccount then
		Result := Listing.FindByHomePath(Path.Path)
	else
		Result := Listing.FindByName(ExtractUniversalFileName(Path.Path));
end;

function TListingItemFetcher.RefreshAndSearch(var Listing: TCMRDirItemList;
	const Path: TRealPath; Cloud: TCloudMailRu): TCMRDirItem;
begin
	Result := Result.None;

	//если там его нет (нажали пробел на папке, например), то запросим в облаке напрямую
	if Path.trashDir then //корзина - обновим CurrentListing, поищем в нём
	begin
		if Cloud.getTrashbinListing(Listing) then
			Exit(Listing.FindByName(Path.Path));
	end;

	if Path.sharedDir then //ссылки - обновим список
	begin
		if Cloud.getSharedLinksListing(Listing) then
			Exit(Listing.FindByName(Path.Path));
	end;

	if Path.invitesDir then
	begin
		//FindIncomingInviteItemByPath in that case!
		Exit;
	end;

	//Обычный каталог
	if Cloud.statusFile(Path.Path, Result) then
	begin
		if (Result.home = EmptyWideStr) and not Cloud.IsPublicAccount then
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_WHERE_IS_THE_FILE, [Path.Path]); {Такого быть не может, но...}
	end;
	//Не рапортуем, это будет уровнем выше
end;

function TListingItemFetcher.FetchItem(var Listing: TCMRDirItemList;
	const Path: TRealPath; Cloud: TCloudMailRu; UpdateListing: Boolean): TCMRDirItem;
begin
	if not Assigned(Cloud) then
	begin
		Result := Result.None;
		Exit;
	end;

	Result := SearchListing(Listing, Path, Cloud.IsPublicAccount);

	if Result.isNone and UpdateListing then
		Result := RefreshAndSearch(Listing, Path, Cloud);
end;

end.

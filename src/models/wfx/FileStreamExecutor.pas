unit FileStreamExecutor;

{Executes file streaming operations.
 Handles streaming URL resolution based on format (playlist vs direct),
 automatic file publication when needed, and command execution.}

interface

uses
	RealPath,
	CMRDirItem,
	CMRConstants,
	StreamingSettings,
	ConnectionManager,
	CloudMailRu,
	IFileStreamExecutorInterface;

type
	TFileStreamExecutor = class(TInterfacedObject, IFileStreamExecutor)
	private
		{Resolves streaming URL based on format.
		 For playlist: gets HLS stream URL.
		 For other formats: publishes file if needed and gets shared URL.}
		function ResolveStreamUrl(const RealPath: TRealPath; const Item: TCMRDirItem;
			Format: Integer; TempCloud: TCloudMailRu; ConnManager: TConnectionManager;
			out StreamUrl: WideString): Boolean;

		{Executes streaming command with URL substitution.}
		function ExecuteCommand(var Settings: TStreamingSettings;
			const StreamUrl: WideString): Boolean;
	public
		function Execute(const RealPath: TRealPath; const Item: TCMRDirItem;
			var Settings: TStreamingSettings; ConnManager: TConnectionManager): Integer;
	end;

implementation

uses
	SysUtils,
	PLUGIN_TYPES,
	PluginHelper,
	WindowsHelper;

function TFileStreamExecutor.ResolveStreamUrl(const RealPath: TRealPath;
	const Item: TCMRDirItem; Format: Integer; TempCloud: TCloudMailRu;
	ConnManager: TConnectionManager; out StreamUrl: WideString): Boolean;
var
	getResult: Integer;
	CurrentCloud: TCloudMailRu;
	MutableItem: TCMRDirItem;
begin
	Result := True;
	MutableItem := Item;

	if STREAMING_FORMAT_PLAYLIST = Format then
	begin
		{Playlist format - get HLS stream URL directly}
		Result := TempCloud.getPublishedFileStreamUrl(MutableItem, StreamUrl);
	end
	else
	begin
		{Other formats - ensure file is published first}
		if not Item.isPublished then
		begin
			CurrentCloud := ConnManager.Get(RealPath.account, getResult);
			Result := CurrentCloud.publishFile(MutableItem.home, MutableItem.weblink);
			//Здесь можно бы обновить листинг
		end;

		if Result then
			StreamUrl := TempCloud.getSharedFileUrl(EmptyWideStr,
				ShardTypeFromStreamingFormat(Format));
	end;
end;

function TFileStreamExecutor.ExecuteCommand(var Settings: TStreamingSettings;
	const StreamUrl: WideString): Boolean;
begin
	{Default to %url% if no parameters specified}
	if EmptyWideStr = Settings.Parameters then
		Settings.Parameters := '%url%';

	{Substitute URL placeholder}
	Settings.Parameters := StringReplace(Settings.Parameters, '%url%', StreamUrl,
		[rfReplaceAll, rfIgnoreCase]);

	Result := Run(Settings.Command, StreamUrl, Settings.StartPath);
end;

function TFileStreamExecutor.Execute(const RealPath: TRealPath;
	const Item: TCMRDirItem; var Settings: TStreamingSettings;
	ConnManager: TConnectionManager): Integer;
var
	StreamUrl: WideString;
	TempPublicCloud: TCloudMailRu;
begin
	Result := FS_EXEC_OK;

	{Skip if streaming is disabled}
	if (STREAMING_FORMAT_DISABLED = Settings.Format) or
	   (STREAMING_FORMAT_UNSET = Settings.Format) then
		Exit;

	{Initialize temporary public cloud for URL resolution}
	if not TCloudMailRu.TempPublicCloudInit(TempPublicCloud,
		PUBLIC_ACCESS_URL + Item.weblink) then
		Exit(FS_EXEC_ERROR);

	try
		{Resolve streaming URL based on format}
		if not ResolveStreamUrl(RealPath, Item, Settings.Format, TempPublicCloud,
			ConnManager, StreamUrl) then
			Exit(FS_EXEC_ERROR);

		{Execute streaming command}
		if not ExecuteCommand(Settings, StreamUrl) then
			Result := FS_EXEC_ERROR;
	finally
		FreeAndNil(TempPublicCloud);
	end;
end;

end.

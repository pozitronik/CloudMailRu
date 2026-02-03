unit FileStreamExecutor;

{Executes file streaming operations.
	Handles streaming URL resolution based on format (playlist vs direct),
	automatic file publication when needed, and command execution.
	Handles URL resolution, file publication, and command execution for streaming.}

interface

uses
	CloudDirItem,
	CloudConstants,
	StreamingSettings,
	RealPath,
	CloudMailRu,
	CloudMailRuFactory,
	CommandExecutor,
	ConnectionManager;

{Maps streaming format to corresponding shard type for URL resolution.
	Exposed for testability.}
function ShardTypeFromStreamingFormat(StreamingFormat: Integer): string;

type
	IFileStreamExecutor = interface
		['{B8E5D3A1-7C9F-4E2B-A6D8-9F1C3E5B7A2D}']

		{Executes file streaming for the given item.
			Resolves streaming URL based on format, publishes file if needed,
			and launches the configured streaming command.
			@param RealPath Parsed path with account info (for connection lookup)
			@param Item Directory item with weblink info
			@param Settings Streaming configuration (command, parameters, format)
			@param ConnManager Connection manager for resolving cloud instances
			@return FS_EXEC_OK on success, FS_EXEC_ERROR on failure}
		function Execute(const RealPath: TRealPath; const Item: TCloudDirItem; var Settings: TStreamingSettings; ConnManager: IConnectionManager): Integer;
	end;

	TFileStreamExecutor = class(TInterfacedObject, IFileStreamExecutor)
	private
		FCloudFactory: IPublicCloudFactory;
		FCommandExecutor: ICommandExecutor;

		{Resolves streaming URL based on format.
			For playlist: gets HLS stream URL.
			For other formats: publishes file if needed and gets shared URL.}
		function ResolveStreamUrl(Cloud: TCloudMailRu; const Item: TCloudDirItem; Format: Integer; TempCloud: TCloudMailRu; out StreamUrl: WideString): Boolean;

		{Executes streaming command with URL substitution.}
		function ExecuteCommand(var Settings: TStreamingSettings; const StreamUrl: WideString): Boolean;
	public
		{Creates executor with injected dependencies.
			@param CloudFactory Factory for creating public cloud instances
			@param CommandExecutor Executor for running external commands}
		constructor Create(CloudFactory: IPublicCloudFactory; CommandExecutor: ICommandExecutor);

		function Execute(const RealPath: TRealPath; const Item: TCloudDirItem; var Settings: TStreamingSettings; ConnManager: IConnectionManager): Integer;
	end;

implementation

uses
	SysUtils,
	WFXTypes;

function ShardTypeFromStreamingFormat(StreamingFormat: Integer): string;
begin
	case StreamingFormat of
		STREAMING_FORMAT_WEBLINK_VIEW:
			Result := SHARD_TYPE_WEBLINK_VIEW;
		STREAMING_FORMAT_VIDEO:
			Result := SHARD_TYPE_VIDEO;
		STREAMING_FORMAT_VIEW_DIRECT:
			Result := SHARD_TYPE_VIEW_DIRECT;
		STREAMING_FORMAT_THUMBNAILS:
			Result := SHARD_TYPE_THUMBNAILS;
		STREAMING_FORMAT_WEBLINK_THUMBNAILS:
			Result := SHARD_TYPE_WEBLINK_THUMBNAILS;
		else
			Result := SHARD_TYPE_DEFAULT;
	end;
end;

constructor TFileStreamExecutor.Create(CloudFactory: IPublicCloudFactory; CommandExecutor: ICommandExecutor);
begin
	inherited Create;
	FCloudFactory := CloudFactory;
	FCommandExecutor := CommandExecutor;
end;

function TFileStreamExecutor.ResolveStreamUrl(Cloud: TCloudMailRu; const Item: TCloudDirItem; Format: Integer; TempCloud: TCloudMailRu; out StreamUrl: WideString): Boolean;
var
	MutableItem: TCloudDirItem;
begin
	Result := True;
	MutableItem := Item;

	if STREAMING_FORMAT_PLAYLIST = Format then
	begin
		{Playlist format - get HLS stream URL directly}
		Result := TempCloud.ShareService.GetPublishedFileStreamUrl(MutableItem, StreamUrl);
	end else begin
		{Other formats - ensure file is published first}
		if not Item.isPublished then
		begin
			Result := Cloud.PublishFile(MutableItem.home, MutableItem.weblink);
			{Could refresh the listing here}
		end;

		if Result then
			StreamUrl := TempCloud.Downloader.GetSharedFileUrl(EmptyWideStr, ShardTypeFromStreamingFormat(Format));
	end;
end;

function TFileStreamExecutor.ExecuteCommand(var Settings: TStreamingSettings; const StreamUrl: WideString): Boolean;
begin
	{Default to %url% if no parameters specified}
	if EmptyWideStr = Settings.Parameters then
		Settings.Parameters := '%url%';

	{Substitute URL placeholder}
	Settings.Parameters := StringReplace(Settings.Parameters, '%url%', StreamUrl, [rfReplaceAll, rfIgnoreCase]);

	Result := FCommandExecutor.Execute(Settings.Command, StreamUrl, Settings.StartPath);
end;

function TFileStreamExecutor.Execute(const RealPath: TRealPath; const Item: TCloudDirItem; var Settings: TStreamingSettings; ConnManager: IConnectionManager): Integer;
var
	StreamUrl: WideString;
	TempPublicCloud: TCloudMailRu;
	Cloud: TCloudMailRu;
begin
	Result := FS_EXEC_OK;

	{Skip if streaming is disabled}
	if (STREAMING_FORMAT_DISABLED = Settings.Format) or (STREAMING_FORMAT_UNSET = Settings.Format) then
		Exit;

	Cloud := ConnManager.Get(RealPath.Account);

	{Initialize temporary public cloud for URL resolution}
	if not FCloudFactory.CreatePublicCloud(TempPublicCloud, Cloud.GetEndpoints.PublicUrl + Item.weblink) then
		Exit(FS_EXEC_ERROR);

	try
		{Resolve streaming URL based on format}
		if not ResolveStreamUrl(Cloud, Item, Settings.Format, TempPublicCloud, StreamUrl) then
			Exit(FS_EXEC_ERROR);

		{Execute streaming command}
		if not ExecuteCommand(Settings, StreamUrl) then
			Result := FS_EXEC_ERROR;
	finally
		FreeAndNil(TempPublicCloud);
	end;
end;

end.

unit CloudShardManager;

interface

uses
	CMRConstants,
	TCLogger;

type
	{Interface for managing cloud shard URLs.
	 Shards are server endpoints for specific operations (download, upload, etc.).
	 This interface abstracts shard caching and override handling.}
	ICloudShardManager = interface
		['{B4C8D2E6-F7A3-4B5C-9D1E-2F3A4B5C6D7E}']
		{Get the download shard URL, returns empty string if not yet resolved}
		function GetDownloadShard: WideString;
		{Set the download shard URL after resolution}
		procedure SetDownloadShard(const Shard: WideString);
		{Get the upload shard URL, returns empty string if not yet resolved}
		function GetUploadShard: WideString;
		{Set the upload shard URL after resolution}
		procedure SetUploadShard(const Shard: WideString);
		{Get the public shard URL, returns empty string if not yet resolved}
		function GetPublicShard: WideString;
		{Set the public shard URL after resolution}
		procedure SetPublicShard(const Shard: WideString);
		{Get the override URL for download if configured, empty otherwise}
		function GetDownloadShardOverride: WideString;
		{Get the override URL for upload if configured, empty otherwise}
		function GetUploadShardOverride: WideString;
		{Check if download shard override is configured}
		function HasDownloadOverride: Boolean;
		{Check if upload shard override is configured}
		function HasUploadOverride: Boolean;
		{Invalidate (clear) a specific shard type to force re-resolution}
		procedure InvalidateShard(ShardType: WideString);
		{Clear all cached shards}
		procedure InvalidateAll;
	end;

	{Shard manager implementation that caches shard URLs and handles overrides.
	 Shard resolution (HTTP calls) remains external - this class only manages state.}
	TCloudShardManager = class(TInterfacedObject, ICloudShardManager)
	private
		FDownloadShard: WideString;
		FUploadShard: WideString;
		FPublicShard: WideString;
		FDownloadOverride: WideString;
		FUploadOverride: WideString;
		FLogger: ILogger;
	public
		constructor Create(Logger: ILogger; DownloadOverride: WideString = ''; UploadOverride: WideString = '');

		function GetDownloadShard: WideString;
		procedure SetDownloadShard(const Shard: WideString);
		function GetUploadShard: WideString;
		procedure SetUploadShard(const Shard: WideString);
		function GetPublicShard: WideString;
		procedure SetPublicShard(const Shard: WideString);
		function GetDownloadShardOverride: WideString;
		function GetUploadShardOverride: WideString;
		function HasDownloadOverride: Boolean;
		function HasUploadOverride: Boolean;
		procedure InvalidateShard(ShardType: WideString);
		procedure InvalidateAll;
	end;

	{Null implementation for testing}
	TNullShardManager = class(TInterfacedObject, ICloudShardManager)
	public
		function GetDownloadShard: WideString;
		procedure SetDownloadShard(const Shard: WideString);
		function GetUploadShard: WideString;
		procedure SetUploadShard(const Shard: WideString);
		function GetPublicShard: WideString;
		procedure SetPublicShard(const Shard: WideString);
		function GetDownloadShardOverride: WideString;
		function GetUploadShardOverride: WideString;
		function HasDownloadOverride: Boolean;
		function HasUploadOverride: Boolean;
		procedure InvalidateShard(ShardType: WideString);
		procedure InvalidateAll;
	end;

implementation

uses
	System.SysUtils,
	PLUGIN_TYPES,
	LANGUAGE_STRINGS;

{ TCloudShardManager }

constructor TCloudShardManager.Create(Logger: ILogger; DownloadOverride: WideString; UploadOverride: WideString);
begin
	inherited Create;
	FLogger := Logger;
	FDownloadOverride := DownloadOverride;
	FUploadOverride := UploadOverride;
	FDownloadShard := EmptyWideStr;
	FUploadShard := EmptyWideStr;
	FPublicShard := EmptyWideStr;
end;

function TCloudShardManager.GetDownloadShard: WideString;
begin
	Result := FDownloadShard;
end;

procedure TCloudShardManager.SetDownloadShard(const Shard: WideString);
begin
	FDownloadShard := Shard;
	if (Shard <> EmptyWideStr) then
		FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, PREFIX_SHARD_RECEIVED, [Shard, SHARD_TYPE_GET]);
end;

function TCloudShardManager.GetUploadShard: WideString;
begin
	Result := FUploadShard;
end;

procedure TCloudShardManager.SetUploadShard(const Shard: WideString);
begin
	FUploadShard := Shard;
	if (Shard <> EmptyWideStr) then
		FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, PREFIX_SHARD_RECEIVED, [Shard, SHARD_TYPE_UPLOAD]);
end;

function TCloudShardManager.GetPublicShard: WideString;
begin
	Result := FPublicShard;
end;

procedure TCloudShardManager.SetPublicShard(const Shard: WideString);
begin
	FPublicShard := Shard;
end;

function TCloudShardManager.GetDownloadShardOverride: WideString;
begin
	Result := FDownloadOverride;
end;

function TCloudShardManager.GetUploadShardOverride: WideString;
begin
	Result := FUploadOverride;
end;

function TCloudShardManager.HasDownloadOverride: Boolean;
begin
	Result := FDownloadOverride <> EmptyWideStr;
end;

function TCloudShardManager.HasUploadOverride: Boolean;
begin
	Result := FUploadOverride <> EmptyWideStr;
end;

procedure TCloudShardManager.InvalidateShard(ShardType: WideString);
begin
	if ShardType = SHARD_TYPE_GET then
		FDownloadShard := EmptyWideStr
	else if ShardType = SHARD_TYPE_UPLOAD then
		FUploadShard := EmptyWideStr
	else if ShardType = SHARD_TYPE_WEBLINK_GET then
		FPublicShard := EmptyWideStr;
end;

procedure TCloudShardManager.InvalidateAll;
begin
	FDownloadShard := EmptyWideStr;
	FUploadShard := EmptyWideStr;
	FPublicShard := EmptyWideStr;
end;

{ TNullShardManager }

function TNullShardManager.GetDownloadShard: WideString;
begin
	Result := EmptyWideStr;
end;

procedure TNullShardManager.SetDownloadShard(const Shard: WideString);
begin
	{No-op}
end;

function TNullShardManager.GetUploadShard: WideString;
begin
	Result := EmptyWideStr;
end;

procedure TNullShardManager.SetUploadShard(const Shard: WideString);
begin
	{No-op}
end;

function TNullShardManager.GetPublicShard: WideString;
begin
	Result := EmptyWideStr;
end;

procedure TNullShardManager.SetPublicShard(const Shard: WideString);
begin
	{No-op}
end;

function TNullShardManager.GetDownloadShardOverride: WideString;
begin
	Result := EmptyWideStr;
end;

function TNullShardManager.GetUploadShardOverride: WideString;
begin
	Result := EmptyWideStr;
end;

function TNullShardManager.HasDownloadOverride: Boolean;
begin
	Result := False;
end;

function TNullShardManager.HasUploadOverride: Boolean;
begin
	Result := False;
end;

procedure TNullShardManager.InvalidateShard(ShardType: WideString);
begin
	{No-op}
end;

procedure TNullShardManager.InvalidateAll;
begin
	{No-op}
end;

end.

unit CloudShardManager;

interface

uses
	CloudConstants,
	CloudContext,
	TCLogger;

type
	{Interface for managing cloud shard URLs.
		Shards are server endpoints for specific operations (download, upload, etc.).
		This interface abstracts shard caching, resolution, and override handling.}
	ICloudShardManager = interface
		['{D8F2AA4A-4560-471A-A1E7-3E374BB9A4E0}']
		{Resolve and cache a shard URL, returns true on success}
		function ResolveShard(var Shard: WideString; ShardType: WideString): Boolean;
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
		{Ensure download shard is resolved: returns cached, override, or OAuth-dispatched URL.
			Returns empty string if resolution fails.}
		function EnsureDownloadShard: WideString;
		{Ensure upload shard is resolved: returns cached, override, or OAuth-dispatched URL.
			Returns empty string if resolution fails.}
		function EnsureUploadShard: WideString;
	end;

	{Shard manager implementation that caches shard URLs and handles resolution.}
	TCloudShardManager = class(TInterfacedObject, ICloudShardManager)
	private
		FDownloadShard: WideString;
		FUploadShard: WideString;
		FPublicShard: WideString;
		FDownloadOverride: WideString;
		FUploadOverride: WideString;
		FLogger: ILogger;
		FContext: IShardContext;
		{Resolve shard via OAuth dispatcher endpoint (plain text "URL IP COUNT" format)}
		function ResolveOAuthDispatcherShard(const ShardSuffix: WideString): WideString;
		{Common logic for EnsureDownloadShard/EnsureUploadShard}
		function EnsureShard(var CachedShard: WideString; const Override, UndefinedMsg, OverrideMsg, DispatcherSuffix: WideString): WideString;
	public
		constructor Create(Logger: ILogger; Context: IShardContext; DownloadOverride: WideString = ''; UploadOverride: WideString = '');

		function ResolveShard(var Shard: WideString; ShardType: WideString): Boolean;
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
		function EnsureDownloadShard: WideString;
		function EnsureUploadShard: WideString;
	end;

	{Null implementation for testing}
	TNullShardManager = class(TInterfacedObject, ICloudShardManager)
	public
		function ResolveShard(var Shard: WideString; ShardType: WideString): Boolean;
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
		function EnsureDownloadShard: WideString;
		function EnsureUploadShard: WideString;
	end;

implementation

uses
	System.SysUtils,
	WFXTypes,
	LanguageStrings,
	JSONHelper;

{TCloudShardManager}

constructor TCloudShardManager.Create(Logger: ILogger; Context: IShardContext; DownloadOverride: WideString; UploadOverride: WideString);
begin
	inherited Create;
	FLogger := Logger;
	FContext := Context;
	FDownloadOverride := DownloadOverride;
	FUploadOverride := UploadOverride;
	FDownloadShard := EmptyWideStr;
	FUploadShard := EmptyWideStr;
	FPublicShard := EmptyWideStr;
end;

function TCloudShardManager.ResolveShard(var Shard: WideString; ShardType: WideString): Boolean;
var
	JSON: WideString;
begin
	Result := FContext.PostForm(API_DISPATCHER + '?' + FContext.GetUnitedParams, '', JSON) and FContext.CloudResultToBoolean(JSON, PREFIX_ERR_SHARD_RECEIVE);
	if Result then
	begin
		Result := JSONHelper.GetShard(JSON, Shard, ShardType) and (Shard <> EmptyWideStr);
		FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, PREFIX_SHARD_RECEIVED, [Shard, ShardType]);
	end;
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

function TCloudShardManager.ResolveOAuthDispatcherShard(const ShardSuffix: WideString): WideString;
var
	DispatcherResponse: WideString;
	ShowProgress: Boolean;
begin
	Result := EmptyWideStr;
	ShowProgress := False;
	if FContext.GetPage(Format('%s/%s?token=%s', [OAUTH_DISPATCHER_URL, ShardSuffix, FContext.GetOAuthAccessToken]), DispatcherResponse, ShowProgress) then
	begin
		{Response format: "URL IP COUNT", extract the URL (first word)}
		if Pos(' ', DispatcherResponse) > 0 then
			Result := Trim(Copy(DispatcherResponse, 1, Pos(' ', DispatcherResponse) - 1))
		else begin
			FLogger.Log(LOG_LEVEL_WARNING, MSGTYPE_DETAILS, WARN_DISPATCHER_UNEXPECTED_FORMAT, [DispatcherResponse]);
			Result := Trim(DispatcherResponse);
		end;
	end;
end;

function TCloudShardManager.EnsureShard(var CachedShard: WideString; const Override, UndefinedMsg, OverrideMsg, DispatcherSuffix: WideString): WideString;
begin
	Result := CachedShard;
	if Result <> EmptyWideStr then
		Exit;

	FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, UndefinedMsg);
	if Override <> EmptyWideStr then
	begin
		FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_DETAILS, OverrideMsg);
		Result := Override;
	end else
		Result := ResolveOAuthDispatcherShard(DispatcherSuffix);

	if Result <> EmptyWideStr then
		CachedShard := Result;
end;

function TCloudShardManager.EnsureDownloadShard: WideString;
begin
	Result := EnsureShard(FDownloadShard, FDownloadOverride, UNDEFINED_DOWNLOAD_SHARD, SHARD_OVERRIDDEN, 'd');
end;

function TCloudShardManager.EnsureUploadShard: WideString;
begin
	Result := EnsureShard(FUploadShard, FUploadOverride, UNDEFINED_UPLOAD_SHARD, UPLOAD_URL_OVERRIDDEN, 'u');
end;

{TNullShardManager}

function TNullShardManager.ResolveShard(var Shard: WideString; ShardType: WideString): Boolean;
begin
	Shard := EmptyWideStr;
	Result := False;
end;

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

function TNullShardManager.EnsureDownloadShard: WideString;
begin
	Result := EmptyWideStr;
end;

function TNullShardManager.EnsureUploadShard: WideString;
begin
	Result := EmptyWideStr;
end;

end.

unit MockShardHelper;

{Helper class for generating shard/dispatcher JSON responses in tests.
 Simplifies test setup for operations requiring shard URL retrieval.}

interface

uses
	System.SysUtils,
	System.Generics.Collections,
	CMRConstants;

type
	{Shard definition for test setup}
	TMockShard = record
		ShardType: WideString;
		URL: WideString;
		constructor Create(const AShardType, AURL: WideString);
	end;

	{Helper for generating dispatcher/shard JSON responses}
	TMockShardHelper = class
	public
		{Create single shard response}
		class function CreateShardResponse(const ShardType, URL: WideString): WideString; static;

		{Create dispatcher response with multiple shards}
		class function CreateDispatcherResponse(const Shards: array of TMockShard): WideString; static;

		{Convenience methods for common shard types}
		class function CreateDownloadShardResponse(const URL: WideString): WideString; static;
		class function CreateUploadShardResponse(const URL: WideString): WideString; static;
		class function CreateVideoShardResponse(const URL: WideString): WideString; static;
		class function CreateWeblinkGetShardResponse(const URL: WideString): WideString; static;
		class function CreateWeblinkVideoShardResponse(const URL: WideString): WideString; static;

		{Create full dispatcher response with all common shard types}
		class function CreateFullDispatcherResponse(
			const DownloadURL: WideString = 'https://download.cloud.mail.ru/';
			const UploadURL: WideString = 'https://upload.cloud.mail.ru/';
			const VideoURL: WideString = 'https://video.cloud.mail.ru/';
			const WeblinkGetURL: WideString = 'https://weblink.cloud.mail.ru/';
			const WeblinkVideoURL: WideString = 'https://weblinkvideo.cloud.mail.ru/'): WideString; static;

		{OAuth dispatcher responses (plain text format)}
		class function CreateOAuthDownloadDispatcherResponse(const URL: WideString): WideString; static;
		class function CreateOAuthUploadDispatcherResponse(const URL: WideString): WideString; static;
	end;

implementation

{TMockShard}

constructor TMockShard.Create(const AShardType, AURL: WideString);
begin
	ShardType := AShardType;
	URL := AURL;
end;

{TMockShardHelper}

class function TMockShardHelper.CreateShardResponse(const ShardType, URL: WideString): WideString;
begin
	{Single shard in dispatcher response format}
	Result := Format(
		'{"email":"test@mail.ru","body":{"%s":[{"url":"%s"}]},"status":200}',
		[ShardType, URL]);
end;

class function TMockShardHelper.CreateDispatcherResponse(const Shards: array of TMockShard): WideString;
var
	ShardsJSON: WideString;
	i: Integer;
begin
	ShardsJSON := '';
	for i := Low(Shards) to High(Shards) do
	begin
		if i > Low(Shards) then
			ShardsJSON := ShardsJSON + ',';
		ShardsJSON := ShardsJSON + Format('"%s":[{"url":"%s"}]', [Shards[i].ShardType, Shards[i].URL]);
	end;

	Result := Format('{"email":"test@mail.ru","body":{%s},"status":200}', [ShardsJSON]);
end;

class function TMockShardHelper.CreateDownloadShardResponse(const URL: WideString): WideString;
begin
	Result := CreateShardResponse(SHARD_TYPE_GET, URL);
end;

class function TMockShardHelper.CreateUploadShardResponse(const URL: WideString): WideString;
begin
	Result := CreateShardResponse(SHARD_TYPE_UPLOAD, URL);
end;

class function TMockShardHelper.CreateVideoShardResponse(const URL: WideString): WideString;
begin
	Result := CreateShardResponse(SHARD_TYPE_VIDEO, URL);
end;

class function TMockShardHelper.CreateWeblinkGetShardResponse(const URL: WideString): WideString;
begin
	Result := CreateShardResponse(SHARD_TYPE_WEBLINK_GET, URL);
end;

class function TMockShardHelper.CreateWeblinkVideoShardResponse(const URL: WideString): WideString;
begin
	Result := CreateShardResponse(SHARD_TYPE_WEBLINK_VIDEO, URL);
end;

class function TMockShardHelper.CreateFullDispatcherResponse(
	const DownloadURL, UploadURL, VideoURL, WeblinkGetURL, WeblinkVideoURL: WideString): WideString;
var
	Shards: array[0..4] of TMockShard;
begin
	Shards[0] := TMockShard.Create(SHARD_TYPE_GET, DownloadURL);
	Shards[1] := TMockShard.Create(SHARD_TYPE_UPLOAD, UploadURL);
	Shards[2] := TMockShard.Create(SHARD_TYPE_VIDEO, VideoURL);
	Shards[3] := TMockShard.Create(SHARD_TYPE_WEBLINK_GET, WeblinkGetURL);
	Shards[4] := TMockShard.Create(SHARD_TYPE_WEBLINK_VIDEO, WeblinkVideoURL);
	Result := CreateDispatcherResponse(Shards);
end;

class function TMockShardHelper.CreateOAuthDownloadDispatcherResponse(const URL: WideString): WideString;
begin
	{OAuth dispatcher returns plain text: "URL IP COUNT"}
	Result := Format('%s 127.0.0.1 1', [URL]);
end;

class function TMockShardHelper.CreateOAuthUploadDispatcherResponse(const URL: WideString): WideString;
begin
	{OAuth dispatcher returns plain text: "URL IP COUNT"}
	Result := Format('%s 127.0.0.1 1', [URL]);
end;

end.

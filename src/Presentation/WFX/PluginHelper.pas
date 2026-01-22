unit PluginHelper;

{Plugin helper routines}
interface

uses
	CMRConstants,
	StringHelper,
	SysUtils;

{Re-export FormatSize from StringHelper for backwards compatibility}
function FormatSize(size: Int64; SizeType: Integer = SIZE_TYPE_AUTO): WideString;
function ShardTypeFromStreamingFormat(StreamingFormat: integer): string;

implementation

function FormatSize(size: Int64; SizeType: Integer = SIZE_TYPE_AUTO): WideString;
begin
	Result := StringHelper.FormatSize(size, SizeType);
end;

function ShardTypeFromStreamingFormat(StreamingFormat: integer): string;
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

end.

unit PluginHelper;

{Plugin helper routines}
interface

uses
	CMRConstants,
	SysUtils;

function FormatSize(size: Int64; SizeType: integer = TYPE_AUTO): WideString; //Форматируем размер в удобочитаемый вид
function ShardTypeFromStreamingFormat(StreamingFormat: integer): string;

implementation

function FormatSize(size: Int64; SizeType: integer = TYPE_AUTO): WideString; //Форматируем размер в удобочитаемый вид
const
	postfixes: array [0 .. 6] of string = ('b', 'kb', 'Mb', 'Gb', 'Tb', 'Pb', 'Eb');
var
	iteration: integer;
begin
	if TYPE_AUTO = SizeType then
	begin
		iteration := 0;
		while size > 1024 do
		begin
			iteration := iteration + 1;
			size := size div 1024;
		end;
		exit(Format('%d %s', [size, postfixes[iteration]]));
	end else begin
		iteration := 0;
		while iteration < SizeType do
		begin
			iteration := iteration + 1;
			size := size div 1024;
		end;
		exit(Format('%d %s', [size, postfixes[iteration + SizeType]]));
	end;

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

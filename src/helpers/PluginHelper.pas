unit PluginHelper;

{Plugin helper routines}
interface

uses
	CMRConstants,
	Math,
	SysUtils;

function FormatSize(size: Int64; SizeType: integer = TYPE_AUTO): WideString; //Форматируем размер в удобочитаемый вид
function ShardTypeFromStreamingFormat(StreamingFormat: integer): string;

implementation

function FormatSize(size: Int64; SizeType: integer = TYPE_AUTO): WideString; //Форматируем размер в удобочитаемый вид
const
	postfixes: array [0 .. 6] of string = ('b', 'kb', 'Mb', 'Gb', 'Tb', 'Pb', 'Eb');
var
	iteration: integer;
	floatSize: Double;
begin
	floatSize := size;
	iteration := 0;

	if SizeType = TYPE_AUTO then
	begin
		for iteration := 0 to Length(postfixes) - 1 do
		begin
			if floatSize < 1024 then
				Break;
			floatSize := floatSize / 1024;
		end;
	end
	else begin
		while iteration < Min(SizeType, Length(postfixes) - 1) do
		begin
			floatSize := floatSize / 1024;
			Inc(iteration);
		end;
	end;

	Result := Format('%d %s', [Round(floatSize), postfixes[iteration]]);
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

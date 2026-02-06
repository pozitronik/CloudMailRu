unit TimestampEntry;

{Value object representing a file timestamp entry.
	Stores the local file mtime at upload time and the cloud mtime at download time,
	enabling mtime preservation across upload/download cycles.}

interface

type
	TTimestampEntry = record
		LocalMTime: Int64;  {Local file mtime when uploaded (Unix timestamp)}
		CloudMTime: Int64;  {Cloud file mtime when last downloaded (0 = unknown)}

		{Returns an empty entry with all fields zeroed}
		class function Empty: TTimestampEntry; static;

		{Returns True if both fields are zero (no stored timestamp)}
		function IsEmpty: Boolean;
	end;

implementation

class function TTimestampEntry.Empty: TTimestampEntry;
begin
	Result.LocalMTime := 0;
	Result.CloudMTime := 0;
end;

function TTimestampEntry.IsEmpty: Boolean;
begin
	Result := (LocalMTime = 0) and (CloudMTime = 0);
end;

end.

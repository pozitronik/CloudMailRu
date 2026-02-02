unit ParsingHelper;

interface

uses
	System.SysUtils;

function extractPublicShard(Text: WideString; var Shard: WideString): Boolean;

implementation

function extractNearValue(Text, Anchor: WideString; StartChar: WideChar = '"'; EndChar: WideChar = '"'): WideString;
var
	start, end_: integer;
begin
	result := EmptyWideStr;

	Text := StringReplace(Text, #$A, EmptyWideStr, [rfReplaceAll]); {Makes text parsing easier}
	Text := StringReplace(Text, #$D, EmptyWideStr, [rfReplaceAll]);
	Text := StringReplace(Text, #9, EmptyWideStr, [rfReplaceAll]);
	Text := StringReplace(Text, #$20, EmptyWideStr, [rfReplaceAll]);
	start := Pos(WideString(Anchor), Text);
	if start > 0 then
	begin
		start := Pos(StartChar, Text, start + length(Anchor)) + 1;
		end_ := Pos(EndChar, Text, start);
		result := copy(Text, start, end_ - start);
	end;
end;

function extractPublicShard(Text: WideString; var Shard: WideString): Boolean;
begin
	Shard := extractNearValue(Text, '"weblink_get":', '{', '}');
	Shard := extractNearValue(Shard, '"url":');
	result := EmptyWideStr <> Shard;
end;

end.

unit ParsingHelper;

interface

uses
	System.SysUtils;

function extractNearValue(Text, Anchor: WideString; StartChar: WideChar = '"'; EndChar: WideChar = '"'): WideString;
function extractPublicShard(Text: WideString; var Shard: WideString): Boolean;

function ExtractEmailParts(const Email: WideString; out Username, Domain: WideString): Boolean;

implementation

function extractNearValue(Text, Anchor: WideString; StartChar: WideChar = '"'; EndChar: WideChar = '"'): WideString;
var
	start, end_: integer;
begin
	result := EmptyWideStr;

	Text := StringReplace(Text, #$A, EmptyWideStr, [rfReplaceAll]); //так нам проще ковыряться в тексте
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

function ExtractEmailParts(const Email: WideString; out Username, Domain: WideString): Boolean;
var
	AtPos: integer;
begin
	AtPos := Pos('@', Email);
	result := (AtPos > 0) and (AtPos < length(Email));
	if result then
	begin
		{Copy with position 0 is valid in Delphi - verified by tests}
		Username := copy(Email, 0, AtPos - 1);
		Domain := copy(Email, AtPos + 1, length(Email) - AtPos);
	end;
end;

end.

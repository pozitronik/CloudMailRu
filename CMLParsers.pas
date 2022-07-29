unit CMLParsers;

interface

uses CMLTypes, System.SysUtils, System.StrUtils;

function extractNearValue(Text, Anchor: WideString; StartChar: WideChar = '"'; EndChar: WideChar = '"'): WideString;
function extractTokenFromText(Text: WideString; var token: WideString): Boolean;
function extractPublicTokenFromText(Text: WideString; var PublicToken: WideString): Boolean;
function extract_x_page_id_FromText(Text: WideString; var PageId: WideString): Boolean;
function extract_build_FromText(Text: WideString; var build: WideString): Boolean;
function extract_upload_url_FromText(Text: WideString; var UploadUrl: WideString): Boolean;
function extractPublicShard(Text: WideString; var Shard: WideString): Boolean;
function extractTwostepJson(Text: WideString; var JSON: WideString): Boolean;

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

function extractPublicTokenFromText(Text: WideString; var PublicToken: WideString): Boolean;
begin
	PublicToken := extractNearValue(Text, '"tokens":{"download":');
	result := EmptyWideStr <> PublicToken;
end;

function extract_x_page_id_FromText(Text: WideString; var PageId: WideString): Boolean;
begin
	PageId := extractNearValue(Text, '"x-page-id"');
	result := PageId <> EmptyWideStr;
end;

function extract_build_FromText(Text: WideString; var build: WideString): Boolean;
begin
	build := extractNearValue(Text, '"BUILD"');
	result := build <> EmptyWideStr;
end;

function extract_upload_url_FromText(Text: WideString; var UploadUrl: WideString): Boolean;
var
	start, start1, start2, finish, length: Cardinal;
	temp: WideString;
begin
	result := false;
	start := Pos(WideString('mail.ru/upload/"'), Text);
	if start > 0 then
	begin
		start1 := start - 50;
		finish := start + 15;
		length := finish - start1;
		temp := copy(Text, start1, length);
		start2 := Pos(WideString('https://'), temp);
		UploadUrl := copy(temp, start2, Strlen(PWideChar(temp)) - start2);
		result := true;
	end;
end;

function extractPublicShard(Text: WideString; var Shard: WideString): Boolean;
begin
	Shard := extractNearValue(Text, '"weblink_get":', '{', '}');
	Shard := extractNearValue(Shard, '"url":');
	result := EmptyWideStr <> Shard;
end;

function extractTokenFromText(Text: WideString; var token: WideString): Boolean;
begin
	token := extractNearValue(Text, '"csrf"');
	result := token <> EmptyWideStr;
end;

function extractTwostepJson(Text: WideString; var JSON: WideString): Boolean;
var
	start, finish: integer;
begin
	result := false;
	start := Pos(WideString('<script type="text/html" id="json">'), Text);
	finish := PosEx(WideString('</script>'), Text, start);
	if (start > 0) and (finish > 0) then
	begin
		JSON := copy(Text, start + 35, finish - start - 35);

		result := true;
	end;
end;

end.

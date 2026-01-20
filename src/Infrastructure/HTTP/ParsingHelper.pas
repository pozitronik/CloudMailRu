unit ParsingHelper;

interface

uses
	CMRConstants,
	System.SysUtils,
	System.StrUtils,
	System.Math;

function extractNearValue(Text, Anchor: WideString; StartChar: WideChar = '"'; EndChar: WideChar = '"'): WideString;
function extractTokenFromText(Text: WideString; var token: WideString): Boolean;
function extractPublicTokenFromText(Text: WideString; var PublicToken: WideString): Boolean;
function extract_x_page_id_FromText(Text: WideString; var PageId: WideString): Boolean;
function extract_build_FromText(Text: WideString; var build: WideString): Boolean;
function extract_upload_url_FromText(Text: WideString; var UploadUrl: WideString): Boolean;
function extractPublicShard(Text: WideString; var Shard: WideString): Boolean;
function extractTwostepJson(Text: WideString; var JSON: WideString): Boolean;

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
const
	URL_SEARCH_WINDOW = 50;
	PATTERN = 'mail.ru/upload/"';
	HTTPS_PREFIX = 'https://';
var
	PatternPos, WindowStart, WindowEnd, HttpsPos: Integer;
	SearchWindow: WideString;
begin
	result := false;
	UploadUrl := EmptyWideStr;

	PatternPos := Pos(WideString(PATTERN), Text);
	if PatternPos <= 0 then
		Exit;

	{ Extract a window of text that should contain the full URL }
	WindowStart := Max(1, PatternPos - URL_SEARCH_WINDOW);
	WindowEnd := PatternPos + Length(PATTERN) - 1;
	SearchWindow := Copy(Text, WindowStart, WindowEnd - WindowStart + 1);

	{ Find https:// within the window }
	HttpsPos := Pos(WideString(HTTPS_PREFIX), SearchWindow);
	if HttpsPos <= 0 then
		Exit;

	{ Extract URL from https:// to the end of the window }
	UploadUrl := Copy(SearchWindow, HttpsPos, Length(SearchWindow) - HttpsPos + 1);
	result := true;
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

function ExtractEmailParts(const Email: WideString; out Username, Domain: WideString): Boolean;
var
	AtPos: integer;
begin
	AtPos := Pos('@', Email);
	result := (AtPos > 0) and (AtPos < length(Email));
	if result then
	begin
		Username := copy(Email, 0, AtPos - 1);
		Domain := copy(Email, AtPos + 1, length(Email) - AtPos);
	end;
end;

end.

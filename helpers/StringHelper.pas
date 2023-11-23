unit StringHelper;

{String manipulation helper routines}
interface

uses
	SysUtils,
	Classes;

function GetWord(command: WideString; WordIndex: integer = 0): WideString; //Возвращает указанное значащее слово из строки с учётом кавычек (парсинг команд)
function ExtractLinkFromUrl(URL: WideString): WideString; //При необходимости преобразует адрес публичной ссылки к нужному виду
function Implode(S: TStringList; Delimiter: WideString): WideString;
function Explode(S: WideString; Delimiter: char): TStringList;
function MyExtractStrings(Separators, WhiteSpace: TSysCharSet; Content: PWideChar; Strings: TStrings): integer;
function TrimEx(const Str: WideString; TrimChar: WideChar): WideString;
function PosLast(Substring, S: WideString; Offset: integer = 0): integer;
function UrlEncode(URL: WideString): WideString;

implementation

function Implode(S: TStringList; Delimiter: WideString): WideString;
var
	iCount: integer;
begin
	Result := EmptyWideStr;
	if (S.Count = 0) then
		exit;
	for iCount := 0 to pred(S.Count) do
		Result := Result + S.Strings[iCount] + Delimiter;
	System.Delete(Result, Length(Result), 1);
end;

function Explode(S: WideString; Delimiter: char): TStringList;
begin
	Result := TStringList.Create;
	Result.DelimitedText := S;
	Result.Delimiter := Delimiter;
end;

function MyExtractStrings(Separators, WhiteSpace: TSysCharSet; Content: PWideChar; Strings: TStrings): integer;
var
	Head, Tail: PWideChar;
	EOS, InQuote: boolean;
	QuoteChar: WideChar;
	Item: WideString;
begin
	Result := 0;
	if (Content = nil) or (Content^ = #0) or (Strings = nil) then
		exit;
	Tail := Content;
	InQuote := False;
	QuoteChar := #0;
	Strings.BeginUpdate;
	try
		Include(WhiteSpace, #13);
		Include(WhiteSpace, #10);

		Include(Separators, #0);
		Include(Separators, #13);
		Include(Separators, #10);
		//Include(Separators, '''');
		Include(Separators, '"');
		repeat
			while CharInSet(Tail^, WhiteSpace) do
				Inc(Tail);

			Head := Tail;
			while true do
			begin
				while (InQuote and not((Tail^ = #0) or (Tail^ = QuoteChar))) or not(CharInSet(Tail^, Separators)) do
					Inc(Tail);
				if CharInSet(Tail^, ['''', '"']) then
				begin
					if (QuoteChar <> #0) and (QuoteChar = Tail^) then
						QuoteChar := #0
					else if QuoteChar = #0 then
						QuoteChar := Tail^;
					InQuote := QuoteChar <> #0;
					Inc(Tail);
				end
				else
					Break;
			end;
			EOS := Tail^ = #0;
			if (Head <> Tail) and (Head^ <> #0) then
			begin
				if Strings <> nil then
				begin
					SetString(Item, Head, Tail - Head);
					Strings.Add(Item);
				end;
				Inc(Result);
			end;
			Inc(Tail);
		until EOS;
	finally
		Strings.EndUpdate;
	end;
end;

function TrimEx(const Str: WideString; TrimChar: WideChar): WideString;
var
	S, E: integer;
begin
	S := 1;
	while (S <= Length(Str)) and (Str[S] = TrimChar) do
		Inc(S);
	E := Length(Str);
	while (E >= 1) and (Str[E] = TrimChar) do
		Dec(E);
	SetString(Result, PChar(@Str[S]), E - S + 1);
end;

function PosLast(Substring, S: WideString; Offset: integer = 0): integer;
var
	tmp: integer;
begin
	tmp := Offset;
	Repeat
		Result := tmp;
		tmp := Pos(Substring, S, tmp + 1);
	until tmp = 0;
end;

function ExtractLinkFromUrl(URL: WideString): WideString; //При необходимости преобразует адрес публичной ссылки к нужному виду
const
	pulicPrefix = 'https://cloud.mail.ru/public';
begin
	Result := URL;
	if Pos(WideString(pulicPrefix), URL) <> 0 then
		Result := Copy(URL, Length(pulicPrefix) + 1, Length(URL) - Length(pulicPrefix));
end;

function GetWord(command: WideString; WordIndex: integer = 0): WideString;
var
	Exploded: TStringList;
begin
	Result := EmptyWideStr;
	Exploded := Explode(command, ' ');
	if Exploded.Count = 0 then
		exit;
	if Exploded.Count <= WordIndex then
		exit;
	Result := Exploded.Strings[WordIndex];
end;

function UrlEncode(URL: WideString): WideString;
var
	I: integer;
	UTF8: UTF8String;
begin
	UTF8 := UTF8String(URL);
	Result := EmptyWideStr;
	for I := 1 to Length(UTF8) do
		if UTF8[I] in ['a' .. 'z', 'A' .. 'Z', '/', '_', '-', '.', '0' .. '9'] then
			Result := Result + WideString(UTF8[I])
		else
			Result := Result + '%' + IntToHex(Ord(UTF8[I]), 2);
end;

end.

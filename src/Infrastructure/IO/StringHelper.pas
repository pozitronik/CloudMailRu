unit StringHelper;

{String manipulation helper routines}
interface

uses
	SysUtils,
	Math;

const
	SIZE_TYPE_AUTO = -1;

function FormatSize(size: Int64; SizeType: Integer = SIZE_TYPE_AUTO): WideString;
function UrlEncode(URL: WideString): WideString;

implementation

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

function FormatSize(size: Int64; SizeType: Integer = SIZE_TYPE_AUTO): WideString;
const
	Postfixes: array [0 .. 6] of string = ('b', 'kb', 'Mb', 'Gb', 'Tb', 'Pb', 'Eb');
var
	Iteration: Integer;
	FloatSize: Double;
begin
	FloatSize := size;
	Iteration := 0;

	if SizeType = SIZE_TYPE_AUTO then
	begin
		for Iteration := 0 to Length(Postfixes) - 1 do
		begin
			if FloatSize < 1024 then
				Break;
			FloatSize := FloatSize / 1024;
		end;
	end else begin
		while Iteration < Min(SizeType, Length(Postfixes) - 1) do
		begin
			FloatSize := FloatSize / 1024;
			Inc(Iteration);
		end;
	end;

	Result := Format('%d %s', [Round(FloatSize), Postfixes[Iteration]]);
end;

end.

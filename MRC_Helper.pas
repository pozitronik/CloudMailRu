unit MRC_Helper;

interface

uses Classes;

type
	TRealPath = record
		account: WideString;
		path: WideString;
	end;

function Implode(S: TStringList; Delimiter: Char): WideString;
function ExtractRealPath(VirtualPath: WideString): TRealPath;
function DateTimeToUnix(ConvDate: TDateTime): Integer;
function CheckFlag(Check: Byte; Flags: Integer): Boolean; // Определяет, установлен ли указанный бит

implementation

function Implode(S: TStringList; Delimiter: Char): WideString;
var
	iCount: Integer;
begin
	Result := '';
	if (S.Count = 0) then exit;
	for iCount := 0 to pred(S.Count) do Result := Result + S.Strings[iCount] + Delimiter;
	System.Delete(Result, Length(Result), 1);
end;

function ExtractRealPath(VirtualPath: WideString): TRealPath;
var
	List: TStringList;
begin
	List := TStringList.Create;
	ExtractStrings(['\'], [], PWideChar(VirtualPath), List);
	if List.Count = 1 then
	begin // в виртуальной ФС это каталог первого уровня
		Result.account := '';
		Result.path := '';
	end else begin
		Result.account := List.Strings[0];
		List.Delete(0);

		Result.path := Implode(List, '\');
		if Result.path = '' then ExtractRealPath.path := '\';
	end;

	List.Destroy;
end;

function DateTimeToUnix(ConvDate: TDateTime): Integer;
const
	UnixStartDate: TDateTime = 25569.0;
begin
	// example: DateTimeToUnix(now);
	Result := Round((ConvDate - UnixStartDate) * 86400);
end;

function CheckFlag(Check: Byte; Flags: LongInt): Boolean; // Определяет, установлен ли указанный бит
begin
	Result := (Flags and Check) <> 0;
end;

end.

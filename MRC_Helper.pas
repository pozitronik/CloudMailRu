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

implementation

function Implode(S: TStringList; Delimiter: Char): WideString;
var
	iCount: integer;
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
	Result.account := List.Strings[0];
	List.Delete(0);

	Result.path := Implode(List, '\');
	if Result.path = '' then ExtractRealPath.path := '\';

	List.Destroy;
end;

function DateTimeToUnix(ConvDate: TDateTime): Integer;
const
	UnixStartDate: TDateTime = 25569.0;
begin
	// example: DateTimeToUnix(now);
	Result := Round((ConvDate - UnixStartDate) * 86400);
end;

end.

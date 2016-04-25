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
	result.account := List.Strings[0];
	List.Delete(0);

	result.path := Implode(List, '\');
	if result.path = '' then ExtractRealPath.path := '\';

	List.Free;
end;

end.

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

function Implode(S: TStringList; Delimiter: Char): WideString; // todo helper
var
	iCount: integer;
begin
	Result := '';
	if (S.Count = 0) then exit;
	for iCount := 0 to pred(S.Count) do Result := Result + S.Strings[iCount] + Delimiter;
	System.Delete(Result, Length(Result), 1);
end;

function ExtractRealPath(VirtualPath: WideString): TRealPath; // todo helper
var
	List: TStringList;
begin
	List := TStringList.Create;
	ExtractStrings(['\'], [], PWideChar(VirtualPath), List);
	ExtractRealPath.account := List.Strings[0];
	List.Delete(0);

	ExtractRealPath.path := Implode(List, '\');
	List.Free;
end;

end.

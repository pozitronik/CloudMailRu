unit AssociativeArray;

interface

uses
	SysUtils, Classes, System.Types;

type

	TAssociativeArray = record
	private
		FItems: array of DWORD;
		function GetCount: integer; inline;
		function GetItem(index: integer): DWORD;
		procedure SetItem(index: integer; const Value: DWORD);
	public
		function Add(const Value: DWORD): integer;
		function Delete(index: integer): boolean;
		procedure Clear;
		function IndexOf(const Value: DWORD): integer;
		property Count: integer read GetCount;
		property Items[index: integer]: DWORD read GetItem write SetItem; default;
	end;

implementation

{TAssociativeArray}

function TAssociativeArray.Add(const Value: DWORD): integer;
begin
	SetLength(FItems, Count + 1);
	FItems[Count - 1] := Value;
	result := Count - 1;
end;

procedure TAssociativeArray.Clear;
begin
	SetLength(FItems, 0);
end;

function TAssociativeArray.Delete(index: integer): boolean;
var
	k: integer;
begin
	if (Index < 0) or (Index >= Count) then exit(false);

	for k := Index to Count - 2{2nd last element} do
	begin
		FItems[k] := FItems[k + 1];
	end;

	SetLength(FItems, Count - 1);
end;

function TAssociativeArray.GetCount: integer;
begin
	result := length(FItems);
end;

function TAssociativeArray.GetItem(index: integer): DWORD;
begin
	result := FItems[index];
end;

function TAssociativeArray.IndexOf(const Value: DWORD): integer;
var
	k: integer;
begin
	result := -1;
	for k := 0 to Count - 1 do
	begin
		if FItems[k] = Value then
		begin
			result := k;
			break;
		end;
	end;
end;

procedure TAssociativeArray.SetItem(index: integer; const Value: DWORD);
begin
	FItems[index] := Value;
end;

end.

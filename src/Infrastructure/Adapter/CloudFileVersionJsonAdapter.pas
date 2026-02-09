unit CloudFileVersionJsonAdapter;

{Parses file/history API JSON response into TCloudFileVersionList.
	Handles both paid (with hash/rev) and free (without) account responses.}

interface

uses
	CloudFileVersion;

type
	TCloudFileVersionJsonAdapter = class
	public
		class function Parse(const JSON: WideString; out Versions: TCloudFileVersionList): Boolean; static;
	end;

implementation

uses
	SafeJSON;

class function TCloudFileVersionJsonAdapter.Parse(const JSON: WideString; out Versions: TCloudFileVersionList): Boolean;
var
	Root, Body, Item: TSafeJSON;
	I, Count: Integer;
begin
	Result := False;
	SetLength(Versions, 0);

	Root := TSafeJSON.Parse(JSON);
	try
		if Root.IsNull then
			Exit;

		Body := Root.Get('body');
		if Body.IsNull or not Body.IsArray then
			Exit;

		Count := Body.Count;
		SetLength(Versions, Count);

		for I := 0 to Count - 1 do
		begin
			Item := Body.Item(I);
			Versions[I].Hash := Item.Get('hash').AsString;
			Versions[I].Name := Item.Get('name').AsString;
			Versions[I].Path := Item.Get('path').AsString;
			Versions[I].Size := Item.Get('size').AsInt64;
			Versions[I].Time := Item.Get('time').AsInt64;
			Versions[I].Rev := Item.Get('rev').AsInt;
			Versions[I].UID := Item.Get('uid').AsInt;
		end;

		Result := True;
	finally
		Root.Free;
	end;
end;

end.

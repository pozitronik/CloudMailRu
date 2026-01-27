unit CloudInviteListJsonAdapter;

{Adapter to parse JSON into TCloudInviteList arrays.
	Separates JSON parsing (infrastructure concern) from the domain object.}

interface

uses
	CloudInviteList;

type
	TCloudInviteListJsonAdapter = class
	public
		class function Parse(const JSON: WideString; var List: TCloudInviteList): Boolean; static;
	end;

implementation

uses
	CloudConstants,
	SafeJSON;

class function TCloudInviteListJsonAdapter.Parse(const JSON: WideString; var List: TCloudInviteList): Boolean;
var
	Root, Invited, Item: TSafeJSON;
	I: Integer;
begin
	Result := False;
	SetLength(List, 0);

	Root := TSafeJSON.Parse(JSON);
	try
		if Root.IsNull then
			Exit;

		Invited := Root.Get(NAME_BODY).Get(NAME_INVITED);

		{No invites array is a valid state - return success with empty list}
		if Invited.IsNull then
			Exit(True);

		if not Invited.IsArray then
			Exit(True);

		SetLength(List, Invited.Count);
		for I := 0 to Invited.Count - 1 do
		begin
			Item := Invited.Item(I);
			List[I].email := Item.Get(NAME_EMAIL).AsString;
			List[I].status := Item.Get(NAME_STATUS).AsString;
			List[I].access := Item.Get(NAME_ACCESS).AsString;
			List[I].name := Item.Get(NAME_NAME).AsString;
		end;

		Result := True;
	finally
		Root.Free;
	end;
end;

end.

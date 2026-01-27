unit CloudIncomingInviteListJsonAdapter;

{Adapter to parse JSON into TCloudIncomingInviteList arrays.
	Separates JSON parsing (infrastructure concern) from the domain object.}

interface

uses
	CloudIncomingInviteList;

type
	TCloudIncomingInviteListJsonAdapter = class
	public
		class function Parse(const JSON: WideString; var List: TCloudIncomingInviteList): Boolean; static;
	end;

implementation

uses
	CloudConstants,
	SafeJSON;

class function TCloudIncomingInviteListJsonAdapter.Parse(const JSON: WideString; var List: TCloudIncomingInviteList): Boolean;
var
	Root, ListArray, Item, Owner: TSafeJSON;
	I: Integer;
begin
	Result := False;
	SetLength(List, 0);

	Root := TSafeJSON.Parse(JSON);
	try
		if Root.IsNull then
			Exit;

		ListArray := Root.Get(NAME_BODY).Get(NAME_LIST);

		{No list array means failure for incoming invites}
		if ListArray.IsNull or not ListArray.IsArray then
			Exit;

		SetLength(List, ListArray.Count);
		for I := 0 to ListArray.Count - 1 do
		begin
			Item := ListArray.Item(I);

			{Parse nested owner object - TSafeJSON handles missing/null gracefully}
			Owner := Item.Get(NAME_OWNER);
			List[I].owner.email := Owner.Get(NAME_EMAIL).AsString;
			List[I].owner.name := Owner.Get(NAME_NAME).AsString;

			List[I].tree := Item.Get(NAME_TREE).AsString;
			List[I].access := Item.Get(NAME_ACCESS).AsString;
			List[I].name := Item.Get(NAME_NAME).AsString;
			List[I].home := Item.Get(NAME_HOME).AsString;
			List[I].size := Item.Get(NAME_SIZE).AsInt64;
			List[I].invite_token := Item.Get(NAME_INVITE_TOKEN).AsString;
		end;

		Result := True;
	finally
		Root.Free;
	end;
end;

end.

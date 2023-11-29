unit WSList;

interface

type
	{A simple array-like type to replace TStrings}
	TWSList = TArray<WideString>;

	TWSHelper = record helper for TWSList
		procedure Add(S: WideString);
		procedure Clear();
		function Count: Integer;
		function Contains(S: WideString): Boolean;
	end;

implementation

{TWSHelper}

procedure TWSHelper.Add(S: WideString);
begin
	SetLength(self, Count + 1);
	self[Count - 1] := S;
end;

procedure TWSHelper.Clear;
begin
	self := [];
end;

function TWSHelper.Contains(S: WideString): Boolean;
var
	Item: WideString;
begin
	Result := False;
	for Item in self do
	begin
		if Item = S then
		begin
			Result := True;
			Exit;
		end;
	end;
end;

function TWSHelper.Count: Integer;
begin
	Result := Length(self);
end;

end.

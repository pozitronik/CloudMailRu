unit WSList;

interface

type
	{A simple array-like type to replace TStrings}
	TWSList = TArray<WideString>;

	TWSHelper = record helper for TWSList

		procedure Add(S: WideString);
		function Count: Integer;
	end;

implementation

{TWSHelper}

procedure TWSHelper.Add(S: WideString);
begin
	SetLength(self, Count + 1);
	self[Count] := S;
end;

function TWSHelper.Count: Integer;
begin
	result := Length(self);
end;

end.

unit TestHelper;

interface

function RandomString(const Len: Integer): WideString;

implementation

function RandomString(const Len: Integer): WideString;
const
	CharSet = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
var
	i: Integer;
begin
	Result := '';
	Randomize; // Initialize the random number generator

	for i := 1 to Len do
		Result := Result + CharSet[Random(Length(CharSet)) + 1];
end;

end.

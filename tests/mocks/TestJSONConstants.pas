unit TestJSONConstants;

{Shared JSON response constants used across multiple test fixtures.
	Centralizes commonly duplicated API response strings.}

interface

const
	{Standard API success response with empty body}
	JSON_SUCCESS =
		'{"email":"test@mail.ru","body":{},"status":200}';

	{API failure response -- item not found}
	JSON_FAILURE =
		'{"email":"test@mail.ru","body":{"home":{"error":"not_exists"}},"status":400}';

	{Empty directory listing}
	JSON_EMPTY_LISTING =
		'{"email":"test@mail.ru","body":{"list":[]},"status":200}';

implementation

end.

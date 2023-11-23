﻿unit CloudMailRuOAuthInfo;

interface

type
	TCloudMailRuOAuthInfo = Record
		error: WideString;
		error_code: integer;
		error_description: WideString;
		expires_in: integer;
		refresh_token: WideString;
		access_token: WideString;
	end;

implementation

end.

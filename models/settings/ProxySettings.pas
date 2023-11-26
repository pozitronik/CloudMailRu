unit ProxySettings;

interface

type
	TProxySettings = record
		ProxyType: integer;
		Server: WideString;
		Port: integer;
		user: WideString;
		password: WideString;
		use_tc_password_manager: boolean;
	end;

implementation

end.

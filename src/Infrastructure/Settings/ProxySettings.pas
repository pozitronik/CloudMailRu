unit ProxySettings;

interface

type
	TProxySettings = record
		ProxyType: integer;
		Server: WideString;
		Port: integer;
		User: WideString;
		Password: WideString;
		UseTCPasswordManager: boolean;
	end;

implementation

end.

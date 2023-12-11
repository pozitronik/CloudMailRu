unit ConnectionSettings;

interface

uses
	ProxySettings;

type
	{Settings for HTTP transport}
	TConnectionSettings = record
		ProxySettings: TProxySettings;
		SocketTimeout: integer;
		UploadBPS: integer;
		DownloadBPS: integer;
		UserAgent: WideString;
	end;

implementation

end.

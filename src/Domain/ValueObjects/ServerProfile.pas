unit ServerProfile;

{Server profile defines a named set of endpoints for a cloud service instance.
	The built-in default profile targets cloud.mail.ru.
	Custom profiles allow connecting to self-hosted API-compatible servers.}

interface

uses
	CloudEndpoints;

type
	TServerProfile = record
		Name: WideString; {Display name, also serves as unique identifier}
		ServerUrl: WideString; {Base URL for self-configure (e.g., 'http://localhost:8080')}
		Endpoints: TCloudEndpoints; {Resolved endpoint URLs}
		class function CreateDefault: TServerProfile; static;
	end;

implementation

class function TServerProfile.CreateDefault: TServerProfile;
begin
	Result := Default(TServerProfile);
	Result.Name := '';
	Result.ServerUrl := '';
	Result.Endpoints := TCloudEndpoints.CreateDefaults;
end;

end.

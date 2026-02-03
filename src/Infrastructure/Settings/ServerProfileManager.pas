unit ServerProfileManager;

{Manages server profiles stored as [Server:Name] sections in the plugin INI file.
	Handles CRUD operations, endpoint resolution with priority-based merging,
	and self-configure protocol support.}

interface

uses
	ServerProfile,
	CloudEndpoints,
	ConfigFile,
	WSList;

const
	SERVER_SECTION_PREFIX = 'Server:';

type
	IServerProfileManager = interface
		['{F4A2B8C1-3D5E-4F6A-9B0C-1D2E3F4A5B6C}']
		{Returns list of all server profile names}
		function GetProfileNames: TWSList;
		{Returns server profile by name. Returns default profile if name is empty or not found.}
		function GetProfile(const Name: WideString): TServerProfile;
		{Saves server profile to INI. Creates new section or updates existing one.}
		procedure SetProfile(const Profile: TServerProfile);
		{Deletes server profile by name}
		procedure DeleteProfile(const Name: WideString);
		{Renames a server profile, preserving all settings}
		procedure RenameProfile(const OldName, NewName: WideString);
		{Resolves endpoints for an account given its server profile name.
			Empty name returns cloud.mail.ru defaults.}
		function ResolveEndpoints(const ServerName: WideString): TCloudEndpoints;
	end;

	TServerProfileManager = class(TInterfacedObject, IServerProfileManager)
	private
		FConfigFile: IConfigFile;
		function SectionName(const ProfileName: WideString): WideString;
	public
		constructor Create(ConfigFile: IConfigFile);
		function GetProfileNames: TWSList;
		function GetProfile(const Name: WideString): TServerProfile;
		procedure SetProfile(const Profile: TServerProfile);
		procedure DeleteProfile(const Name: WideString);
		procedure RenameProfile(const OldName, NewName: WideString);
		function ResolveEndpoints(const ServerName: WideString): TCloudEndpoints;
		class function InferEndpointsFromServerUrl(const ServerUrl: WideString): TCloudEndpoints; static;
	end;

	{Null implementation for testing -- always returns defaults}
	TNullServerProfileManager = class(TInterfacedObject, IServerProfileManager)
	public
		function GetProfileNames: TWSList;
		function GetProfile(const Name: WideString): TServerProfile;
		procedure SetProfile(const Profile: TServerProfile);
		procedure DeleteProfile(const Name: WideString);
		procedure RenameProfile(const OldName, NewName: WideString);
		function ResolveEndpoints(const ServerName: WideString): TCloudEndpoints;
	end;

implementation

uses
	SysUtils,
	StrUtils,
	Classes,
	PathHelper;

{TServerProfileManager}

constructor TServerProfileManager.Create(ConfigFile: IConfigFile);
begin
	FConfigFile := ConfigFile;
end;

function TServerProfileManager.SectionName(const ProfileName: WideString): WideString;
begin
	Result := SERVER_SECTION_PREFIX + ProfileName;
end;

function TServerProfileManager.GetProfileNames: TWSList;
var
	Sections: TStringList;
	I: Integer;
	Section: string;
begin
	Result.Clear;
	Sections := TStringList.Create;
	try
		FConfigFile.ReadSections(Sections);
		for I := 0 to Sections.Count - 1 do
		begin
			Section := Sections[I];
			if StartsText(SERVER_SECTION_PREFIX, Section) then
				Result.Add(Copy(Section, Length(SERVER_SECTION_PREFIX) + 1, MaxInt));
		end;
	finally
		Sections.Free;
	end;
end;

function TServerProfileManager.GetProfile(const Name: WideString): TServerProfile;
var
	Section: WideString;
begin
	if Name = '' then
		Exit(TServerProfile.CreateDefault);

	Section := SectionName(Name);
	if not FConfigFile.SectionExists(Section) then
		Exit(TServerProfile.CreateDefault);

	Result := Default(TServerProfile);
	Result.Name := Name;
	Result.ServerUrl := FConfigFile.ReadString(Section, 'server_url', '');

	{Read endpoint overrides -- start from defaults, then apply stored values}
	Result.Endpoints := TCloudEndpoints.CreateDefaults;

	{If server_url is set, infer base defaults from it first}
	if Result.ServerUrl <> '' then
		Result.Endpoints := InferEndpointsFromServerUrl(Result.ServerUrl);

	{Then apply any explicit endpoint overrides from INI}
	var Value: WideString;

	Value := FConfigFile.ReadString(Section, 'api_url', '');
	if Value <> '' then
		Result.Endpoints.ApiBase := Value;

	Value := FConfigFile.ReadString(Section, 'oauth_url', '');
	if Value <> '' then
		Result.Endpoints.OAuthUrl := Value;

	Value := FConfigFile.ReadString(Section, 'dispatcher_url', '');
	if Value <> '' then
		Result.Endpoints.DispatcherUrl := Value;

	Value := FConfigFile.ReadString(Section, 'thumbnail_url', '');
	if Value <> '' then
		Result.Endpoints.ThumbnailUrl := Value;

	Value := FConfigFile.ReadString(Section, 'public_url', '');
	if Value <> '' then
		Result.Endpoints.PublicUrl := Value;

	Value := FConfigFile.ReadString(Section, 'download_url', '');
	if Value <> '' then
		Result.Endpoints.DownloadUrl := Value;

	Value := FConfigFile.ReadString(Section, 'upload_url', '');
	if Value <> '' then
		Result.Endpoints.UploadUrl := Value;
end;

procedure TServerProfileManager.SetProfile(const Profile: TServerProfile);
var
	Section: WideString;
	Defaults: TCloudEndpoints;
begin
	Section := SectionName(Profile.Name);
	Defaults := TCloudEndpoints.CreateDefaults;

	FConfigFile.WriteStringIfNotDefault(Section, 'server_url', Profile.ServerUrl, '');
	FConfigFile.WriteStringIfNotDefault(Section, 'api_url', Profile.Endpoints.ApiBase, Defaults.ApiBase);
	FConfigFile.WriteStringIfNotDefault(Section, 'oauth_url', Profile.Endpoints.OAuthUrl, Defaults.OAuthUrl);
	FConfigFile.WriteStringIfNotDefault(Section, 'dispatcher_url', Profile.Endpoints.DispatcherUrl, Defaults.DispatcherUrl);
	FConfigFile.WriteStringIfNotDefault(Section, 'thumbnail_url', Profile.Endpoints.ThumbnailUrl, Defaults.ThumbnailUrl);
	FConfigFile.WriteStringIfNotDefault(Section, 'public_url', Profile.Endpoints.PublicUrl, Defaults.PublicUrl);
	FConfigFile.WriteStringIfNotDefault(Section, 'download_url', Profile.Endpoints.DownloadUrl, '');
	FConfigFile.WriteStringIfNotDefault(Section, 'upload_url', Profile.Endpoints.UploadUrl, '');
end;

procedure TServerProfileManager.DeleteProfile(const Name: WideString);
begin
	if Name <> '' then
		FConfigFile.EraseSection(SectionName(Name));
end;

procedure TServerProfileManager.RenameProfile(const OldName, NewName: WideString);
var
	Profile: TServerProfile;
begin
	if (OldName = NewName) or (OldName = '') or (NewName = '') then
		Exit;

	Profile := GetProfile(OldName);
	Profile.Name := NewName;
	SetProfile(Profile);
	DeleteProfile(OldName);
end;

function TServerProfileManager.ResolveEndpoints(const ServerName: WideString): TCloudEndpoints;
var
	Profile: TServerProfile;
begin
	if ServerName = '' then
		Exit(TCloudEndpoints.CreateDefaults);

	Profile := GetProfile(ServerName);
	Result := Profile.Endpoints;
end;

class function TServerProfileManager.InferEndpointsFromServerUrl(const ServerUrl: WideString): TCloudEndpoints;
var
	BaseUrl: WideString;
begin
	Result := TCloudEndpoints.CreateDefaults;
	if ServerUrl = '' then
		Exit;

	{Strip trailing slash for consistent concatenation}
	BaseUrl := ServerUrl;
	if (Length(BaseUrl) > 0) and (BaseUrl[Length(BaseUrl)] = '/') then
		Delete(BaseUrl, Length(BaseUrl), 1);

	Result.ApiBase := BaseUrl + '/api/v2';
	Result.OAuthUrl := BaseUrl + '/token';
	Result.DispatcherUrl := BaseUrl + '/dispatcher';
	Result.ThumbnailUrl := BaseUrl + '/thumb';
	Result.PublicUrl := BaseUrl + '/public/';
end;

{TNullServerProfileManager}

function TNullServerProfileManager.GetProfileNames: TWSList;
begin
	Result.Clear;
end;

function TNullServerProfileManager.GetProfile(const Name: WideString): TServerProfile;
begin
	Result := TServerProfile.CreateDefault;
end;

procedure TNullServerProfileManager.SetProfile(const Profile: TServerProfile);
begin
	{No-op}
end;

procedure TNullServerProfileManager.DeleteProfile(const Name: WideString);
begin
	{No-op}
end;

procedure TNullServerProfileManager.RenameProfile(const OldName, NewName: WideString);
begin
	{No-op}
end;

function TNullServerProfileManager.ResolveEndpoints(const ServerName: WideString): TCloudEndpoints;
begin
	Result := TCloudEndpoints.CreateDefaults;
end;

end.

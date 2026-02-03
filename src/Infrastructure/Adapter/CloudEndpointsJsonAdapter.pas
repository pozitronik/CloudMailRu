unit CloudEndpointsJsonAdapter;

{Parses self-configure JSON response into TCloudEndpoints.
	Only non-empty fields from the JSON override corresponding endpoint values;
	missing or empty fields are left unchanged.}

interface

uses
	CloudEndpoints;

type
	TCloudEndpointsJsonAdapter = class
	public
		{Merges self-configure JSON fields into Endpoints.
			Only overwrites fields that are present and non-empty in the JSON.
			@param JSON Self-configure response body
			@param Endpoints Record to merge into (modified in-place)
			@return True if JSON was parsed successfully}
		class function Merge(const JSON: WideString; var Endpoints: TCloudEndpoints): Boolean; static;
	end;

implementation

uses
	SysUtils,
	SafeJSON;

class function TCloudEndpointsJsonAdapter.Merge(const JSON: WideString; var Endpoints: TCloudEndpoints): Boolean;
var
	Root: TSafeJSON;
	Value: WideString;
begin
	Result := False;

	Root := TSafeJSON.Parse(JSON);
	try
		if Root.IsNull then
			Exit;

		Value := Root.Get('api').AsString;
		if Value <> '' then
			Endpoints.ApiBase := Value;

		Value := Root.Get('oauth').AsString;
		if Value <> '' then
			Endpoints.OAuthUrl := Value;

		Value := Root.Get('dispatcher').AsString;
		if Value <> '' then
			Endpoints.DispatcherUrl := Value;

		Value := Root.Get('thumbnail').AsString;
		if Value <> '' then
			Endpoints.ThumbnailUrl := Value;

		Value := Root.Get('public').AsString;
		if Value <> '' then
			Endpoints.PublicUrl := Value;

		Value := Root.Get('download').AsString;
		if Value <> '' then
			Endpoints.DownloadUrl := Value;

		Value := Root.Get('upload').AsString;
		if Value <> '' then
			Endpoints.UploadUrl := Value;

		Result := True;
	finally
		Root.Free;
	end;
end;

end.

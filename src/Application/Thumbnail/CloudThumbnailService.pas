unit CloudThumbnailService;

{Service for retrieving thumbnail images from cloud.mail.ru.
	Downloads thumbnail from cloud server and converts to HBITMAP for TC.}

interface

uses
	Windows,
	CloudHTTP,
	CloudShardManager,
	CloudOAuth,
	ThumbnailBitmapConverter,
	Logger;

type
	ICloudThumbnailService = interface
		['{F1E2D3C4-B5A6-4789-0123-456789ABCDEF}']
		{Get thumbnail bitmap for a cloud file.
			@param CloudPath Path in cloud (e.g., /folder/image.jpg)
			@param RequestedWidth Maximum width requested
			@param RequestedHeight Maximum height requested
			@return HBITMAP handle on success, 0 on failure.
			Caller owns the handle and must delete it.}
		function GetThumbnail(const CloudPath: WideString; RequestedWidth, RequestedHeight: Integer): HBITMAP;
	end;

	TCloudThumbnailService = class(TInterfacedObject, ICloudThumbnailService)
	private
		FHTTP: ICloudHTTP;
		FShardManager: ICloudShardManager;
		FLogger: ILogger;
		FOAuthToken: TCloudOAuth;
		FConverter: IThumbnailBitmapConverter;
		FThumbnailUrl: WideString;

		{Build thumbnail URL with authentication parameters}
		function BuildThumbnailURL(const ShardURL, SizePreset, CloudPath: WideString): WideString;

		{Get thumbnail shard URL, resolving from dispatcher if needed}
		function GetThumbnailShard: WideString;
	public
		constructor Create(HTTP: ICloudHTTP; ShardManager: ICloudShardManager; Logger: ILogger; OAuthToken: TCloudOAuth; Converter: IThumbnailBitmapConverter; const ThumbnailUrl: WideString = '');

		function GetThumbnail(const CloudPath: WideString; RequestedWidth, RequestedHeight: Integer): HBITMAP;
	end;

implementation

uses
	System.SysUtils,
	System.Classes,
	CloudConstants,
	WFXTypes,
	PathHelper,
	ThumbnailSizeSelector;

{TCloudThumbnailService}

constructor TCloudThumbnailService.Create(HTTP: ICloudHTTP; ShardManager: ICloudShardManager; Logger: ILogger; OAuthToken: TCloudOAuth; Converter: IThumbnailBitmapConverter; const ThumbnailUrl: WideString);
begin
	inherited Create;
	FHTTP := HTTP;
	FShardManager := ShardManager;
	FLogger := Logger;
	FOAuthToken := OAuthToken;
	FConverter := Converter;
	FThumbnailUrl := ThumbnailUrl;
end;

function TCloudThumbnailService.GetThumbnailShard: WideString;
begin
	{Try to resolve thumbnail shard from dispatcher}
	if not FShardManager.ResolveShard(Result, SHARD_TYPE_THUMBNAILS) then
	begin
		{Fallback to configured thumbnail URL if dispatcher doesn't provide thumbnail shard}
		if FThumbnailUrl <> '' then
			Result := FThumbnailUrl
		else
			Result := THUMB_CLOUD_URL;
		FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, 'Using fallback thumbnail URL: %s', [Result]);
	end;
end;

function TCloudThumbnailService.BuildThumbnailURL(const ShardURL, SizePreset, CloudPath: WideString): WideString;
begin
	{URL format: <shard>/<size><encoded_path>?client_id=...&token=...
		PathToUrl handles backslash-to-slash conversion, URL encoding, and preserves leading '/'.
		Example: https://thumb.cloud.mail.ru/thumb/xw14/folder/image.jpg?client_id=...&token=...}
	Result := Format('%s%s/%s?client_id=%s&token=%s', [IncludeSlash(ShardURL), SizePreset, PathToUrl(CloudPath), OAUTH_CLIENT_ID, FOAuthToken.access_token]);
end;

function TCloudThumbnailService.GetThumbnail(const CloudPath: WideString; RequestedWidth, RequestedHeight: Integer): HBITMAP;
var
	SizePreset, ShardURL, ThumbnailURL: WideString;
	ImageStream: TMemoryStream;
	DownloadResult: Integer;
begin
	Result := 0;

	{Select appropriate size preset}
	SizePreset := SelectThumbnailPreset(RequestedWidth, RequestedHeight);

	{Get thumbnail shard URL}
	ShardURL := GetThumbnailShard;
	if ShardURL = '' then
		Exit;

	{Build full thumbnail URL}
	ThumbnailURL := BuildThumbnailURL(ShardURL, SizePreset, CloudPath);

	FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, 'Requesting thumbnail: %s', [ThumbnailURL]);

	{Download thumbnail image}
	ImageStream := TMemoryStream.Create;
	try
		DownloadResult := FHTTP.GetFile(ThumbnailURL, ImageStream, False);
		if (DownloadResult <> FS_FILE_OK) or (ImageStream.Size = 0) then
		begin
			{Missing thumbnails for unsupported types are expected, not errors}
			FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, 'Thumbnail download failed for: %s', [CloudPath]);
			Exit;
		end;

		ImageStream.Position := 0;
		Result := FConverter.ConvertToBitmap(ImageStream);
	finally
		ImageStream.Free;
	end;
end;

end.

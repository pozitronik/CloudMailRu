unit CloudThumbnailService;

{Service for retrieving thumbnail images from cloud.mail.ru.
	Downloads thumbnail from cloud server and converts to HBITMAP for TC.}

interface

uses
	Windows,
	CloudHTTP,
	CloudShardManager,
	CloudOAuth,
	TCLogger;

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

		{Build thumbnail URL with authentication parameters}
		function BuildThumbnailURL(const ShardURL, SizePreset, CloudPath: WideString): WideString;

		{Get thumbnail shard URL, resolving from dispatcher if needed}
		function GetThumbnailShard: WideString;
	public
		constructor Create(HTTP: ICloudHTTP; ShardManager: ICloudShardManager; Logger: ILogger; OAuthToken: TCloudOAuth);

		function GetThumbnail(const CloudPath: WideString; RequestedWidth, RequestedHeight: Integer): HBITMAP;
	end;

implementation

uses
	System.SysUtils,
	System.Classes,
	Vcl.Graphics,
	Vcl.Imaging.jpeg,
	Vcl.Imaging.pngimage,
	Vcl.Imaging.GIFImg,
	CloudConstants,
	WFXTypes,
	PathHelper,
	ThumbnailSizeSelector;

{TCloudThumbnailService}

constructor TCloudThumbnailService.Create(HTTP: ICloudHTTP; ShardManager: ICloudShardManager; Logger: ILogger; OAuthToken: TCloudOAuth);
begin
	inherited Create;
	FHTTP := HTTP;
	FShardManager := ShardManager;
	FLogger := Logger;
	FOAuthToken := OAuthToken;
end;

function TCloudThumbnailService.GetThumbnailShard: WideString;
begin
	{Try to resolve thumbnail shard from dispatcher}
	if not FShardManager.ResolveShard(Result, SHARD_TYPE_THUMBNAILS) then
	begin
		{Fallback to hardcoded URL if dispatcher doesn't provide thumbnail shard}
		Result := THUMB_CLOUD_URL;
		FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, 'Using fallback thumbnail URL: %s', [Result]);
	end;
end;

function TCloudThumbnailService.BuildThumbnailURL(const ShardURL, SizePreset, CloudPath: WideString): WideString;
var
	Path: WideString;
begin
	//Shard URL from dispatcher already includes /thumb/ path
	//URL format: <shard>/<size>/<path>?client_id=...&token=...
	//Example: https://thumb.cloud.mail.ru/thumb/xw14/folder/image.jpg?client_id=...&token=...
	Path := CloudPath;
	if (Length(Path) > 0) and (Path[1] = '/') then
		Delete(Path, 1, 1);

	Result := Format('%s%s/%s?client_id=%s&token=%s', [
		IncludeSlash(ShardURL),
		SizePreset,
		Path,
		OAUTH_CLIENT_ID,
		FOAuthToken.access_token
	]);
end;

function TCloudThumbnailService.GetThumbnail(const CloudPath: WideString; RequestedWidth, RequestedHeight: Integer): HBITMAP;
var
	SizePreset, ShardURL, ThumbnailURL: WideString;
	ImageStream: TMemoryStream;
	Bitmap: TBitmap;
	JPEGImage: TJPEGImage;
	pngimage: TPngImage;
	GIFImage: TGIFImage;
	DownloadResult: Integer;
	ImageType: string;
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
			FLogger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, 'Thumbnail download failed for: %s', [CloudPath]);
			Exit;
		end;

		ImageStream.Position := 0;

		{Create bitmap to hold the result}
		Bitmap := TBitmap.Create;
		try
			{Determine image type and load accordingly.
				Server typically returns JPEG, but we handle others too.}
			ImageType := LowerCase(ExtractFileExt(CloudPath));

			if (ImageType = '.png') then
			begin
				pngimage := TPngImage.Create;
				try
					pngimage.LoadFromStream(ImageStream);
					Bitmap.Assign(pngimage);
				finally
					pngimage.Free;
				end;
			end else if (ImageType = '.gif') then
			begin
				GIFImage := TGIFImage.Create;
				try
					GIFImage.LoadFromStream(ImageStream);
					Bitmap.Assign(GIFImage);
				finally
					GIFImage.Free;
				end;
			end else begin
				{Default: treat as JPEG (thumbnail server usually returns JPEG regardless of source format)}
				JPEGImage := TJPEGImage.Create;
				try
					JPEGImage.LoadFromStream(ImageStream);
					Bitmap.Assign(JPEGImage);
				finally
					JPEGImage.Free;
				end;
			end;

			{Release bitmap handle to caller - caller is responsible for cleanup}
			Result := Bitmap.ReleaseHandle;
		finally
			Bitmap.Free;
		end;
	finally
		ImageStream.Free;
	end;
end;

end.

unit ThumbnailBitmapConverter;

{Converts image stream data to HBITMAP for Total Commander thumbnail display.
	Detects image format from stream magic bytes because the thumbnail server
	may transcode uncommon formats (e.g. HEIC) to JPEG while the file path
	retains the original extension.}

interface

uses
	Windows,
	System.Classes;

type
	IThumbnailBitmapConverter = interface
		['{B2C3D4E5-F6A7-4890-1234-567890ABCDEF}']
		{Convert image stream to HBITMAP.
			@param ImageStream Stream containing image data (position must be at 0)
			@return HBITMAP handle on success, 0 on failure.
			Caller owns the handle and must delete it.}
		function ConvertToBitmap(ImageStream: TStream): HBITMAP;
	end;

	TThumbnailBitmapConverter = class(TInterfacedObject, IThumbnailBitmapConverter)
	public
		function ConvertToBitmap(ImageStream: TStream): HBITMAP;
	end;

implementation

uses
	Vcl.Graphics,
	Vcl.Imaging.jpeg,
	Vcl.Imaging.pngimage,
	Vcl.Imaging.GIFImg;

type
	{Detected image format based on stream magic bytes}
	TImageFormat = (ifJPEG, ifPNG, ifGIF, ifUnknown);

{Detect image format by examining the first bytes of the stream.
	Resets stream position to 0 after reading.}
function DetectImageFormat(Stream: TStream): TImageFormat;
var
	Header: array[0..7] of Byte;
	BytesRead: Integer;
begin
	Result := ifUnknown;
	if Stream.Size < 3 then
		Exit;

	Stream.Position := 0;
	BytesRead := Stream.Read(Header, SizeOf(Header));
	Stream.Position := 0;

	if BytesRead < 3 then
		Exit;

	{JPEG: FF D8 FF}
	if (Header[0] = $FF) and (Header[1] = $D8) and (Header[2] = $FF) then
		Exit(ifJPEG);

	{PNG: 89 50 4E 47 (.PNG)}
	if (BytesRead >= 4) and (Header[0] = $89) and (Header[1] = $50) and (Header[2] = $4E) and (Header[3] = $47) then
		Exit(ifPNG);

	{GIF: 47 49 46 (GIF)}
	if (Header[0] = $47) and (Header[1] = $49) and (Header[2] = $46) then
		Exit(ifGIF);
end;

function LoadJPEG(Stream: TStream; Bitmap: TBitmap): Boolean;
var
	JPEGImage: TJPEGImage;
begin
	JPEGImage := TJPEGImage.Create;
	try
		JPEGImage.LoadFromStream(Stream);
		Bitmap.Assign(JPEGImage);
		Result := True;
	finally
		JPEGImage.Free;
	end;
end;

function LoadPNG(Stream: TStream; Bitmap: TBitmap): Boolean;
var
	PNGImage: TPngImage;
begin
	PNGImage := TPngImage.Create;
	try
		PNGImage.LoadFromStream(Stream);
		Bitmap.Assign(PNGImage);
		Result := True;
	finally
		PNGImage.Free;
	end;
end;

function LoadGIF(Stream: TStream; Bitmap: TBitmap): Boolean;
var
	GIFImage: TGIFImage;
begin
	GIFImage := TGIFImage.Create;
	try
		GIFImage.LoadFromStream(Stream);
		Bitmap.Assign(GIFImage);
		Result := True;
	finally
		GIFImage.Free;
	end;
end;

{TThumbnailBitmapConverter}

function TThumbnailBitmapConverter.ConvertToBitmap(ImageStream: TStream): HBITMAP;
var
	Bitmap: TBitmap;
	Format: TImageFormat;
	Loaded: Boolean;
begin
	Result := 0;
	{No image format can be identified in fewer than 3 bytes}
	if (ImageStream = nil) or (ImageStream.Size < 3) then
		Exit;

	Format := DetectImageFormat(ImageStream);

	Bitmap := TBitmap.Create;
	try
		try
			case Format of
				ifPNG:  Loaded := LoadPNG(ImageStream, Bitmap);
				ifGIF:  Loaded := LoadGIF(ImageStream, Bitmap);
			else
				{JPEG and Unknown: try JPEG since server transcodes uncommon formats to JPEG}
				Loaded := LoadJPEG(ImageStream, Bitmap);
			end;
		except
			{Corrupt or unrecognized data - silently return 0}
			Loaded := False;
		end;

		if Loaded then
			Result := Bitmap.ReleaseHandle;
	finally
		Bitmap.Free;
	end;
end;

end.

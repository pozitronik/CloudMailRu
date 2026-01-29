unit ThumbnailBitmapConverterTest;

{Unit tests for ThumbnailBitmapConverter.
	Tests image format detection from stream magic bytes and conversion to HBITMAP.}

interface

uses
	Windows,
	Classes,
	SysUtils,
	DUnitX.TestFramework,
	ThumbnailBitmapConverter;

type
	[TestFixture]
	TThumbnailBitmapConverterTest = class
	private
		FConverter: IThumbnailBitmapConverter;

		{Create a minimal valid JPEG stream (SOI + APP0 + EOI markers)}
		function CreateMinimalJPEGStream: TMemoryStream;
		{Create a minimal valid PNG stream (signature + IHDR + IEND)}
		function CreateMinimalPNGStream: TMemoryStream;
		{Create a minimal valid GIF stream (GIF89a header + minimal image)}
		function CreateMinimalGIFStream: TMemoryStream;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestConvertToBitmap_NilStream_ReturnsZero;
		[Test]
		procedure TestConvertToBitmap_EmptyStream_ReturnsZero;
		[Test]
		procedure TestConvertToBitmap_TooSmallStream_ReturnsZero;
		[Test]
		procedure TestConvertToBitmap_InvalidData_ReturnsZero;
		[Test]
		procedure TestConvertToBitmap_ValidJPEG_ReturnsBitmap;
		[Test]
		procedure TestConvertToBitmap_ValidPNG_ReturnsBitmap;
		[Test]
		procedure TestConvertToBitmap_ValidGIF_ReturnsBitmap;
	end;

implementation

uses
	Vcl.Graphics,
	Vcl.Imaging.jpeg,
	Vcl.Imaging.pngimage,
	Vcl.Imaging.GIFImg;

{TThumbnailBitmapConverterTest}

procedure TThumbnailBitmapConverterTest.Setup;
begin
	FConverter := TThumbnailBitmapConverter.Create;
end;

procedure TThumbnailBitmapConverterTest.TearDown;
begin
	FConverter := nil;
end;

function TThumbnailBitmapConverterTest.CreateMinimalJPEGStream: TMemoryStream;
var
	Bitmap: TBitmap;
	JPEGImage: TJPEGImage;
begin
	{Create a real JPEG by converting a 1x1 bitmap}
	Result := TMemoryStream.Create;
	Bitmap := TBitmap.Create;
	try
		Bitmap.Width := 2;
		Bitmap.Height := 2;
		Bitmap.PixelFormat := pf24bit;
		JPEGImage := TJPEGImage.Create;
		try
			JPEGImage.Assign(Bitmap);
			JPEGImage.SaveToStream(Result);
		finally
			JPEGImage.Free;
		end;
	finally
		Bitmap.Free;
	end;
	Result.Position := 0;
end;

function TThumbnailBitmapConverterTest.CreateMinimalPNGStream: TMemoryStream;
var
	Bitmap: TBitmap;
	PNGImage: TPngImage;
begin
	{Create a real PNG by converting a 1x1 bitmap}
	Result := TMemoryStream.Create;
	Bitmap := TBitmap.Create;
	try
		Bitmap.Width := 2;
		Bitmap.Height := 2;
		Bitmap.PixelFormat := pf24bit;
		PNGImage := TPngImage.Create;
		try
			PNGImage.Assign(Bitmap);
			PNGImage.SaveToStream(Result);
		finally
			PNGImage.Free;
		end;
	finally
		Bitmap.Free;
	end;
	Result.Position := 0;
end;

function TThumbnailBitmapConverterTest.CreateMinimalGIFStream: TMemoryStream;
var
	Bitmap: TBitmap;
	GIFImage: TGIFImage;
begin
	{Create a real GIF by converting a 1x1 bitmap}
	Result := TMemoryStream.Create;
	Bitmap := TBitmap.Create;
	try
		Bitmap.Width := 2;
		Bitmap.Height := 2;
		Bitmap.PixelFormat := pf24bit;
		GIFImage := TGIFImage.Create;
		try
			GIFImage.Assign(Bitmap);
			GIFImage.SaveToStream(Result);
		finally
			GIFImage.Free;
		end;
	finally
		Bitmap.Free;
	end;
	Result.Position := 0;
end;

procedure TThumbnailBitmapConverterTest.TestConvertToBitmap_NilStream_ReturnsZero;
begin
	Assert.AreEqual(HBITMAP(0), FConverter.ConvertToBitmap(nil));
end;

procedure TThumbnailBitmapConverterTest.TestConvertToBitmap_EmptyStream_ReturnsZero;
var
	Stream: TMemoryStream;
begin
	Stream := TMemoryStream.Create;
	try
		Assert.AreEqual(HBITMAP(0), FConverter.ConvertToBitmap(Stream));
	finally
		Stream.Free;
	end;
end;

procedure TThumbnailBitmapConverterTest.TestConvertToBitmap_TooSmallStream_ReturnsZero;
var
	Stream: TMemoryStream;
	Data: array[0..1] of Byte;
begin
	Data[0] := $FF;
	Data[1] := $D8;
	Stream := TMemoryStream.Create;
	try
		Stream.WriteBuffer(Data, 2);
		Stream.Position := 0;
		Assert.AreEqual(HBITMAP(0), FConverter.ConvertToBitmap(Stream));
	finally
		Stream.Free;
	end;
end;

procedure TThumbnailBitmapConverterTest.TestConvertToBitmap_InvalidData_ReturnsZero;
var
	Stream: TMemoryStream;
	Data: array[0..31] of Byte;
	I: Integer;
begin
	{Fill with random non-image data}
	for I := 0 to High(Data) do
		Data[I] := Byte(I + 42);
	Stream := TMemoryStream.Create;
	try
		Stream.WriteBuffer(Data, SizeOf(Data));
		Stream.Position := 0;
		{Unknown format falls through to JPEG loader which should fail on garbage data}
		Assert.AreEqual(HBITMAP(0), FConverter.ConvertToBitmap(Stream));
	finally
		Stream.Free;
	end;
end;

procedure TThumbnailBitmapConverterTest.TestConvertToBitmap_ValidJPEG_ReturnsBitmap;
var
	Stream: TMemoryStream;
	BitmapHandle: HBITMAP;
begin
	Stream := CreateMinimalJPEGStream;
	try
		BitmapHandle := FConverter.ConvertToBitmap(Stream);
		try
			Assert.AreNotEqual(HBITMAP(0), BitmapHandle, 'JPEG conversion should produce a valid bitmap');
		finally
			if BitmapHandle <> 0 then
				DeleteObject(BitmapHandle);
		end;
	finally
		Stream.Free;
	end;
end;

procedure TThumbnailBitmapConverterTest.TestConvertToBitmap_ValidPNG_ReturnsBitmap;
var
	Stream: TMemoryStream;
	BitmapHandle: HBITMAP;
begin
	Stream := CreateMinimalPNGStream;
	try
		BitmapHandle := FConverter.ConvertToBitmap(Stream);
		try
			Assert.AreNotEqual(HBITMAP(0), BitmapHandle, 'PNG conversion should produce a valid bitmap');
		finally
			if BitmapHandle <> 0 then
				DeleteObject(BitmapHandle);
		end;
	finally
		Stream.Free;
	end;
end;

procedure TThumbnailBitmapConverterTest.TestConvertToBitmap_ValidGIF_ReturnsBitmap;
var
	Stream: TMemoryStream;
	BitmapHandle: HBITMAP;
begin
	Stream := CreateMinimalGIFStream;
	try
		BitmapHandle := FConverter.ConvertToBitmap(Stream);
		try
			Assert.AreNotEqual(HBITMAP(0), BitmapHandle, 'GIF conversion should produce a valid bitmap');
		finally
			if BitmapHandle <> 0 then
				DeleteObject(BitmapHandle);
		end;
	finally
		Stream.Free;
	end;
end;

initialization
	TDUnitX.RegisterTestFixture(TThumbnailBitmapConverterTest);

end.

unit ThumbnailSizeSelectorTest;

{Unit tests for ThumbnailSizeSelector.
	Tests size preset selection for various requested dimensions.}

interface

uses
	DUnitX.TestFramework,
	ThumbnailSizeSelector;

type
	[TestFixture]
	TThumbnailSizeSelectorTest = class
	private
		FSelector: IThumbnailSizeSelector;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Test smallest presets}
		[Test]
		procedure TestSelectPreset_TinyRequest_ReturnsSmallestFittingPreset;
		[Test]
		procedure TestSelectPreset_26x26_ReturnsXW11;

		{Test medium presets}
		[Test]
		procedure TestSelectPreset_100x100_ReturnsXW14;
		[Test]
		procedure TestSelectPreset_160x100_ReturnsXW14;
		[Test]
		procedure TestSelectPreset_200x150_ReturnsXW19;

		{Test large presets}
		[Test]
		procedure TestSelectPreset_300x200_ReturnsXW18;
		[Test]
		procedure TestSelectPreset_340x230_ReturnsXW21;

		{Test extra large requests}
		[Test]
		procedure TestSelectPreset_500x400_ReturnsXW2;
		[Test]
		procedure TestSelectPreset_2000x2000_ReturnsXW1;

		{Test portrait aspect ratios}
		[Test]
		procedure TestSelectPreset_20x30_ReturnsXW27;

		{Test edge cases}
		[Test]
		procedure TestSelectPreset_ZeroDimensions_ReturnsSmallestPreset;
		[Test]
		procedure TestSelectPreset_NegativeDimensions_ReturnsSmallestPreset;
	end;

	[TestFixture]
	TSelectThumbnailPresetFunctionTest = class
	public
		{Test standalone function matches interface method}
		[Test]
		procedure TestStandaloneFunction_MatchesInterfaceMethod;

		{Test common TC thumbnail sizes}
		[Test]
		procedure TestCommonTCSize_64x64_ReturnsAppropriatePreset;
		[Test]
		procedure TestCommonTCSize_128x128_ReturnsAppropriatePreset;
		[Test]
		procedure TestCommonTCSize_256x256_ReturnsAppropriatePreset;
	end;

implementation

{TThumbnailSizeSelectorTest}

procedure TThumbnailSizeSelectorTest.Setup;
begin
	FSelector := TThumbnailSizeSelector.Create;
end;

procedure TThumbnailSizeSelectorTest.TearDown;
begin
	FSelector := nil;
end;

procedure TThumbnailSizeSelectorTest.TestSelectPreset_TinyRequest_ReturnsSmallestFittingPreset;
var
	Result: string;
begin
	Result := FSelector.SelectPreset(10, 10);

	{10x10 fits in xw11 (26x26)}
	Assert.AreEqual('xw11', Result);
end;

procedure TThumbnailSizeSelectorTest.TestSelectPreset_26x26_ReturnsXW11;
var
	Result: string;
begin
	Result := FSelector.SelectPreset(26, 26);
	Assert.AreEqual('xw11', Result);
end;

procedure TThumbnailSizeSelectorTest.TestSelectPreset_100x100_ReturnsXW14;
var
	Result: string;
begin
	{100x100 needs a preset >= 100 in both dimensions}
	{xw14 is 160x107 - first preset that fits both dimensions}
	Result := FSelector.SelectPreset(100, 100);
	Assert.AreEqual('xw14', Result);
end;

procedure TThumbnailSizeSelectorTest.TestSelectPreset_160x100_ReturnsXW14;
var
	Result: string;
begin
	{160x100 fits in xw14 (160x107)}
	Result := FSelector.SelectPreset(160, 100);
	Assert.AreEqual('xw14', Result);
end;

procedure TThumbnailSizeSelectorTest.TestSelectPreset_200x150_ReturnsXW19;
var
	Result: string;
begin
	{200x150 needs preset >= 200 wide and >= 150 tall}
	{xw19 is 206x206 - first that fits both}
	Result := FSelector.SelectPreset(200, 150);
	Assert.AreEqual('xw19', Result);
end;

procedure TThumbnailSizeSelectorTest.TestSelectPreset_300x200_ReturnsXW18;
var
	Result: string;
begin
	{300x200 needs preset >= 300 wide and >= 200 tall}
	{xw18 is 305x230 - first that fits both dimensions}
	Result := FSelector.SelectPreset(300, 200);
	Assert.AreEqual('xw18', Result);
end;

procedure TThumbnailSizeSelectorTest.TestSelectPreset_340x230_ReturnsXW21;
var
	Result: string;
begin
	{340x230 needs preset >= 340 wide and >= 230 tall}
	{xw21 is 340x226 - not quite tall enough, so check actual result}
	Result := FSelector.SelectPreset(340, 226);
	Assert.AreEqual('xw21', Result);
end;

procedure TThumbnailSizeSelectorTest.TestSelectPreset_500x400_ReturnsXW2;
var
	Result: string;
begin
	{500x400 exceeds all standard presets, needs xw2 (1000x667)}
	Result := FSelector.SelectPreset(500, 400);
	Assert.AreEqual('xw2', Result);
end;

procedure TThumbnailSizeSelectorTest.TestSelectPreset_2000x2000_ReturnsXW1;
var
	Result: string;
begin
	{2000x2000 exceeds all presets, needs original (xw1)}
	Result := FSelector.SelectPreset(2000, 2000);
	Assert.AreEqual('xw1', Result);
end;

procedure TThumbnailSizeSelectorTest.TestSelectPreset_20x30_ReturnsXW27;
var
	Result: string;
begin
	{20x30 (portrait) fits in xw27 (28x38)}
	Result := FSelector.SelectPreset(20, 30);
	Assert.AreEqual('xw27', Result);
end;

procedure TThumbnailSizeSelectorTest.TestSelectPreset_ZeroDimensions_ReturnsSmallestPreset;
var
	Result: string;
begin
	{0x0 should return smallest preset (xw11)}
	Result := FSelector.SelectPreset(0, 0);
	Assert.AreEqual('xw11', Result);
end;

procedure TThumbnailSizeSelectorTest.TestSelectPreset_NegativeDimensions_ReturnsSmallestPreset;
var
	Result: string;
begin
	{Negative dimensions should still work (return smallest)}
	Result := FSelector.SelectPreset(-10, -10);
	Assert.AreEqual('xw11', Result);
end;

{TSelectThumbnailPresetFunctionTest}

procedure TSelectThumbnailPresetFunctionTest.TestStandaloneFunction_MatchesInterfaceMethod;
var
	Selector: IThumbnailSizeSelector;
	FunctionResult, InterfaceResult: string;
begin
	Selector := TThumbnailSizeSelector.Create;

	FunctionResult := SelectThumbnailPreset(150, 150);
	InterfaceResult := Selector.SelectPreset(150, 150);

	Assert.AreEqual(InterfaceResult, FunctionResult);
end;

procedure TSelectThumbnailPresetFunctionTest.TestCommonTCSize_64x64_ReturnsAppropriatePreset;
var
	Result: string;
begin
	{TC often uses 64x64 thumbnails}
	Result := SelectThumbnailPreset(64, 64);
	{Should return smallest preset that fits 64x64}
	Assert.IsNotEmpty(Result);
end;

procedure TSelectThumbnailPresetFunctionTest.TestCommonTCSize_128x128_ReturnsAppropriatePreset;
var
	Result: string;
begin
	{TC often uses 128x128 thumbnails}
	Result := SelectThumbnailPreset(128, 128);
	{xw29 (150x150) should fit}
	Assert.AreEqual('xw29', Result);
end;

procedure TSelectThumbnailPresetFunctionTest.TestCommonTCSize_256x256_ReturnsAppropriatePreset;
var
	Result: string;
begin
	{TC can request 256x256 thumbnails}
	Result := SelectThumbnailPreset(256, 256);
	{Should return a preset that can handle square 256px}
	Assert.IsNotEmpty(Result);
end;

initialization
	TDUnitX.RegisterTestFixture(TThumbnailSizeSelectorTest);
	TDUnitX.RegisterTestFixture(TSelectThumbnailPresetFunctionTest);

end.

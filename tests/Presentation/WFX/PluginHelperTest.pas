unit PluginHelperTest;

interface

uses
	PluginHelper,
	CMRConstants,
	DUnitX.TestFramework;

type

	[TestFixture]
	TPluginHelperTest = class
	public
		{ FormatSize tests - TYPE_AUTO mode }
		[Test]
		procedure TestFormatSizeAutoBytes;
		[Test]
		procedure TestFormatSizeAutoKilobytes;
		[Test]
		procedure TestFormatSizeAutoMegabytes;
		[Test]
		procedure TestFormatSizeAutoGigabytes;
		[Test]
		procedure TestFormatSizeAutoTerabytes;
		[Test]
		procedure TestFormatSizeAutoPetabytes;
		[Test]
		procedure TestFormatSizeAutoZero;
		[Test]
		procedure TestFormatSizeAutoExactBoundary;
		[Test]
		procedure TestFormatSizeAutoBelowBoundary;
		[Test]
		procedure TestFormatSizeAutoLargeValue;

		{ ShardTypeFromStreamingFormat tests }
		[Test]
		procedure TestShardTypeFromStreamingFormatWeblinkView;
		[Test]
		procedure TestShardTypeFromStreamingFormatVideo;
		[Test]
		procedure TestShardTypeFromStreamingFormatViewDirect;
		[Test]
		procedure TestShardTypeFromStreamingFormatThumbnails;
		[Test]
		procedure TestShardTypeFromStreamingFormatWeblinkThumbnails;
		[Test]
		procedure TestShardTypeFromStreamingFormatNone;
		[Test]
		procedure TestShardTypeFromStreamingFormatDisabled;
		[Test]
		procedure TestShardTypeFromStreamingFormatPlaylist;
		[Test]
		procedure TestShardTypeFromStreamingFormatDefault;
		[Test]
		procedure TestShardTypeFromStreamingFormatUnset;
		[Test]
		procedure TestShardTypeFromStreamingFormatUnknown;

		{ FormatSize tests - explicit SizeType mode }
		[Test]
		procedure TestFormatSizeExplicitBytes;
		[Test]
		procedure TestFormatSizeExplicitKilobytes;
		[Test]
		procedure TestFormatSizeExplicitMegabytes;
	end;

implementation

{ FormatSize TYPE_AUTO tests - automatic unit detection }

procedure TPluginHelperTest.TestFormatSizeAutoBytes;
begin
	{ Values under 1024 stay in bytes }
	Assert.AreEqual('0 b', FormatSize(0));
	Assert.AreEqual('1 b', FormatSize(1));
	Assert.AreEqual('100 b', FormatSize(100));
	Assert.AreEqual('1023 b', FormatSize(1023));
end;

procedure TPluginHelperTest.TestFormatSizeAutoKilobytes;
begin
	{ Values at or above 1024 convert to next unit }
	Assert.AreEqual('1 kb', FormatSize(1024)); { exactly 1024 converts }
	Assert.AreEqual('1 kb', FormatSize(1025)); { just over boundary: converts }
	Assert.AreEqual('10 kb', FormatSize(10240)); { 10 * 1024 }
end;

procedure TPluginHelperTest.TestFormatSizeAutoMegabytes;
begin
	{ Values at or above 1024 kb convert to Mb }
	Assert.AreEqual('1 Mb', FormatSize(1048576)); { exactly 1024^2 = 1 Mb }
	Assert.AreEqual('50 Mb', FormatSize(52428800)); { 50 * 1024^2 }
	Assert.AreEqual('100 Mb', FormatSize(104857600)); { 100 * 1024^2 }
end;

procedure TPluginHelperTest.TestFormatSizeAutoGigabytes;
begin
	{ Values at or above 1024 Mb convert to Gb }
	Assert.AreEqual('1 Gb', FormatSize(1073741824)); { exactly 1024^3 = 1 Gb }
	Assert.AreEqual('2 Gb', FormatSize(2147483648)); { 2 * 1024^3 }
	Assert.AreEqual('10 Gb', FormatSize(10737418240)); { 10 * 1024^3 }
end;

procedure TPluginHelperTest.TestFormatSizeAutoTerabytes;
begin
	{ Values at or above 1024 Gb convert to Tb }
	Assert.AreEqual('1 Tb', FormatSize(1099511627776)); { exactly 1024^4 = 1 Tb }
	Assert.AreEqual('2 Tb', FormatSize(2199023255552)); { 2 * 1024^4 }
end;

procedure TPluginHelperTest.TestFormatSizeAutoPetabytes;
begin
	{ Values at or above 1024 Tb convert to Pb }
	Assert.AreEqual('1 Pb', FormatSize(1125899906842624)); { exactly 1024^5 = 1 Pb }
	Assert.AreEqual('2 Pb', FormatSize(2251799813685248)); { 2 * 1024^5 }
end;

procedure TPluginHelperTest.TestFormatSizeAutoZero;
begin
	{ Zero should return 0 bytes }
	Assert.AreEqual('0 b', FormatSize(0));
end;

procedure TPluginHelperTest.TestFormatSizeAutoExactBoundary;
begin
	{ Exactly at 1024 boundary converts to next unit (uses < 1024 check) }
	Assert.AreEqual('1 kb', FormatSize(1024));
	Assert.AreEqual('1 Mb', FormatSize(1048576));
end;

procedure TPluginHelperTest.TestFormatSizeAutoBelowBoundary;
begin
	{ Just below boundary stays in current unit }
	Assert.AreEqual('1023 b', FormatSize(1023));
	Assert.AreEqual('1023 kb', FormatSize(1047552));
end;

procedure TPluginHelperTest.TestFormatSizeAutoLargeValue;
begin
	{ Large values should be handled correctly }
	Assert.AreEqual('2 Tb', FormatSize(2199023255552)); { 2 * 1024^4 }
end;

{ ShardTypeFromStreamingFormat tests }

procedure TPluginHelperTest.TestShardTypeFromStreamingFormatWeblinkView;
begin
	Assert.AreEqual(SHARD_TYPE_WEBLINK_VIEW, ShardTypeFromStreamingFormat(STREAMING_FORMAT_WEBLINK_VIEW));
end;

procedure TPluginHelperTest.TestShardTypeFromStreamingFormatVideo;
begin
	Assert.AreEqual(SHARD_TYPE_VIDEO, ShardTypeFromStreamingFormat(STREAMING_FORMAT_VIDEO));
end;

procedure TPluginHelperTest.TestShardTypeFromStreamingFormatViewDirect;
begin
	Assert.AreEqual(SHARD_TYPE_VIEW_DIRECT, ShardTypeFromStreamingFormat(STREAMING_FORMAT_VIEW_DIRECT));
end;

procedure TPluginHelperTest.TestShardTypeFromStreamingFormatThumbnails;
begin
	Assert.AreEqual(SHARD_TYPE_THUMBNAILS, ShardTypeFromStreamingFormat(STREAMING_FORMAT_THUMBNAILS));
end;

procedure TPluginHelperTest.TestShardTypeFromStreamingFormatWeblinkThumbnails;
begin
	Assert.AreEqual(SHARD_TYPE_WEBLINK_THUMBNAILS, ShardTypeFromStreamingFormat(STREAMING_FORMAT_WEBLINK_THUMBNAILS));
end;

procedure TPluginHelperTest.TestShardTypeFromStreamingFormatNone;
begin
	{ STREAMING_FORMAT_NONE (0) is not in the case statement, returns default }
	Assert.AreEqual(SHARD_TYPE_DEFAULT, ShardTypeFromStreamingFormat(STREAMING_FORMAT_NONE));
end;

procedure TPluginHelperTest.TestShardTypeFromStreamingFormatDisabled;
begin
	{ STREAMING_FORMAT_DISABLED (1) is not in the case statement, returns default }
	Assert.AreEqual(SHARD_TYPE_DEFAULT, ShardTypeFromStreamingFormat(STREAMING_FORMAT_DISABLED));
end;

procedure TPluginHelperTest.TestShardTypeFromStreamingFormatPlaylist;
begin
	{ STREAMING_FORMAT_PLAYLIST (2) is not in the case statement, returns default }
	Assert.AreEqual(SHARD_TYPE_DEFAULT, ShardTypeFromStreamingFormat(STREAMING_FORMAT_PLAYLIST));
end;

procedure TPluginHelperTest.TestShardTypeFromStreamingFormatDefault;
begin
	{ STREAMING_FORMAT_DEFAULT (3) is not in the case statement, returns default shard }
	Assert.AreEqual(SHARD_TYPE_DEFAULT, ShardTypeFromStreamingFormat(STREAMING_FORMAT_DEFAULT));
end;

procedure TPluginHelperTest.TestShardTypeFromStreamingFormatUnset;
begin
	{ STREAMING_FORMAT_UNSET (-1) returns default }
	Assert.AreEqual(SHARD_TYPE_DEFAULT, ShardTypeFromStreamingFormat(STREAMING_FORMAT_UNSET));
end;

procedure TPluginHelperTest.TestShardTypeFromStreamingFormatUnknown;
begin
	{ Unknown values return default shard type }
	Assert.AreEqual(SHARD_TYPE_DEFAULT, ShardTypeFromStreamingFormat(999));
	Assert.AreEqual(SHARD_TYPE_DEFAULT, ShardTypeFromStreamingFormat(-999));
end;

{ FormatSize explicit SizeType tests - TDD: these expose the array index bug }

procedure TPluginHelperTest.TestFormatSizeExplicitBytes;
begin
	{ TYPE_BYTES (0) - no conversion, just append 'b' suffix }
	Assert.AreEqual('1024 b', FormatSize(1024, TYPE_BYTES));
	Assert.AreEqual('5000 b', FormatSize(5000, TYPE_BYTES));
end;

procedure TPluginHelperTest.TestFormatSizeExplicitKilobytes;
begin
	{ TYPE_KYLOBYTES (1) - divide by 1024 once, append 'kb' suffix }
	{ 2048 bytes = 2 kb }
	Assert.AreEqual('2 kb', FormatSize(2048, TYPE_KYLOBYTES), 'TYPE_KYLOBYTES should show kb suffix');
	{ 10240 bytes = 10 kb }
	Assert.AreEqual('10 kb', FormatSize(10240, TYPE_KYLOBYTES), 'TYPE_KYLOBYTES should divide by 1024 once');
end;

procedure TPluginHelperTest.TestFormatSizeExplicitMegabytes;
begin
	{ TYPE_MEGABYTES (2) - divide by 1024 twice, append 'Mb' suffix }
	{ 2097152 bytes = 2 Mb }
	Assert.AreEqual('2 Mb', FormatSize(2097152, TYPE_MEGABYTES), 'TYPE_MEGABYTES should show Mb suffix');
	{ 10485760 bytes = 10 Mb }
	Assert.AreEqual('10 Mb', FormatSize(10485760, TYPE_MEGABYTES), 'TYPE_MEGABYTES should divide by 1024 twice');
end;

initialization

TDUnitX.RegisterTestFixture(TPluginHelperTest);

end.

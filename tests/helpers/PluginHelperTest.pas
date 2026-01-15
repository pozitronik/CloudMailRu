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
	{ Note: uses > 1024 not >= 1024, so exactly 1024 stays in bytes }
	Assert.AreEqual('1024 b', FormatSize(1024)); { boundary: stays in bytes }
	Assert.AreEqual('1 kb', FormatSize(1025)); { just over boundary: converts }
	Assert.AreEqual('10 kb', FormatSize(10241)); { 10 * 1024 + 1 }
end;

procedure TPluginHelperTest.TestFormatSizeAutoMegabytes;
begin
	{ Note: uses > 1024 at each level with integer division }
	{ To get Mb, need (size div 1024) > 1024, so size > 1024*1025 = 1049600 }
	Assert.AreEqual('1024 kb', FormatSize(1048576)); { exactly 1024^2 stays in kb }
	Assert.AreEqual('1 Mb', FormatSize(1049601)); { smallest value that shows Mb }
	Assert.AreEqual('50 Mb', FormatSize(52428800)); { 50 * 1024^2 }
end;

procedure TPluginHelperTest.TestFormatSizeAutoGigabytes;
begin
	{ Note: boundary values stay in lower unit due to > 1024 check with integer division }
	Assert.AreEqual('1024 Mb', FormatSize(1073741824)); { exactly 1024^3 stays in Mb }
	{ To get Gb, need value where second div 1024 gives > 1024 }
	Assert.AreEqual('1 Gb', FormatSize(1074790401)); { 1024 * 1024 * 1025 + 1 }
	Assert.AreEqual('2 Gb', FormatSize(2149580801)); { 2 * 1024^3 + extra }
end;

procedure TPluginHelperTest.TestFormatSizeAutoTerabytes;
begin
	{ Note: boundary values stay in lower unit due to > 1024 check }
	Assert.AreEqual('1024 Gb', FormatSize(1099511627776)); { exactly 1024^4 stays in Gb }
	{ To get Tb, need value well above boundary: 1024^3 * 1025 + 1 = 1100585369601 }
	Assert.AreEqual('1 Tb', FormatSize(1100585369601));
end;

procedure TPluginHelperTest.TestFormatSizeAutoPetabytes;
begin
	{ Note: boundary values stay in lower unit due to > 1024 check }
	Assert.AreEqual('1024 Tb', FormatSize(1125899906842624)); { exactly 1024^5 stays in Tb }
	{ To get Pb, need value well above boundary: 1024^4 * 1025 + 1 = 1126999418470401 }
	Assert.AreEqual('1 Pb', FormatSize(1126999418470401));
end;

procedure TPluginHelperTest.TestFormatSizeAutoZero;
begin
	{ Zero should return 0 bytes }
	Assert.AreEqual('0 b', FormatSize(0));
end;

procedure TPluginHelperTest.TestFormatSizeAutoExactBoundary;
begin
	{ Exactly at 1024 boundary stays in current unit (uses > not >=) }
	Assert.AreEqual('1024 b', FormatSize(1024));
	Assert.AreEqual('1024 kb', FormatSize(1048576));
end;

procedure TPluginHelperTest.TestFormatSizeAutoBelowBoundary;
begin
	{ Just below boundary stays in current unit }
	Assert.AreEqual('1023 b', FormatSize(1023));
	Assert.AreEqual('1023 kb', FormatSize(1047552));
end;

procedure TPluginHelperTest.TestFormatSizeAutoLargeValue;
begin
	{ Large values should be handled correctly using integer division }
	{ 2 TB = 2199023255552 bytes }
	Assert.AreEqual('2 Tb', FormatSize(2199023255552));
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

initialization

TDUnitX.RegisterTestFixture(TPluginHelperTest);

end.

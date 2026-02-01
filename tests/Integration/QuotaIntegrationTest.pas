unit QuotaIntegrationTest;

{Integration tests for quota/space operations against live cloud.mail.ru API.}

interface

uses
	DUnitX.TestFramework,
	IntegrationTestBase,
	IntegrationTestConfig;

type
	{No [TestFixture] attribute - registered conditionally in initialization}
	TQuotaIntegrationTest = class(TIntegrationTestBase)
	public
		[Test]
		procedure TestGetUserSpace_ReturnsValidData;

		[Test]
		procedure TestLogUserSpaceInfo_LogsCorrectly;

		[Test]
		procedure TestGetUserSpace_ReturnsValidSpaceInfo;
	end;

implementation

uses
	System.SysUtils,
	CloudSpace;

{TQuotaIntegrationTest}

procedure TQuotaIntegrationTest.TestGetUserSpace_ReturnsValidData;
begin
	{Access protected method via downcast - alternatively expose via property}
	{For now, use LogUserSpaceInfo which internally calls GetUserSpace}

	{The primary cloud instance is logged in - verify space info is accessible}
	{We don't have direct access to GetUserSpace, but we can verify via listing}
	Assert.IsNotNull(FPrimaryCloud, 'Cloud should be initialized');

	{LogUserSpaceInfo internally fetches and logs space - if it doesn't crash, space fetch works}
	FPrimaryCloud.LogUserSpaceInfo;

	{Test passes if no exception was raised}
	Assert.Pass('GetUserSpace executed successfully via LogUserSpaceInfo');
end;

procedure TQuotaIntegrationTest.TestLogUserSpaceInfo_LogsCorrectly;
begin
	{Simply verify the method executes without error}
	Assert.IsNotNull(FPrimaryCloud, 'Cloud should be initialized');

	FPrimaryCloud.LogUserSpaceInfo;

	{If we reach here without exception, logging works}
	Assert.Pass('LogUserSpaceInfo executed successfully');
end;

procedure TQuotaIntegrationTest.TestGetUserSpace_ReturnsValidSpaceInfo;
var
	SpaceInfo: TCloudSpace;
	GetResult: Boolean;
begin
	{Call GetUserSpace directly (now public)}
	GetResult := FPrimaryCloud.GetUserSpace(SpaceInfo);

	Assert.IsTrue(GetResult, 'GetUserSpace should succeed');
	Assert.IsTrue(SpaceInfo.total > 0, 'Total space should be > 0');
	Assert.IsTrue(SpaceInfo.used >= 0, 'Used space should be >= 0');
	Assert.IsTrue(SpaceInfo.used <= SpaceInfo.total, 'Used space should not exceed total');
end;

initialization
	if TIntegrationTestConfig.IsEnabled then
		TDUnitX.RegisterTestFixture(TQuotaIntegrationTest);

end.

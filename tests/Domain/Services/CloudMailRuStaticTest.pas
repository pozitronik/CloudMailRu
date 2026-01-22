unit CloudMailRuStaticTest;

interface

uses
	CloudMailRu,
	CloudMailRuFactory,
	DUnitX.TestFramework;

type
	{ Tests for TCloudMailRuFactory.
	  CloudAccessToString/StringToCloudAccess tests moved to CloudAccessUtilsTest.
	  ErrorCodeText tests moved to CloudErrorMapperTest. }
	[TestFixture]
	TCloudMailRuStaticTest = class
	public
		{ CreatePublicCloud tests }
		[Test]
		procedure TestCreatePublicCloud_CreatesCloudInstance;
		[Test]
		procedure TestCreatePublicCloud_SetsPublicAccountFlag;
		[Test]
		procedure TestCreatePublicCloud_CloudMustBeFreedByCaller;
	end;

implementation

{ CreatePublicCloud tests }

procedure TCloudMailRuStaticTest.TestCreatePublicCloud_CreatesCloudInstance;
var
	TempCloud: TCloudMailRu;
begin
	{ CreatePublicCloud creates a TCloudMailRu instance via the factory.
	  Note: Login will fail without network, but instance should still be created.
	  We don't test Login result since it requires network. }
	TempCloud := nil;
	try
		TCloudMailRuFactory.CreatePublicCloud(TempCloud, 'https://cloud.mail.ru/public/test123');
		Assert.IsNotNull(TempCloud, 'CreatePublicCloud should create a cloud instance');
	finally
		TempCloud.Free;
	end;
end;

procedure TCloudMailRuStaticTest.TestCreatePublicCloud_SetsPublicAccountFlag;
var
	TempCloud: TCloudMailRu;
begin
	{ Verify the created instance is configured as public account }
	TempCloud := nil;
	try
		TCloudMailRuFactory.CreatePublicCloud(TempCloud, 'https://cloud.mail.ru/public/abc123');
		Assert.IsTrue(TempCloud.IsPublicAccount, 'Created cloud should be marked as public account');
	finally
		TempCloud.Free;
	end;
end;

procedure TCloudMailRuStaticTest.TestCreatePublicCloud_CloudMustBeFreedByCaller;
var
	TempCloud: TCloudMailRu;
begin
	{ Verify that after CreatePublicCloud, the caller owns the instance and must free it.
	  This test ensures no memory leaks by properly freeing. FastMM will catch leaks. }
	TempCloud := nil;
	TCloudMailRuFactory.CreatePublicCloud(TempCloud, 'https://cloud.mail.ru/public/xyz789');
	Assert.IsNotNull(TempCloud, 'Instance should be created');
	TempCloud.Free; { Caller is responsible for freeing }
	Assert.Pass('Instance freed successfully - caller owns the instance');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudMailRuStaticTest);

end.

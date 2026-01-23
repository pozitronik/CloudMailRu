unit CloudMailRuFactoryTest;

interface

uses
	CloudMailRu,
	CloudMailRuFactory,
	DUnitX.TestFramework;

type
	{Tests for CloudMailRuFactory - verifies factory creates instances with correct configuration.
		Uses null implementations to avoid network dependencies.}
	[TestFixture]
	TCloudMailRuFactoryTest = class
	public
		{TCloudMailRuFactory.CreatePublicCloud tests}
		[Test]
		procedure TestCreatePublicCloud_ReturnsCloudInstance;
		[Test]
		procedure TestCreatePublicCloud_SetsPublicAccountTrue;
		[Test]
		procedure TestCreatePublicCloud_WithEmptyUrl_SetsPublicAccountTrue;
		[Test]
		procedure TestCreatePublicCloud_ReturnsFalse_WhenNoHTTPManager;

		{TPublicCloudFactory interface tests}
		[Test]
		procedure TestPublicCloudFactory_ImplementsInterface;
		[Test]
		procedure TestPublicCloudFactory_CreatesCloudInstance;
		[Test]
		procedure TestPublicCloudFactory_SetsPublicAccountTrue;
	end;

implementation

{TCloudMailRuFactoryTest}

procedure TCloudMailRuFactoryTest.TestCreatePublicCloud_ReturnsCloudInstance;
var
	TempCloud: TCloudMailRu;
begin
	TempCloud := nil;
	try
		{Login returns false because null HTTP manager, but cloud is still created}
		TCloudMailRuFactory.CreatePublicCloud(TempCloud, 'https://cloud.mail.ru/public/test');
		Assert.IsNotNull(TempCloud, 'Factory should create TCloudMailRu instance');
	finally
		TempCloud.Free;
	end;
end;

procedure TCloudMailRuFactoryTest.TestCreatePublicCloud_SetsPublicAccountTrue;
var
	TempCloud: TCloudMailRu;
begin
	TempCloud := nil;
	try
		TCloudMailRuFactory.CreatePublicCloud(TempCloud, 'https://cloud.mail.ru/public/test');
		Assert.IsTrue(TempCloud.IsPublicAccount, 'Factory should set PublicAccount to True');
	finally
		TempCloud.Free;
	end;
end;

procedure TCloudMailRuFactoryTest.TestCreatePublicCloud_WithEmptyUrl_SetsPublicAccountTrue;
var
	TempCloud: TCloudMailRu;
begin
	TempCloud := nil;
	try
		TCloudMailRuFactory.CreatePublicCloud(TempCloud, '');
		Assert.IsNotNull(TempCloud, 'Factory should create instance even with empty URL');
		Assert.IsTrue(TempCloud.IsPublicAccount, 'PublicAccount should be True');
	finally
		TempCloud.Free;
	end;
end;

procedure TCloudMailRuFactoryTest.TestCreatePublicCloud_ReturnsFalse_WhenNoHTTPManager;
var
	TempCloud: TCloudMailRu;
	Result: Boolean;
begin
	TempCloud := nil;
	try
		{Factory passes nil HTTP manager, so Login should fail}
		Result := TCloudMailRuFactory.CreatePublicCloud(TempCloud, 'https://cloud.mail.ru/public/test');
		Assert.IsFalse(Result, 'CreatePublicCloud should return False without HTTP manager');
	finally
		TempCloud.Free;
	end;
end;

procedure TCloudMailRuFactoryTest.TestPublicCloudFactory_ImplementsInterface;
var
	Factory: IPublicCloudFactory;
begin
	Factory := TPublicCloudFactory.Create;
	Assert.IsNotNull(Factory, 'TPublicCloudFactory should implement IPublicCloudFactory');
end;

procedure TCloudMailRuFactoryTest.TestPublicCloudFactory_CreatesCloudInstance;
var
	Factory: IPublicCloudFactory;
	TempCloud: TCloudMailRu;
begin
	Factory := TPublicCloudFactory.Create;
	TempCloud := nil;
	try
		Factory.CreatePublicCloud(TempCloud, 'https://cloud.mail.ru/public/test');
		Assert.IsNotNull(TempCloud, 'Interface method should create cloud instance');
	finally
		TempCloud.Free;
	end;
end;

procedure TCloudMailRuFactoryTest.TestPublicCloudFactory_SetsPublicAccountTrue;
var
	Factory: IPublicCloudFactory;
	TempCloud: TCloudMailRu;
begin
	Factory := TPublicCloudFactory.Create;
	TempCloud := nil;
	try
		Factory.CreatePublicCloud(TempCloud, 'https://cloud.mail.ru/public/test');
		Assert.IsTrue(TempCloud.IsPublicAccount, 'Interface method should set PublicAccount to True');
	finally
		TempCloud.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TCloudMailRuFactoryTest);

end.

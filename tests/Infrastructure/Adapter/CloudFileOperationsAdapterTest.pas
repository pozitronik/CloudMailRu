unit CloudFileOperationsAdapterTest;

{Tests for CloudFileOperationsAdapter - adapter wrapping TCloudMailRu for basic file operations.}

interface

uses
	CloudFileOperationsAdapter,
	DUnitX.TestFramework;

type
	{Tests for TCloudFileOperationsAdapter}
	[TestFixture]
	TCloudFileOperationsAdapterTest = class
	public
		{ Interface implementation test }
		[Test]
		procedure TestImplementsICloudFileOperations;
	end;

implementation

{ TCloudFileOperationsAdapterTest }

procedure TCloudFileOperationsAdapterTest.TestImplementsICloudFileOperations;
var
	Adapter: ICloudFileOperations;
begin
	{Cannot test actual delegation without TCloudMailRu instance,
	 but we can verify interface implementation}
	Adapter := TCloudFileOperationsAdapter.Create(nil);
	Assert.IsNotNull(Adapter, 'Should implement ICloudFileOperations interface');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudFileOperationsAdapterTest);

end.

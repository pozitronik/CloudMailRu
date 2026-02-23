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
		procedure TestImplementsICloudFileOperationsAdapter;
	end;

implementation

{ TCloudFileOperationsAdapterTest }

procedure TCloudFileOperationsAdapterTest.TestImplementsICloudFileOperationsAdapter;
var
	Adapter: ICloudFileOperationsAdapter;
begin
	{Cannot test actual delegation without TCloudMailRu instance,
	 but we can verify interface implementation}
	Adapter := TCloudFileOperationsAdapter.Create(nil);
	Assert.IsNotNull(Adapter, 'Should implement ICloudFileOperationsAdapter interface');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudFileOperationsAdapterTest);

end.

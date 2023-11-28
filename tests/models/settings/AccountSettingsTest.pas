unit AccountSettingsTest;

interface

uses
	NewAccountSettings,
	DUnitX.TestFramework;

type

	[TestFixture]
	TAccountSettingsTest = class
	public
		// Sample Methods
		// Simple single Test
		[Test]
		procedure Test1;
		// Test with TestCase Attribute to supply parameters.
		[Test]
		[TestCase('TestA', '1,2')]
		[TestCase('TestB', '3,4')]
		procedure Test2(const AValue1: Integer; const AValue2: Integer);
	end;

implementation

procedure TAccountSettingsTest.Test1;
begin
end;

procedure TAccountSettingsTest.Test2(const AValue1: Integer; const AValue2: Integer);
begin
end;

initialization

TDUnitX.RegisterTestFixture(TAccountSettingsTest);

end.

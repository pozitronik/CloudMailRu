unit SystemHelperTest;

interface

uses
	SystemHelper,
	DUnitX.TestFramework;

type
	TTestRecordType = record
		i: Integer;
		s: WideString;
	end;

	[TestFixture]
	TSystemHelperTest = class
	public
		[test]
		procedure TestTernary;
	end;

implementation

{TSystemHelperTest}

procedure TSystemHelperTest.TestTernary;
var
	TestRecordOne, TestRecordTwo: TTestRecordType;
begin

	Assert.IsTrue(Ternary.IfElse(1 = 1, True, False));
	Assert.IsFalse(Ternary.IfElse(1 = 0, True, False));

	Assert.AreEqual('StringOne', Ternary.IfElse(1 = 1, 'StringOne', 'StringTwo'));
	Assert.AreEqual('StringFour', Ternary.IfElse(1 = 0, 'StringThree', 'StringFour'));

	Ternary.IfElse<>(1 = 0, (100 * 2), (21 * 2));

	Assert.AreEqual(10, Ternary.IfElse(1 = 1, 5 + 5, 6 + 6));
	Assert.IsTrue(42 = Ternary.IfElse(1 = 0, 100 * 2, 84 / 2));

	TestRecordOne.i := 12;
	TestRecordOne.s := 'abc';

	TestRecordTwo.i := 54;
	TestRecordTwo.s := 'xyz';

	Assert.AreEqual('StringOne', Ternary.IfElse<TTestRecordType>(1 = 1, TestRecordOne, TestRecordTwo));
	Assert.AreEqual('StringFour', Ternary.IfElse<TTestRecordType>(1 = 0, TestRecordOne, TestRecordTwo));
end;

initialization

TDUnitX.RegisterTestFixture(TSystemHelperTest);

end.

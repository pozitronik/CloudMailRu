unit TCPasswordManagerTest;

interface

uses
	IPasswordManagerInterface,
	TCPasswordManager,
	ILoggerInterface,
	DUnitX.TestFramework;

type
	[TestFixture]
	TTCPasswordManagerTest = class
	public
		[Test]
		{Verifies TTCPasswordManager implements IPasswordManager interface}
		procedure TestImplementsIPasswordManager;
	end;

implementation

procedure TTCPasswordManagerTest.TestImplementsIPasswordManager;
var
	PasswordManager: IPasswordManager;
begin
	{Note: TTCPasswordManager requires TC callbacks for actual password operations.
	 This test only verifies interface implementation, not functionality.
	 Functional testing requires running within Total Commander context.}
	PasswordManager := TTCPasswordManager.Create(nil, 0, 0, TNullLogger.Create, 0);
	Assert.IsNotNull(PasswordManager);
end;

initialization

TDUnitX.RegisterTestFixture(TTCPasswordManagerTest);

end.

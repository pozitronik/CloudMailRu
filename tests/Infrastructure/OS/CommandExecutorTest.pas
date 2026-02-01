unit CommandExecutorTest;

interface

uses
	CommandExecutor,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCommandExecutorTest = class
	public
		[Test]
		procedure TestImplementsInterface;
	end;

implementation

procedure TCommandExecutorTest.TestImplementsInterface;
var
	Executor: ICommandExecutor;
begin
	Executor := TWindowsCommandExecutor.Create;
	Assert.IsNotNull(Executor, 'TWindowsCommandExecutor should implement ICommandExecutor');
end;

initialization

TDUnitX.RegisterTestFixture(TCommandExecutorTest);

end.

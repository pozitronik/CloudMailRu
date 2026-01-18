unit NullProgressTest;

interface

uses
	IProgressInterface,
	DUnitX.TestFramework;

type
	[TestFixture]
	TNullProgressTest = class
	public
		[Test]
		{Verifies TNullProgress can be assigned to IProgress variable}
		procedure TestImplementsIProgress;

		[Test]
		{Verifies Progress with two names returns False (not cancelled)}
		procedure TestProgressWithTwoNamesReturnsFalse;

		[Test]
		{Verifies Progress with one name returns False (not cancelled)}
		procedure TestProgressWithOneNameReturnsFalse;

		[Test]
		{Verifies Progress with only percent returns False (not cancelled)}
		procedure TestProgressWithOnlyPercentReturnsFalse;

		[Test]
		{Verifies Aborted returns False}
		procedure TestAbortedReturnsFalse;

		[Test]
		{Verifies multiple sequential calls work correctly}
		procedure TestMultipleCalls;
	end;

implementation

procedure TNullProgressTest.TestImplementsIProgress;
var
	Progress: IProgress;
begin
	Progress := TNullProgress.Create;
	Assert.IsNotNull(Progress);
end;

procedure TNullProgressTest.TestProgressWithTwoNamesReturnsFalse;
var
	Progress: IProgress;
begin
	Progress := TNullProgress.Create;
	Assert.IsFalse(Progress.Progress('source', 'target', 50));
end;

procedure TNullProgressTest.TestProgressWithOneNameReturnsFalse;
var
	Progress: IProgress;
begin
	Progress := TNullProgress.Create;
	Assert.IsFalse(Progress.Progress('source', 50));
end;

procedure TNullProgressTest.TestProgressWithOnlyPercentReturnsFalse;
var
	Progress: IProgress;
begin
	Progress := TNullProgress.Create;
	Assert.IsFalse(Progress.Progress(50));
end;

procedure TNullProgressTest.TestAbortedReturnsFalse;
var
	Progress: IProgress;
begin
	Progress := TNullProgress.Create;
	Assert.IsFalse(Progress.Aborted());
end;

procedure TNullProgressTest.TestMultipleCalls;
var
	Progress: IProgress;
	i: Integer;
begin
	Progress := TNullProgress.Create;
	for i := 1 to 100 do
	begin
		Assert.IsFalse(Progress.Progress('file.txt', 'dest.txt', i));
		Assert.IsFalse(Progress.Progress('file.txt', i));
		Assert.IsFalse(Progress.Progress(i));
		Assert.IsFalse(Progress.Aborted());
	end;
	Assert.Pass('Multiple progress calls completed without exception');
end;

initialization

TDUnitX.RegisterTestFixture(TNullProgressTest);

end.

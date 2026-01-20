unit TCProgressTest;

interface

uses
	TCProgress,
	TestHelper,
	Plugin_TYPES,
	SysUtils,
	DUnitX.TestFramework;

type

	[TestFixture]
	TTCProgressTest = class
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;
		[Test]
		{Verifies TTCProgress implements IProgress interface}
		procedure TestImplementsIProgress;
		[Test]
		procedure TestProgress;
		[Test]
		procedure TestProgressNoTarget;
		[Test]
		procedure TestProgressNoSourceTarget;
		[Test]
		procedure TestProgressNoParams;
	end;

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

var
	PluginNr: Integer;
	SourceName: WideString;
	TargetName: WideString;
	PercentDone: Integer;

function TestProgressProc(Plugin_Nr: Integer; Source_Name, Target_Name: PWideChar; Percent_Done: Integer): Integer; stdcall;

implementation

{TTCProgressTest}

function TestProgressProc(Plugin_Nr: Integer; Source_Name, Target_Name: PWideChar; Percent_Done: Integer): Integer;
begin
	PluginNr := Plugin_Nr;
	SourceName := Source_Name;
	TargetName := Target_Name;
	PercentDone := Percent_Done;
	result := 0;
end;

procedure TTCProgressTest.Setup;
begin
	PluginNr := 0;
	SourceName := '';
	TargetName := '';
	PercentDone := 0;
	randomize;
end;

procedure TTCProgressTest.TearDown;
begin

end;

procedure TTCProgressTest.TestImplementsIProgress;
var
	Progress: IProgress;
begin
	Progress := TTCProgress.Create(TestProgressProc, 1);
	Assert.IsNotNull(Progress);
end;

procedure TTCProgressTest.TestProgress;
var
	TestTCProgress: TTCProgress;
	randomPN: Integer;
	RandomSourceName: WideString;
	RandomTargetName: WideString;
	RandomProgress: Integer;
begin
	Assert.AreEqual(0, PluginNr);
	Assert.AreEqual(0, PercentDone);
	Assert.AreEqual('', SourceName);
	Assert.AreEqual('', TargetName);

	randomPN := Random(100);
	RandomProgress := Random(100);
	RandomSourceName := RandomString(32);
	RandomTargetName := RandomString(64);

	TestTCProgress := TTCProgress.Create(TestProgressProc, randomPN);

	Assert.IsFalse(TestTCProgress.Progress(RandomSourceName, RandomTargetName, RandomProgress));

	Assert.AreEqual(randomPN, PluginNr);
	Assert.AreEqual(RandomProgress, PercentDone);
	Assert.AreEqual(RandomSourceName, SourceName);
	Assert.AreEqual(RandomTargetName, TargetName);

end;

procedure TTCProgressTest.TestProgressNoParams;
var
	TestTCProgress: TTCProgress;
	randomPN: Integer;
begin
	PercentDone := 50;
	Assert.AreEqual(0, PluginNr);
	Assert.AreEqual(50, PercentDone);
	Assert.AreEqual('', SourceName);
	Assert.AreEqual('', TargetName);

	randomPN := Random(100);

	TestTCProgress := TTCProgress.Create(TestProgressProc, randomPN);

	Assert.IsFalse(TestTCProgress.Progress(0));

	Assert.AreEqual(randomPN, PluginNr);
	Assert.AreEqual(0, PercentDone); {should be reset to the default value}
	Assert.AreEqual('', SourceName);
	Assert.AreEqual('', TargetName);

end;

procedure TTCProgressTest.TestProgressNoSourceTarget;
var
	TestTCProgress: TTCProgress;
	randomPN: Integer;
	RandomProgress: Integer;
begin
	Assert.AreEqual(0, PluginNr);
	Assert.AreEqual(0, PercentDone);
	Assert.AreEqual('', SourceName);
	Assert.AreEqual('', TargetName);

	randomPN := Random(100);
	RandomProgress := Random(100);

	TestTCProgress := TTCProgress.Create(TestProgressProc, randomPN);
	Assert.IsFalse(TestTCProgress.Progress(RandomProgress));

	Assert.AreEqual(randomPN, PluginNr);
	Assert.AreEqual(RandomProgress, PercentDone);
	Assert.AreEqual('', SourceName);
	Assert.AreEqual('', TargetName);
end;

procedure TTCProgressTest.TestProgressNoTarget;
var
	TestTCProgress: TTCProgress;
	randomPN: Integer;
	RandomSourceName: WideString;

	RandomProgress: Integer;
begin
	Assert.AreEqual(0, PluginNr);
	Assert.AreEqual(0, PercentDone);
	Assert.AreEqual('', SourceName);
	Assert.AreEqual('', TargetName);

	randomPN := Random(100);
	RandomProgress := Random(100);
	RandomSourceName := RandomString(32);

	TestTCProgress := TTCProgress.Create(TestProgressProc, randomPN);

	Assert.IsFalse(TestTCProgress.Progress(RandomSourceName, RandomProgress));

	Assert.AreEqual(randomPN, PluginNr);
	Assert.AreEqual(RandomProgress, PercentDone);
	Assert.AreEqual(RandomSourceName, SourceName);
	Assert.AreEqual('', TargetName);

end;

{TNullProgressTest}

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

TDUnitX.RegisterTestFixture(TTCProgressTest);
TDUnitX.RegisterTestFixture(TNullProgressTest);

end.

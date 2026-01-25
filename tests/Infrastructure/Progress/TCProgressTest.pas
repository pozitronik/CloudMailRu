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

	[TestFixture]
	TScaledProgressTest = class
	public
		[Test]
		{Verifies TScaledProgress implements IProgress}
		procedure TestImplementsIProgress;

		[Test]
		{Without scaling, percentage passes through unchanged}
		procedure TestNoScaling_PassesThrough;

		[Test]
		{With single part (TotalParts=1), no scaling applied}
		procedure TestSinglePart_NoScaling;

		[Test]
		{First part of 5 at 50% = (0*100+50)/5 = 10%}
		procedure TestFirstPartOfFive_At50Percent;

		[Test]
		{Third part of 5 at 50% = (2*100+50)/5 = 50%}
		procedure TestThirdPartOfFive_At50Percent;

		[Test]
		{Last part of 5 at 100% = (4*100+100)/5 = 100%}
		procedure TestLastPartOfFive_At100Percent;

		[Test]
		{ResetScale disables scaling}
		procedure TestResetScale;

		[Test]
		{Aborted delegates to inner progress}
		procedure TestAborted_Delegates;
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

{TScaledProgressTest}

procedure TScaledProgressTest.TestImplementsIProgress;
var
	Progress: IProgress;
begin
	Progress := TScaledProgress.Create(TNullProgress.Create);
	Assert.IsNotNull(Progress);
end;

procedure TScaledProgressTest.TestNoScaling_PassesThrough;
var
	Scaled: TScaledProgress;
begin
	PercentDone := 0;
	Scaled := TScaledProgress.Create(TTCProgress.Create(TestProgressProc, 1));
	{No SetScale called - defaults to 0, 0}
	Scaled.Progress('src', 'dst', 75);
	Assert.AreEqual(75, PercentDone);
end;

procedure TScaledProgressTest.TestSinglePart_NoScaling;
var
	Scaled: TScaledProgress;
begin
	PercentDone := 0;
	Scaled := TScaledProgress.Create(TTCProgress.Create(TestProgressProc, 1));
	Scaled.SetScale(0, 1); {Single part - no scaling}
	Scaled.Progress('src', 'dst', 50);
	Assert.AreEqual(50, PercentDone);
end;

procedure TScaledProgressTest.TestFirstPartOfFive_At50Percent;
var
	Scaled: TScaledProgress;
begin
	PercentDone := 0;
	Scaled := TScaledProgress.Create(TTCProgress.Create(TestProgressProc, 1));
	Scaled.SetScale(0, 5); {First part (index 0) of 5}
	Scaled.Progress('src', 'dst', 50);
	{Expected: (0*100 + 50) / 5 = 10}
	Assert.AreEqual(10, PercentDone);
end;

procedure TScaledProgressTest.TestThirdPartOfFive_At50Percent;
var
	Scaled: TScaledProgress;
begin
	PercentDone := 0;
	Scaled := TScaledProgress.Create(TTCProgress.Create(TestProgressProc, 1));
	Scaled.SetScale(2, 5); {Third part (index 2) of 5}
	Scaled.Progress('src', 'dst', 50);
	{Expected: (2*100 + 50) / 5 = 50}
	Assert.AreEqual(50, PercentDone);
end;

procedure TScaledProgressTest.TestLastPartOfFive_At100Percent;
var
	Scaled: TScaledProgress;
begin
	PercentDone := 0;
	Scaled := TScaledProgress.Create(TTCProgress.Create(TestProgressProc, 1));
	Scaled.SetScale(4, 5); {Last part (index 4) of 5}
	Scaled.Progress('src', 'dst', 100);
	{Expected: (4*100 + 100) / 5 = 100}
	Assert.AreEqual(100, PercentDone);
end;

procedure TScaledProgressTest.TestResetScale;
var
	Scaled: TScaledProgress;
begin
	PercentDone := 0;
	Scaled := TScaledProgress.Create(TTCProgress.Create(TestProgressProc, 1));
	Scaled.SetScale(2, 5); {Enable scaling}
	Scaled.ResetScale; {Disable scaling}
	Scaled.Progress('src', 'dst', 75);
	{After reset, should pass through unchanged}
	Assert.AreEqual(75, PercentDone);
end;

procedure TScaledProgressTest.TestAborted_Delegates;
var
	Scaled: TScaledProgress;
begin
	Scaled := TScaledProgress.Create(TNullProgress.Create);
	{TNullProgress.Aborted returns False}
	Assert.IsFalse(Scaled.Aborted);
end;

initialization

TDUnitX.RegisterTestFixture(TTCProgressTest);
TDUnitX.RegisterTestFixture(TNullProgressTest);
TDUnitX.RegisterTestFixture(TScaledProgressTest);

end.

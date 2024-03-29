﻿unit TCProgressTest;

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
		procedure TestProgress;
		[Test]
		procedure TestProgressNoTarget;
		[Test]
		procedure TestProgressNoSourceTarget;
		[Test]
		procedure TestProgressNoParams;
		[Test]
		procedure TestCreateDummy;
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

procedure TTCProgressTest.TestCreateDummy;
var
	TestTCProgress: TTCProgress;
	RandomSourceName: WideString;
	RandomTargetName: WideString;
	RandomProgress: Integer;
begin
	Assert.AreEqual(0, PluginNr);
	Assert.AreEqual(0, PercentDone);
	Assert.AreEqual('', SourceName);
	Assert.AreEqual('', TargetName);

	RandomProgress := Random(100);
	RandomSourceName := RandomString(32);
	RandomTargetName := RandomString(64);

	TestTCProgress := TTCProgress.Create();

	Assert.IsFalse(TestTCProgress.Progress(RandomSourceName, RandomTargetName, RandomProgress));
	Assert.AreEqual(0, PluginNr);
	Assert.AreEqual(0, PercentDone);
	Assert.AreEqual('', SourceName);
	Assert.AreEqual('', TargetName);
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

	Assert.IsFalse(TestTCProgress.Progress());

	Assert.AreEqual(randomPN, PluginNr);
	Assert.AreEqual(0, PercentDone); //should be reset to the default value
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

initialization

TDUnitX.RegisterTestFixture(TTCProgressTest);

end.

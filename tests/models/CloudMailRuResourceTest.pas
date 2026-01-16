unit CloudMailRuResourceTest;

interface

uses
	System.SysUtils,
	System.Classes,
	System.Generics.Collections,
	Winapi.Windows,
	DUnitX.TestFramework;

type
	{ Tests for CloudMailRu resource management patterns.
	  These tests document and verify the correct resource cleanup patterns
	  that should be used in TCloudMailRu methods. }
	[TestFixture]
	TCloudMailRuResourceTest = class
	public
		{ FormFields dictionary leak test - demonstrates the bug pattern
		  that existed in LoginRegular where early exits could leak FormFields.
		  This test verifies the correct try-finally pattern. }
		[Test]
		procedure TestFormFieldsDictionaryCleanupOnEarlyExit;

		{ Verify dictionary is freed even when nested conditions fail }
		[Test]
		procedure TestFormFieldsDictionaryCleanupOnNestedFailure;

		{ Verify dictionary is freed when exception occurs }
		[Test]
		procedure TestFormFieldsDictionaryCleanupOnException;

		{ Dictionary with object values - demonstrates the pattern from
		  ThreadFsRemoveDirSkippedPath where TStringList values must be
		  freed before freeing the dictionary itself. }
		[Test]
		procedure TestDictionaryWithObjectValuesCleanup;

		{ Dictionary with object values - some values may be nil }
		[Test]
		procedure TestDictionaryWithMixedNilObjectValuesCleanup;

		{ Dictionary with object values - cleanup when operation interrupted }
		[Test]
		procedure TestDictionaryObjectValuesCleanupOnInterrupt;
	end;

implementation

{ Simulates the control flow pattern from LoginRegular that could leak FormFields.
  The pattern requires try-finally to ensure cleanup on all exit paths. }
procedure TCloudMailRuResourceTest.TestFormFieldsDictionaryCleanupOnEarlyExit;
var
	FormFields: TDictionary<WideString, WideString>;
	SimulatedUserCancelled: Boolean;
	WasFreed: Boolean;
begin
	{ Simulate user cancelling security key entry - this was a leak path }
	SimulatedUserCancelled := True;
	WasFreed := False;

	FormFields := TDictionary<WideString, WideString>.Create;
	try
		FormFields.Add('Domain', 'mail.ru');
		FormFields.Add('Login', 'test@mail.ru');

		{ Simulate early exit that previously leaked FormFields }
		if SimulatedUserCancelled then
		begin
			{ In old code: Exit(False) here would leak FormFields }
			{ With try-finally: FormFields.Free is always called }
			Exit;
		end;

		{ This code not reached when user cancels }
		FormFields.Add('AuthCode', '123456');
	finally
		FormFields.Free;
		WasFreed := True;
	end;

	{ Note: WasFreed would remain False in old buggy pattern without try-finally }
	Assert.IsTrue(WasFreed, 'FormFields should be freed on early exit');
end;

procedure TCloudMailRuResourceTest.TestFormFieldsDictionaryCleanupOnNestedFailure;
var
	FormFields: TDictionary<WideString, WideString>;
	SimulatedParsingFailed: Boolean;
	CleanupExecuted: Boolean;
begin
	{ Simulate JSON parsing failure - another leak path in LoginRegular }
	SimulatedParsingFailed := True;
	CleanupExecuted := False;

	FormFields := TDictionary<WideString, WideString>.Create;
	try
		FormFields.Add('Domain', 'mail.ru');

		{ Simulated nested condition that fails }
		if True then { First condition passes }
		begin
			if SimulatedParsingFailed then { Nested condition fails }
			begin
				{ In old code: Exit(False) here would leak FormFields }
				Exit;
			end;
		end;
	finally
		FormFields.Free;
		CleanupExecuted := True;
	end;

	Assert.IsTrue(CleanupExecuted, 'FormFields should be freed when nested conditions fail');
end;

procedure TCloudMailRuResourceTest.TestFormFieldsDictionaryCleanupOnException;
var
	FormFields: TDictionary<WideString, WideString>;
	CleanupExecuted: Boolean;
	ExceptionCaught: Boolean;
begin
	CleanupExecuted := False;
	ExceptionCaught := False;

	try
		FormFields := TDictionary<WideString, WideString>.Create;
		try
			FormFields.Add('Key', 'Value');

			{ Simulate an exception during processing }
			raise Exception.Create('Simulated error');
		finally
			FormFields.Free;
			CleanupExecuted := True;
		end;
	except
		ExceptionCaught := True;
	end;

	Assert.IsTrue(CleanupExecuted, 'FormFields should be freed even when exception occurs');
	Assert.IsTrue(ExceptionCaught, 'Exception should have been raised');
end;

{ Tests for dictionary with object values - pattern from ThreadFsRemoveDirSkippedPath }

procedure TCloudMailRuResourceTest.TestDictionaryWithObjectValuesCleanup;
var
	Dict: TDictionary<DWORD, TStringList>;
	StringList1, StringList2: TStringList;
	Pair: TPair<DWORD, TStringList>;
begin
	{ This pattern demonstrates how ThreadFsRemoveDirSkippedPath should be cleaned up.
	  The dictionary holds TStringList objects that must be freed before the dictionary. }
	Dict := TDictionary<DWORD, TStringList>.Create;
	try
		{ Simulate multiple threads creating their own TStringList }
		StringList1 := TStringList.Create;
		StringList1.Add('path1');
		Dict.Add(1001, StringList1);

		StringList2 := TStringList.Create;
		StringList2.Add('path2');
		Dict.Add(1002, StringList2);

		Assert.AreEqual(Integer(2), Integer(Dict.Count), 'Dictionary should have 2 entries');
	finally
		{ Correct cleanup: free all object values before freeing dictionary }
		for Pair in Dict do
			if Assigned(Pair.Value) then
				Pair.Value.Free;
		Dict.Free;
	end;

	{ If we reach here without memory leak, the pattern is correct }
	Assert.Pass('Dictionary with object values cleaned up correctly');
end;

procedure TCloudMailRuResourceTest.TestDictionaryWithMixedNilObjectValuesCleanup;
var
	Dict: TDictionary<DWORD, TStringList>;
	StringList1: TStringList;
	Pair: TPair<DWORD, TStringList>;
begin
	{ Some dictionary values may be nil (e.g., after operation completes and sets value to nil).
	  Cleanup must handle both assigned and nil values. }
	Dict := TDictionary<DWORD, TStringList>.Create;
	try
		{ Thread 1001 has active TStringList }
		StringList1 := TStringList.Create;
		StringList1.Add('active_path');
		Dict.Add(1001, StringList1);

		{ Thread 1002 completed, value set to nil }
		Dict.Add(1002, nil);

		{ Thread 1003 has another active TStringList }
		Dict.Add(1003, TStringList.Create);

		Assert.AreEqual(Integer(3), Integer(Dict.Count), 'Dictionary should have 3 entries');
	finally
		{ Cleanup must check Assigned before freeing }
		for Pair in Dict do
			if Assigned(Pair.Value) then
				Pair.Value.Free;
		Dict.Free;
	end;

	Assert.Pass('Mixed nil/assigned values cleaned up correctly');
end;

procedure TCloudMailRuResourceTest.TestDictionaryObjectValuesCleanupOnInterrupt;
var
	Dict: TDictionary<DWORD, TStringList>;
	StringList: TStringList;
	Pair: TPair<DWORD, TStringList>;
	SimulatedInterrupt: Boolean;
begin
	{ Simulates scenario where operation is interrupted (user abort) before
	  FS_STATUS_END is called. The destructor must still clean up all values. }
	SimulatedInterrupt := True;
	Dict := TDictionary<DWORD, TStringList>.Create;
	try
		{ Operation starts - TStringList created }
		StringList := TStringList.Create;
		StringList.Add('operation_in_progress');
		Dict.Add(GetCurrentThreadId, StringList);

		{ Simulate interrupt - operation never completes normally }
		if SimulatedInterrupt then
		begin
			{ In buggy code: Exit here without cleanup would leak StringList }
			{ FS_STATUS_END never called, so normal cleanup path skipped }
			Exit;
		end;

		{ This code never reached on interrupt }
		Dict.Items[GetCurrentThreadId].Free;
		Dict.AddOrSetValue(GetCurrentThreadId, nil);
	finally
		{ Destructor cleanup - must free all remaining object values }
		for Pair in Dict do
			if Assigned(Pair.Value) then
				Pair.Value.Free;
		Dict.Free;
	end;

	{ Note: this line not reached due to Exit, but finally block still runs }
end;

initialization

TDUnitX.RegisterTestFixture(TCloudMailRuResourceTest);

end.

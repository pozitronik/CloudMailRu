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

		{ HTTP stream cleanup patterns - demonstrates correct cleanup when
		  multiple streams are created for HTTP POST operations.
		  Pattern used in CloudMailRuHTTP.PostFile, PostForm, PostMultipart. }
		[Test]
		procedure TestMultipleStreamsCleanupOnException;

		{ Two streams cleanup - simulates PostForm pattern with ResultStream and PostData }
		[Test]
		procedure TestTwoStreamsCleanupOnException;

		{ Cleanup with operation that may throw - simulates HTTP Post that throws }
		[Test]
		procedure TestStreamCleanupWhenOperationThrows;

		{ Nested streams with recursive exit - simulates GetFileRegular pattern
		  where FileStream and MemoryStream must be freed on recursive retry }
		[Test]
		procedure TestNestedStreamsCleanupOnRecursiveExit;
	end;

implementation

{ Simulates the control flow pattern from LoginRegular that could leak FormFields.
  The pattern requires try-finally to ensure cleanup on all exit paths. }
procedure TCloudMailRuResourceTest.TestFormFieldsDictionaryCleanupOnEarlyExit;
var
	FormFields: TDictionary<WideString, WideString>;
	SimulatedUserCancelled: Boolean;
begin
	{ Simulate user cancelling security key entry - this was a leak path.
	  FastMM5 will detect if FormFields leaks due to missing try-finally. }
	SimulatedUserCancelled := True;

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
	end;
end;

procedure TCloudMailRuResourceTest.TestFormFieldsDictionaryCleanupOnNestedFailure;
var
	FormFields: TDictionary<WideString, WideString>;
	SimulatedParsingFailed: Boolean;
begin
	{ Simulate JSON parsing failure - another leak path in LoginRegular.
	  FastMM5 will detect if FormFields leaks due to missing try-finally. }
	SimulatedParsingFailed := True;

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
	end;
end;

procedure TCloudMailRuResourceTest.TestFormFieldsDictionaryCleanupOnException;
var
	FormFields: TDictionary<WideString, WideString>;
	CleanupExecuted: Boolean;
begin
	CleanupExecuted := False;

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
		{ Exception caught - this is expected }
	end;

	Assert.IsTrue(CleanupExecuted, 'FormFields should be freed even when exception occurs');
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

{ HTTP stream cleanup pattern tests.
  These tests document the correct try-finally pattern that must be used
  in CloudMailRuHTTP methods like PostFile, PostForm, PostMultipart.
  FastMM5 will detect leaks if streams are not properly freed. }

procedure TCloudMailRuResourceTest.TestMultipleStreamsCleanupOnException;
var
	ResultStream: TStringStream;
	PostData: TStringStream;
begin
	{ Simulates PostForm pattern where two TStringStream objects are created.
	  Both must be freed even if an operation between them throws.
	  The correct pattern is:
	    ResultStream := TStringStream.Create;
	    try
	      PostData := TStringStream.Create(...);
	      try
	        // operations that may throw
	      finally
	        PostData.Free;
	      end;
	    finally
	      ResultStream.Free;
	    end;
	  FastMM5 will detect if streams leak due to missing try-finally.
	}

	try
		ResultStream := TStringStream.Create;
		try
			PostData := TStringStream.Create('test data', TEncoding.UTF8);
			try
				{ Simulate operation that throws }
				raise Exception.Create('Simulated HTTP error');
			finally
				PostData.Free;
			end;
		finally
			ResultStream.Free;
		end;
	except
		{ Exception caught - this is expected }
	end;

	{ If we reach here without memory leak (FastMM5 would report), pattern is correct }
	Assert.Pass('Multiple streams cleaned up correctly on exception');
end;

procedure TCloudMailRuResourceTest.TestTwoStreamsCleanupOnException;
var
	Stream1, Stream2: TStringStream;
begin
	{ Tests the simpler pattern where both streams are created upfront.
	  This is what CloudMailRuHTTP currently does (without try-finally).
	  The fix requires nested try-finally blocks.
	  FastMM5 will detect if streams leak due to missing try-finally. }

	Stream1 := TStringStream.Create;
	try
		Stream2 := TStringStream.Create;
		try
			Stream1.WriteString('data1');
			Stream2.WriteString('data2');

			{ Simulate early exit that previously leaked both streams }
			Exit;
		finally
			Stream2.Free;
		end;
	finally
		Stream1.Free;
	end;
end;

procedure TCloudMailRuResourceTest.TestStreamCleanupWhenOperationThrows;
var
	ResultStream: TStringStream;
	OperationResult: Integer;
	CleanupExecuted: Boolean;

	{ Simulates an operation like HTTP.Post that may throw }
	function SimulatedPostOperation(Data: TStringStream): Integer;
	begin
		raise Exception.Create('Network error');
	end;

begin
	{ This pattern simulates PostFile/PutFile where:
	    ResultStream := TStringStream.Create;
	    result := self.Post(URL, PostData, ResultStream);  // May throw!
	    Answer := ResultStream.DataString;
	    ResultStream.free;  // Never reached if Post throws!

	  Correct pattern:
	    ResultStream := TStringStream.Create;
	    try
	      result := self.Post(URL, PostData, ResultStream);
	      Answer := ResultStream.DataString;
	    finally
	      ResultStream.Free;  // Always executed
	    end;
	}
	CleanupExecuted := False;
	OperationResult := -1;

	try
		ResultStream := TStringStream.Create;
		try
			OperationResult := SimulatedPostOperation(ResultStream);
		finally
			ResultStream.Free;
			CleanupExecuted := True;
		end;
	except
		{ Swallow exception for test }
	end;

	Assert.IsTrue(CleanupExecuted, 'Stream should be freed even when operation throws');
	Assert.AreEqual(-1, OperationResult, 'Operation result should remain -1 (never assigned)');
end;

procedure TCloudMailRuResourceTest.TestNestedStreamsCleanupOnRecursiveExit;
var
	FileStream: TStringStream;
	MemoryStream: TMemoryStream;
	NeedsRetry: Boolean;
begin
	{ This pattern simulates GetFileRegular where:
	    FileStream := TBufferedFileStream.Create(...);
	    if FDoCryptFiles then
	    begin
	      MemoryStream := TMemoryStream.Create;
	      Result := HTTP.GetFile(..., MemoryStream);
	      if (token_outdated) then
	        Result := GetFileRegular(...);  // Recursive! Old streams leak!
	      ...
	      MemoryStream.free;  // Never reached if recursion happens
	    end
	    FileStream.free;  // Never reached if recursion happens

	  Correct pattern:
	    FileStream := TBufferedFileStream.Create(...);
	    try
	      if FDoCryptFiles then
	      begin
	        MemoryStream := TMemoryStream.Create;
	        try
	          Result := HTTP.GetFile(..., MemoryStream);
	          if (token_outdated) then
	            Exit(GetFileRegular(...));  // Exit after recursive call!
	          ...
	        finally
	          MemoryStream.Free;
	        end;
	      end
	    finally
	      FileStream.Free;
	    end;
	  FastMM5 will detect if streams leak due to missing try-finally.
	}
	NeedsRetry := True;

	FileStream := TStringStream.Create;
	try
		MemoryStream := TMemoryStream.Create;
		try
			{ Simulate condition that triggers retry }
			if NeedsRetry then
			begin
				{ With Exit, finally blocks still execute }
				Exit;
			end;

			{ This code never reached on retry }
			MemoryStream.WriteBuffer(FileStream, 0);
		finally
			MemoryStream.Free;
		end;
	finally
		FileStream.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TCloudMailRuResourceTest);

end.

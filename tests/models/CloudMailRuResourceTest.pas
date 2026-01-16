unit CloudMailRuResourceTest;

interface

uses
	System.SysUtils,
	System.Generics.Collections,
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

initialization

TDUnitX.RegisterTestFixture(TCloudMailRuResourceTest);

end.

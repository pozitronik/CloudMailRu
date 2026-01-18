unit NullPasswordUIProviderTest;

interface

uses
	IPasswordUIProviderInterface,
	System.Generics.Collections,
	System.UITypes,
	Windows,
	DUnitX.TestFramework;

type
	[TestFixture]
	TNullPasswordUIProviderTest = class
	public
		[Test]
		{Verifies TNullPasswordUIProvider can be assigned to IPasswordUIProvider variable}
		procedure TestImplementsIPasswordUIProvider;

		[Test]
		{Verifies AskPassword returns mrCancel}
		procedure TestAskPasswordReturnsMrCancel;

		[Test]
		{Verifies AskPassword does not modify Password parameter}
		procedure TestAskPasswordDoesNotModifyPassword;

		[Test]
		{Verifies AskPassword does not modify UseTCPwdMngr parameter}
		procedure TestAskPasswordDoesNotModifyUseTCPwdMngr;

		[Test]
		{Verifies AskAction returns mrCancel}
		procedure TestAskActionReturnsMrCancel;

		[Test]
		{Verifies multiple calls work correctly}
		procedure TestMultipleCalls;
	end;

implementation

procedure TNullPasswordUIProviderTest.TestImplementsIPasswordUIProvider;
var
	Provider: IPasswordUIProvider;
begin
	Provider := TNullPasswordUIProvider.Create;
	Assert.IsNotNull(Provider);
end;

procedure TNullPasswordUIProviderTest.TestAskPasswordReturnsMrCancel;
var
	Provider: IPasswordUIProvider;
	Password: WideString;
	UseTC: Boolean;
	Result: Integer;
begin
	Provider := TNullPasswordUIProvider.Create;
	Password := '';
	UseTC := False;

	Result := Provider.AskPassword('Title', 'Text', Password, UseTC, False, 0);

	Assert.AreEqual(mrCancel, Result);
end;

procedure TNullPasswordUIProviderTest.TestAskPasswordDoesNotModifyPassword;
var
	Provider: IPasswordUIProvider;
	Password: WideString;
	UseTC: Boolean;
begin
	Provider := TNullPasswordUIProvider.Create;
	Password := 'original_password';
	UseTC := False;

	Provider.AskPassword('Title', 'Text', Password, UseTC, False, 0);

	Assert.AreEqual('original_password', Password, 'Password should not be modified on cancel');
end;

procedure TNullPasswordUIProviderTest.TestAskPasswordDoesNotModifyUseTCPwdMngr;
var
	Provider: IPasswordUIProvider;
	Password: WideString;
	UseTC: Boolean;
begin
	Provider := TNullPasswordUIProvider.Create;
	Password := '';
	UseTC := True;

	Provider.AskPassword('Title', 'Text', Password, UseTC, False, 0);

	Assert.IsTrue(UseTC, 'UseTCPwdMngr should not be modified on cancel');
end;

procedure TNullPasswordUIProviderTest.TestAskActionReturnsMrCancel;
var
	Provider: IPasswordUIProvider;
	Actions: TDictionary<Int32, WideString>;
	Result: Integer;
begin
	Provider := TNullPasswordUIProvider.Create;
	Actions := TDictionary<Int32, WideString>.Create;
	try
		Actions.Add(mrYes, 'Yes');
		Actions.Add(mrNo, 'No');

		Result := Provider.AskAction('Title', 'Text', Actions, 0);

		Assert.AreEqual(mrCancel, Result);
	finally
		Actions.Free;
	end;
end;

procedure TNullPasswordUIProviderTest.TestMultipleCalls;
var
	Provider: IPasswordUIProvider;
	Password: WideString;
	UseTC: Boolean;
	Actions: TDictionary<Int32, WideString>;
begin
	Provider := TNullPasswordUIProvider.Create;
	Password := '';
	UseTC := False;

	Assert.AreEqual(mrCancel, Provider.AskPassword('T1', 'M1', Password, UseTC, False, 0));
	Assert.AreEqual(mrCancel, Provider.AskPassword('T2', 'M2', Password, UseTC, True, 0));

	Actions := TDictionary<Int32, WideString>.Create;
	try
		Actions.Add(mrOk, 'OK');
		Assert.AreEqual(mrCancel, Provider.AskAction('T3', 'M3', Actions, 0));
	finally
		Actions.Free;
	end;

	Assert.Pass('Multiple calls completed without exception');
end;

initialization

TDUnitX.RegisterTestFixture(TNullPasswordUIProviderTest);

end.

unit CloudAuthorizationStateTest;

interface

uses
	CloudAuthorizationState,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCloudAuthorizationStateTest = class
	public
		{TAuthorizationError.Empty}
		[Test]
		procedure TestEmptyErrorCodeIsNone;
		[Test]
		procedure TestEmptyErrorMessageIsEmpty;

		{TAuthorizationError.Create(Code, Message)}
		[Test]
		procedure TestCreateWithCodeAndMessage;
		[Test]
		procedure TestCreateWithCodeAndEmptyMessage;
		[Test]
		procedure TestCreateWithInitFailed;
		[Test]
		procedure TestCreateWithAuthFailed;

		{TAuthorizationError.Create(Message) - single-param overload}
		[Test]
		procedure TestCreateWithMessageOnlyDefaultsToUnknown;
		[Test]
		procedure TestCreateWithEmptyMessageDefaultsToUnknown;

		{TAuthorizationState enum values}
		[Test]
		procedure TestAuthorizationStateValues;
	end;

implementation

{TAuthorizationError.Empty}

procedure TCloudAuthorizationStateTest.TestEmptyErrorCodeIsNone;
var
	Error: TAuthorizationError;
begin
	Error := TAuthorizationError.Empty;
	Assert.AreEqual(Ord(aecNone), Ord(Error.ErrorCode));
end;

procedure TCloudAuthorizationStateTest.TestEmptyErrorMessageIsEmpty;
var
	Error: TAuthorizationError;
begin
	Error := TAuthorizationError.Empty;
	Assert.AreEqual('', Error.ErrorMessage);
end;

{TAuthorizationError.Create(Code, Message)}

procedure TCloudAuthorizationStateTest.TestCreateWithCodeAndMessage;
var
	Error: TAuthorizationError;
begin
	Error := TAuthorizationError.Create(aecAuthFailed, 'Invalid password');
	Assert.AreEqual(Ord(aecAuthFailed), Ord(Error.ErrorCode));
	Assert.AreEqual('Invalid password', Error.ErrorMessage);
end;

procedure TCloudAuthorizationStateTest.TestCreateWithCodeAndEmptyMessage;
var
	Error: TAuthorizationError;
begin
	Error := TAuthorizationError.Create(aecInitFailed, '');
	Assert.AreEqual(Ord(aecInitFailed), Ord(Error.ErrorCode));
	Assert.AreEqual('', Error.ErrorMessage);
end;

procedure TCloudAuthorizationStateTest.TestCreateWithInitFailed;
var
	Error: TAuthorizationError;
begin
	Error := TAuthorizationError.Create(aecInitFailed, 'Network timeout');
	Assert.AreEqual(Ord(aecInitFailed), Ord(Error.ErrorCode));
	Assert.AreEqual('Network timeout', Error.ErrorMessage);
end;

procedure TCloudAuthorizationStateTest.TestCreateWithAuthFailed;
var
	Error: TAuthorizationError;
begin
	Error := TAuthorizationError.Create(aecAuthFailed, 'Token expired');
	Assert.AreEqual(Ord(aecAuthFailed), Ord(Error.ErrorCode));
	Assert.AreEqual('Token expired', Error.ErrorMessage);
end;

{TAuthorizationError.Create(Message) - single-param overload defaults to aecUnknown}

procedure TCloudAuthorizationStateTest.TestCreateWithMessageOnlyDefaultsToUnknown;
var
	Error: TAuthorizationError;
begin
	Error := TAuthorizationError.Create('Something went wrong');
	Assert.AreEqual(Ord(aecUnknown), Ord(Error.ErrorCode));
	Assert.AreEqual('Something went wrong', Error.ErrorMessage);
end;

procedure TCloudAuthorizationStateTest.TestCreateWithEmptyMessageDefaultsToUnknown;
var
	Error: TAuthorizationError;
begin
	Error := TAuthorizationError.Create('');
	Assert.AreEqual(Ord(aecUnknown), Ord(Error.ErrorCode));
	Assert.AreEqual('', Error.ErrorMessage);
end;

{TAuthorizationState enum - verify all states exist with expected ordinal values}

procedure TCloudAuthorizationStateTest.TestAuthorizationStateValues;
begin
	Assert.AreEqual(0, Ord(asPending));
	Assert.AreEqual(1, Ord(asAuthorizing));
	Assert.AreEqual(2, Ord(asAuthorized));
	Assert.AreEqual(3, Ord(asFailed));
end;

initialization

TDUnitX.RegisterTestFixture(TCloudAuthorizationStateTest);

end.

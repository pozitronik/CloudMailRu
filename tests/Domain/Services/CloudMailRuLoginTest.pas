unit CloudMailRuLoginTest;

{Tests for TMockAuthStrategy functionality.
 Tests authentication mock behavior for testing TCloudMailRu login flows.}

interface

uses
	AuthStrategy,
	MockAuthStrategy,
	CMROAuth,
	System.SysUtils,
	DUnitX.TestFramework;

type
	[TestFixture]
	TCloudMailRuLoginTest = class
	public
		{TMockAuthStrategy tests}
		[Test]
		procedure TestMockAuthStrategy_CreateSuccess_ReturnsToken;
		[Test]
		procedure TestMockAuthStrategy_CreateFailure_ReturnsError;
		[Test]
		procedure TestMockAuthStrategy_TracksCallCount;
		[Test]
		procedure TestMockAuthStrategy_CapturesCredentials;
		[Test]
		procedure TestMockAuthStrategy_Reset_ClearsState;
		[Test]
		procedure TestMockAuthStrategy_SetSucceed_ChangesResult;
		[Test]
		procedure TestMockAuthStrategy_SetAccessToken_ChangesToken;
		[Test]
		procedure TestMockAuthStrategy_CreateSharedSuccess_ReturnsShardAndLink;
		[Test]
		procedure TestMockAuthStrategy_GetName_ReturnsMock;

		{TMockAuthStrategySequence tests}
		[Test]
		procedure TestMockAuthStrategySequence_ReturnsResultsInOrder;
		[Test]
		procedure TestMockAuthStrategySequence_RepeatLastAfterExhausted;
		[Test]
		procedure TestMockAuthStrategySequence_AddOAuthSuccess_IncludesAllFields;
		[Test]
		procedure TestMockAuthStrategySequence_Reset_RestartsSequence;
		[Test]
		procedure TestMockAuthStrategySequence_TracksCallCount;
		[Test]
		procedure TestMockAuthStrategySequence_EmptySequence_ReturnsFailure;
	end;

implementation

{TMockAuthStrategy tests}

procedure TCloudMailRuLoginTest.TestMockAuthStrategy_CreateSuccess_ReturnsToken;
var
	Strategy: TMockAuthStrategy;
	Result: TAuthResult;
	Credentials: TAuthCredentials;
begin
	Strategy := TMockAuthStrategy.CreateSuccess('test_token');
	try
		Credentials := TAuthCredentials.Create('test@mail.ru', 'pass', 'test', 'mail.ru');
		Result := Strategy.Authenticate(Credentials, nil, nil);

		Assert.IsTrue(Result.Success, 'Should succeed');
		Assert.AreEqual(String('test_token'), String(Result.AuthToken), 'Should return configured token');
	finally
		Strategy.Free;
	end;
end;

procedure TCloudMailRuLoginTest.TestMockAuthStrategy_CreateFailure_ReturnsError;
var
	Strategy: TMockAuthStrategy;
	Result: TAuthResult;
	Credentials: TAuthCredentials;
begin
	Strategy := TMockAuthStrategy.CreateFailure('Test error message');
	try
		Credentials := TAuthCredentials.Create('test@mail.ru', 'pass', 'test', 'mail.ru');
		Result := Strategy.Authenticate(Credentials, nil, nil);

		Assert.IsFalse(Result.Success, 'Should fail');
		Assert.AreEqual(String('Test error message'), String(Result.ErrorMessage), 'Should return error message');
	finally
		Strategy.Free;
	end;
end;

procedure TCloudMailRuLoginTest.TestMockAuthStrategy_TracksCallCount;
var
	Strategy: TMockAuthStrategy;
	Credentials: TAuthCredentials;
begin
	Strategy := TMockAuthStrategy.CreateSuccess('token');
	try
		Credentials := TAuthCredentials.Create('test@mail.ru', 'pass', 'test', 'mail.ru');

		Assert.AreEqual(0, Strategy.CallCount, 'Initial call count should be 0');

		Strategy.Authenticate(Credentials, nil, nil);
		Assert.AreEqual(1, Strategy.CallCount, 'Call count should be 1 after first call');

		Strategy.Authenticate(Credentials, nil, nil);
		Assert.AreEqual(2, Strategy.CallCount, 'Call count should be 2 after second call');
	finally
		Strategy.Free;
	end;
end;

procedure TCloudMailRuLoginTest.TestMockAuthStrategy_CapturesCredentials;
var
	Strategy: TMockAuthStrategy;
	Credentials: TAuthCredentials;
begin
	Strategy := TMockAuthStrategy.CreateSuccess('token');
	try
		Credentials := TAuthCredentials.Create('user@domain.com', 'secret', 'user', 'domain.com');
		Strategy.Authenticate(Credentials, nil, nil);

		Assert.AreEqual(String('user@domain.com'), String(Strategy.LastCredentials.Email), 'Should capture email');
		Assert.AreEqual(String('secret'), String(Strategy.LastCredentials.Password), 'Should capture password');
	finally
		Strategy.Free;
	end;
end;

procedure TCloudMailRuLoginTest.TestMockAuthStrategy_Reset_ClearsState;
var
	Strategy: TMockAuthStrategy;
	Credentials: TAuthCredentials;
begin
	Strategy := TMockAuthStrategy.CreateSuccess('token');
	try
		Credentials := TAuthCredentials.Create('test@mail.ru', 'pass', 'test', 'mail.ru');
		Strategy.Authenticate(Credentials, nil, nil);

		Assert.IsTrue(Strategy.AuthenticateCalled, 'Should be called');
		Assert.AreEqual(1, Strategy.CallCount, 'Should have 1 call');

		Strategy.Reset;

		Assert.IsFalse(Strategy.AuthenticateCalled, 'AuthenticateCalled should be reset');
		Assert.AreEqual(0, Strategy.CallCount, 'CallCount should be reset');
	finally
		Strategy.Free;
	end;
end;

procedure TCloudMailRuLoginTest.TestMockAuthStrategy_SetSucceed_ChangesResult;
var
	Strategy: TMockAuthStrategy;
	Result: TAuthResult;
	Credentials: TAuthCredentials;
begin
	Strategy := TMockAuthStrategy.CreateSuccess('token');
	try
		Credentials := TAuthCredentials.Create('test@mail.ru', 'pass', 'test', 'mail.ru');

		Result := Strategy.Authenticate(Credentials, nil, nil);
		Assert.IsTrue(Result.Success, 'Initially should succeed');

		Strategy.SetSucceed(False);
		Strategy.SetErrorMessage('Now failing');

		Result := Strategy.Authenticate(Credentials, nil, nil);
		Assert.IsFalse(Result.Success, 'Should now fail');
		Assert.AreEqual(String('Now failing'), String(Result.ErrorMessage), 'Should have error message');
	finally
		Strategy.Free;
	end;
end;

procedure TCloudMailRuLoginTest.TestMockAuthStrategy_SetAccessToken_ChangesToken;
var
	Strategy: TMockAuthStrategy;
	Result: TAuthResult;
	Credentials: TAuthCredentials;
begin
	Strategy := TMockAuthStrategy.CreateSuccess('original_token');
	try
		Credentials := TAuthCredentials.Create('test@mail.ru', 'pass', 'test', 'mail.ru');

		Result := Strategy.Authenticate(Credentials, nil, nil);
		Assert.AreEqual(String('original_token'), String(Result.AuthToken), 'Should have original token');

		Strategy.SetAccessToken('new_token');

		Result := Strategy.Authenticate(Credentials, nil, nil);
		Assert.AreEqual(String('new_token'), String(Result.AuthToken), 'Should have new token');
	finally
		Strategy.Free;
	end;
end;

procedure TCloudMailRuLoginTest.TestMockAuthStrategy_CreateSharedSuccess_ReturnsShardAndLink;
var
	Strategy: TMockAuthStrategy;
	Result: TAuthResult;
	Credentials: TAuthCredentials;
begin
	Strategy := TMockAuthStrategy.CreateSharedSuccess('https://public.shard/', 'abc123');
	try
		Credentials := TAuthCredentials.Create('', '', '', '');
		Result := Strategy.Authenticate(Credentials, nil, nil);

		Assert.IsTrue(Result.Success, 'Should succeed');
		Assert.AreEqual(String('https://public.shard/'), String(Result.PublicShard), 'Should have public shard');
		Assert.AreEqual(String('abc123'), String(Result.PublicLink), 'Should have public link');
	finally
		Strategy.Free;
	end;
end;

procedure TCloudMailRuLoginTest.TestMockAuthStrategy_GetName_ReturnsMock;
var
	Strategy: TMockAuthStrategy;
begin
	Strategy := TMockAuthStrategy.CreateSuccess('token');
	try
		Assert.AreEqual(String('Mock'), String(Strategy.GetName), 'Should return Mock as name');
	finally
		Strategy.Free;
	end;
end;

{TMockAuthStrategySequence tests}

procedure TCloudMailRuLoginTest.TestMockAuthStrategySequence_ReturnsResultsInOrder;
var
	Strategy: TMockAuthStrategySequence;
	Result: TAuthResult;
	Credentials: TAuthCredentials;
begin
	Strategy := TMockAuthStrategySequence.Create;
	try
		Strategy.AddFailure('First fails');
		Strategy.AddSuccess('second_token');
		Strategy.AddSuccess('third_token');

		Credentials := TAuthCredentials.Create('test@mail.ru', 'pass', 'test', 'mail.ru');

		Result := Strategy.Authenticate(Credentials, nil, nil);
		Assert.IsFalse(Result.Success, 'First should fail');

		Result := Strategy.Authenticate(Credentials, nil, nil);
		Assert.IsTrue(Result.Success, 'Second should succeed');
		Assert.AreEqual(String('second_token'), String(Result.AuthToken), 'Second token');

		Result := Strategy.Authenticate(Credentials, nil, nil);
		Assert.IsTrue(Result.Success, 'Third should succeed');
		Assert.AreEqual(String('third_token'), String(Result.AuthToken), 'Third token');
	finally
		Strategy.Free;
	end;
end;

procedure TCloudMailRuLoginTest.TestMockAuthStrategySequence_RepeatLastAfterExhausted;
var
	Strategy: TMockAuthStrategySequence;
	Result: TAuthResult;
	Credentials: TAuthCredentials;
begin
	Strategy := TMockAuthStrategySequence.Create;
	try
		Strategy.AddFailure('First fails');
		Strategy.AddSuccess('final_token');

		Credentials := TAuthCredentials.Create('test@mail.ru', 'pass', 'test', 'mail.ru');

		Strategy.Authenticate(Credentials, nil, nil); {First - fails}
		Strategy.Authenticate(Credentials, nil, nil); {Second - succeeds}

		{Third and beyond should repeat last result}
		Result := Strategy.Authenticate(Credentials, nil, nil);
		Assert.IsTrue(Result.Success, 'Should succeed (repeating last)');
		Assert.AreEqual(String('final_token'), String(Result.AuthToken), 'Should repeat last token');
	finally
		Strategy.Free;
	end;
end;

procedure TCloudMailRuLoginTest.TestMockAuthStrategySequence_AddOAuthSuccess_IncludesAllFields;
var
	Strategy: TMockAuthStrategySequence;
	Result: TAuthResult;
	Credentials: TAuthCredentials;
begin
	Strategy := TMockAuthStrategySequence.Create;
	try
		Strategy.AddOAuthSuccess('access123', 'refresh456', 7200);

		Credentials := TAuthCredentials.Create('test@mail.ru', 'pass', 'test', 'mail.ru');
		Result := Strategy.Authenticate(Credentials, nil, nil);

		Assert.IsTrue(Result.Success, 'Should succeed');
		Assert.AreEqual(String('access123'), String(Result.OAuthToken.access_token), 'Should have access token');
		Assert.AreEqual(String('refresh456'), String(Result.OAuthToken.refresh_token), 'Should have refresh token');
		Assert.AreEqual(7200, Result.OAuthToken.expires_in, 'Should have expires_in');
	finally
		Strategy.Free;
	end;
end;

procedure TCloudMailRuLoginTest.TestMockAuthStrategySequence_Reset_RestartsSequence;
var
	Strategy: TMockAuthStrategySequence;
	Result: TAuthResult;
	Credentials: TAuthCredentials;
begin
	Strategy := TMockAuthStrategySequence.Create;
	try
		Strategy.AddFailure('First fails');
		Strategy.AddSuccess('second_succeeds');

		Credentials := TAuthCredentials.Create('test@mail.ru', 'pass', 'test', 'mail.ru');

		Strategy.Authenticate(Credentials, nil, nil);
		Strategy.Authenticate(Credentials, nil, nil);

		{Reset should restart from beginning}
		Strategy.Reset;

		Result := Strategy.Authenticate(Credentials, nil, nil);
		Assert.IsFalse(Result.Success, 'After reset, should fail again (first result)');
	finally
		Strategy.Free;
	end;
end;

procedure TCloudMailRuLoginTest.TestMockAuthStrategySequence_TracksCallCount;
var
	Strategy: TMockAuthStrategySequence;
	Credentials: TAuthCredentials;
begin
	Strategy := TMockAuthStrategySequence.Create;
	try
		Strategy.AddSuccess('token');

		Credentials := TAuthCredentials.Create('test@mail.ru', 'pass', 'test', 'mail.ru');

		Assert.AreEqual(0, Strategy.CallCount, 'Initial count should be 0');

		Strategy.Authenticate(Credentials, nil, nil);
		Assert.AreEqual(1, Strategy.CallCount, 'Count after first call');

		Strategy.Authenticate(Credentials, nil, nil);
		Assert.AreEqual(2, Strategy.CallCount, 'Count after second call');

		Strategy.Reset;
		Assert.AreEqual(0, Strategy.CallCount, 'Count after reset should be 0');
	finally
		Strategy.Free;
	end;
end;

procedure TCloudMailRuLoginTest.TestMockAuthStrategySequence_EmptySequence_ReturnsFailure;
var
	Strategy: TMockAuthStrategySequence;
	Result: TAuthResult;
	Credentials: TAuthCredentials;
begin
	Strategy := TMockAuthStrategySequence.Create;
	try
		{No results added}

		Credentials := TAuthCredentials.Create('test@mail.ru', 'pass', 'test', 'mail.ru');
		Result := Strategy.Authenticate(Credentials, nil, nil);

		Assert.IsFalse(Result.Success, 'Empty sequence should fail');
		Assert.AreEqual(String('No results configured'), String(Result.ErrorMessage), 'Should have error message');
	finally
		Strategy.Free;
	end;
end;

initialization
	TDUnitX.RegisterTestFixture(TCloudMailRuLoginTest);

end.

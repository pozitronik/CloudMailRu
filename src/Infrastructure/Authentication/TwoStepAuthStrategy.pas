unit TwoStepAuthStrategy;

{DEPRECATED: Two-step web authentication - no longer works after VK ID migration.
	This strategy requires UI interaction for the security code prompt.
	This strategy is preserved for historical reference only.
	Use TOAuthAppAuthStrategy for production authentication.}

interface

uses
	AuthStrategy,
	PasswordUIProvider,
	CloudHTTP,
	TCLogger;

type
	TTwoStepAuthStrategy = class(TInterfacedObject, IAuthStrategy)
	private
		FPasswordUI: IPasswordUIProvider;
	public
		constructor Create(PasswordUI: IPasswordUIProvider);
		function Authenticate(const Credentials: TAuthCredentials; HTTP: ICloudHTTP; Logger: ILogger): TAuthResult;
		function GetName: WideString;
	end;

implementation

uses
	SysUtils,
	DateUtils,
	System.Generics.Collections,
	System.UITypes,
	Windows,
	CMRConstants,
	CMRTwostep,
	CMRTwostepJsonAdapter,
	WFXTypes,
	LANGUAGE_STRINGS,
	ParsingHelper;

{TTwoStepAuthStrategy}

constructor TTwoStepAuthStrategy.Create(PasswordUI: IPasswordUIProvider);
begin
	inherited Create;
	FPasswordUI := PasswordUI;
end;

function TTwoStepAuthStrategy.Authenticate(const Credentials: TAuthCredentials; HTTP: ICloudHTTP; Logger: ILogger): TAuthResult;
var
	PostAnswer: WideString;
	TwoStepJson: WideString;
	AuthMessage: WideString;
	TwostepData: TCMRTwostep;
	SecurityKey: WideString;
	FormFields: TDictionary<WideString, WideString>;
	TokenPageContent: WideString;
	AuthToken: WideString;
	x_page_id, build: WideString;
	UnitedParams: WideString;
	Progress: Boolean;
	UseTC: Boolean;
begin
	Result := TAuthResult.CreateFailure('Two-step authentication failed');

	if (Credentials.Email = '') or (Credentials.Password = '') then
	begin
		Result := TAuthResult.CreateFailure('Email and password are required');
		Exit;
	end;

	if (Credentials.User = '') or (Credentials.Domain = '') then
	begin
		Result := TAuthResult.CreateFailure('User and domain must be parsed from email');
		Exit;
	end;

	FormFields := TDictionary<WideString, WideString>.Create();
	try
		{Step 1: Initial login request}
		FormFields.AddOrSetValue('Domain', Credentials.Domain);
		FormFields.AddOrSetValue('Login', Credentials.User);
		FormFields.AddOrSetValue('Password', Credentials.Password);

		if Assigned(Logger) then
			Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, REQUESTING_FIRST_STEP_AUTH_TOKEN, [Credentials.Email]);

		if not HTTP.PostMultipart(LOGIN_URL, FormFields, PostAnswer) then
		begin
			if Assigned(Logger) then
				Logger.Log(LOG_LEVEL_ERROR, msgtype_importanterror, ERR_GET_FIRST_STEP_AUTH_TOKEN, [Credentials.Email]);
			Result := TAuthResult.CreateFailure('Failed to perform first step authentication');
			Exit;
		end;

		{Step 2: Parse two-step response}
		if Assigned(Logger) then
			Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, PARSING_AUTH_DATA);

		if not(extractTwostepJson(PostAnswer, TwoStepJson) and TCMRTwostepJsonAdapter.Parse(TwoStepJson, TwostepData)) then
		begin
			if Assigned(Logger) then
				Logger.Log(LOG_LEVEL_ERROR, msgtype_importanterror, ERR_PARSE_AUTH_DATA);
			Result := TAuthResult.CreateFailure('Failed to parse two-step authentication data');
			Exit;
		end;

		{Step 3: Determine message for user}
		if TwostepData.secstep_timeout = AUTH_APP_USED then
			AuthMessage := ASK_AUTH_APP_CODE
		else if TwostepData.secstep_resend_fail = '1' then
			AuthMessage := Format(SMS_TIMEOUT, [TwostepData.secstep_phone, TwostepData.secstep_timeout])
		else
			AuthMessage := Format(ASK_SENT_CODE, [TwostepData.secstep_phone]);

		if Assigned(Logger) then
			Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, AWAIT_SECURITY_KEY);

		{Step 4: Ask user for security code}
		UseTC := False;
		if FPasswordUI.AskPassword(ASK_AUTH_KEY, AuthMessage, SecurityKey, UseTC, True, 0) <> mrOk then
		begin
			if Assigned(Logger) then
				Logger.Log(LOG_LEVEL_ERROR, msgtype_importanterror, ERR_SECURITY_KEY);
			Result := TAuthResult.CreateFailure('User cancelled security key input');
			Exit;
		end;

		{Step 5: Submit security code}
		FormFields.Clear;
		FormFields.AddOrSetValue('Login', Credentials.Email);
		FormFields.AddOrSetValue('csrf', TwostepData.csrf);
		FormFields.AddOrSetValue('AuthCode', SecurityKey);

		if Assigned(Logger) then
			Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, SECOND_STEP_AUTH);

		if not HTTP.PostMultipart(SECSTEP_URL, FormFields, PostAnswer) then
		begin
			Result := TAuthResult.CreateFailure('Failed to submit security code');
			Exit;
		end;

		{Step 6: Get token page and extract parameters}
		Progress := False;
		if not HTTP.GetPage(TOKEN_HOME_URL, TokenPageContent, Progress) then
		begin
			Result := TAuthResult.CreateFailure('Failed to fetch token page after two-step auth');
			Exit;
		end;

		if not(extractTokenFromText(TokenPageContent, AuthToken) and extract_x_page_id_FromText(TokenPageContent, x_page_id) and extract_build_FromText(TokenPageContent, build)) then
		begin
			if Assigned(Logger) then
				Logger.Log(LOG_LEVEL_ERROR, msgtype_importanterror, ERR_TWOSTEP_AUTH);
			Result := TAuthResult.CreateFailure('Failed to extract authentication token after two-step');
			Exit;
		end;

		{Build united params string for API calls}
		UnitedParams := Format('api=2&build=%s&x-page-id=%s&email=%s@%s&x-email=%s@%s&_=%d810', [build, x_page_id, Credentials.User, Credentials.Domain, Credentials.User, Credentials.Domain, DateTimeToUnix(Now)]);

		Result := TAuthResult.CreateSuccess(AuthToken, UnitedParams);

	finally
		FormFields.Free;
	end;
end;

function TTwoStepAuthStrategy.GetName: WideString;
begin
	Result := 'Two-Step Web (Deprecated)';
end;

end.

unit MockServerManager;

{Manages the tucha mock server lifecycle for integration tests.
	Starts/stops the server process and creates test accounts via CLI.
	When TuchaPath is configured, tests become fully self-contained:
	no manual server start or account setup needed.}

interface

uses
	System.SysUtils,
	System.Classes;

type
	{Manages tucha mock server: start, stop, and account provisioning}
	TMockServerManager = class
	private
		FTuchaPath: WideString;
		FTuchaConfigPath: WideString;
		FServerUrl: WideString;
		FStarted: Boolean;

		{Execute tucha CLI command and wait for completion.
			@param Args Command-line arguments to pass to tucha
			@param AllowedExitCodes Exit codes treated as success besides 0
			@return Process exit code
			@raises Exception if execution fails or exit code is unexpected}
		function ExecuteTucha(const Args: WideString; const AllowedExitCodes: array of DWORD): DWORD;

		{Poll the server URL until it responds or timeout expires.
			@param TimeoutMs Maximum wait time in milliseconds
			@return True if server became available within timeout}
		function WaitForServer(TimeoutMs: Integer = 10000): Boolean;
	public
		{@param TuchaPath Full path to tucha.exe
			@param ConfigPath Full path to tucha config.yaml
			@param ServerUrl Base URL the server will listen on (for health polling)}
		constructor Create(const TuchaPath, ConfigPath, ServerUrl: WideString);
		destructor Destroy; override;

		{Start the mock server in background mode}
		procedure Start;

		{Stop the mock server}
		procedure Stop;

		{Create a user account on the mock server.
			@param Email User email address
			@param Password User password
			@param Quota Storage quota string (default '16GB')}
		procedure CreateUser(const Email, Password: WideString; const Quota: WideString = '16GB');

		{Enable file history for a user account.
			@param Email User email address}
		procedure EnableHistory(const Email: WideString);

		property Started: Boolean read FStarted;
	end;

implementation

uses
	Winapi.Windows,
	Winapi.ShellAPI;

{TMockServerManager}

constructor TMockServerManager.Create(const TuchaPath, ConfigPath, ServerUrl: WideString);
begin
	inherited Create;
	FTuchaPath := TuchaPath;
	FTuchaConfigPath := ConfigPath;
	FServerUrl := ServerUrl;
	FStarted := False;
end;

destructor TMockServerManager.Destroy;
begin
	if FStarted then
		Stop;
	inherited;
end;

function TMockServerManager.ExecuteTucha(const Args: WideString; const AllowedExitCodes: array of DWORD): DWORD;
var
	SI: TStartupInfoW;
	PI: TProcessInformation;
	CmdLine: WideString;
	WaitResult: DWORD;
	I: Integer;
begin
	CmdLine := '"' + FTuchaPath + '"';
	if FTuchaConfigPath <> '' then
		CmdLine := CmdLine + ' -config "' + FTuchaConfigPath + '"';
	CmdLine := CmdLine + ' ' + Args;

	FillChar(SI, SizeOf(SI), 0);
	SI.cb := SizeOf(SI);
	SI.dwFlags := STARTF_USESHOWWINDOW;
	SI.wShowWindow := SW_HIDE;
	FillChar(PI, SizeOf(PI), 0);

	if not CreateProcessW(
		nil,
		PWideChar(CmdLine),
		nil, nil,
		False,
		CREATE_NO_WINDOW,
		nil, nil,
		SI, PI) then
		raise Exception.CreateFmt('Failed to execute tucha: %s (error %d)', [CmdLine, GetLastError]);

	try
		{Wait up to 30 seconds for the process to complete}
		WaitResult := WaitForSingleObject(PI.hProcess, 30000);
		if WaitResult = WAIT_TIMEOUT then
			raise Exception.Create('Tucha command timed out: ' + CmdLine);

		GetExitCodeProcess(PI.hProcess, Result);
		if Result = 0 then
			Exit;

		{Check if exit code is in the allowed list}
		for I := Low(AllowedExitCodes) to High(AllowedExitCodes) do
			if Result = AllowedExitCodes[I] then
				Exit;

		raise Exception.CreateFmt('Tucha command failed with exit code %d: %s', [Result, CmdLine]);
	finally
		CloseHandle(PI.hProcess);
		CloseHandle(PI.hThread);
	end;
end;

function TMockServerManager.WaitForServer(TimeoutMs: Integer): Boolean;
var
	StartTime: Cardinal;
	Elapsed: Cardinal;
begin
	Result := False;
	StartTime := GetTickCount;

	{Poll by trying to connect to the server URL.
		Use a simple HTTP GET via tucha's health or config endpoint.
		Since we can't easily do HTTP here without pulling in dependencies,
		we rely on a brief sleep after --background returns --
		tucha's --background flag only returns after the server is listening.}
	Sleep(500);
	Elapsed := GetTickCount - StartTime;

	{Poll with --status which returns exit code 0 when running, 4 when not}
	while Elapsed < Cardinal(TimeoutMs) do
	begin
		try
			ExecuteTucha('--status', []);
			Result := True;
			Exit;
		except
			{Server not ready yet, retry}
			Sleep(500);
			Elapsed := GetTickCount - StartTime;
		end;
	end;
end;

procedure TMockServerManager.Start;
const
	EXIT_ALREADY_RUNNING = 3;
var
	ExitCode: DWORD;
begin
	if FStarted then
		Exit;

	WriteLn('[MockServer] Starting tucha: ' + FTuchaPath);

	{--background starts the server and returns once it is listening.
		Exit code 3 means already running -- treat as success.}
	ExitCode := ExecuteTucha('--background', [EXIT_ALREADY_RUNNING]);
	FStarted := True;

	if ExitCode = EXIT_ALREADY_RUNNING then
		WriteLn('[MockServer] Server was already running')
	else if not WaitForServer then
		WriteLn('[MockServer] WARNING: Server may not be fully ready')
	else
		WriteLn('[MockServer] Server started successfully');
end;

procedure TMockServerManager.Stop;
const
	EXIT_NOT_RUNNING = 4;
begin
	if not FStarted then
		Exit;

	WriteLn('[MockServer] Stopping tucha...');
	try
		{Exit code 4 means not running -- harmless during teardown}
		ExecuteTucha('--stop', [EXIT_NOT_RUNNING]);
	except
		on E: Exception do
			WriteLn('[MockServer] WARNING: Stop failed: ' + E.Message);
	end;
	FStarted := False;
	WriteLn('[MockServer] Server stopped');
end;

procedure TMockServerManager.CreateUser(const Email, Password: WideString; const Quota: WideString);
begin
	WriteLn('[MockServer] Creating user: ' + Email);
	ExecuteTucha(Format('--user add %s %s %s', [Email, Password, Quota]), []);
end;

procedure TMockServerManager.EnableHistory(const Email: WideString);
begin
	WriteLn('[MockServer] Enabling history for: ' + Email);
	ExecuteTucha(Format('--user history %s on', [Email]), []);
end;

end.

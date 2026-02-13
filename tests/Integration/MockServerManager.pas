unit MockServerManager;

{Manages the tucha mock server lifecycle for integration tests.
	Given only a path to tucha.exe, this class handles everything:
	generates server config, starts/stops the process, provisions accounts.
	Uses a fixed work directory next to the test executable so that
	PID files persist across runs and stale servers can be stopped.}

interface

uses
	System.SysUtils,
	System.Classes,
	Winapi.Windows;

type
	{Manages tucha mock server: config generation, start, stop, account provisioning}
	TMockServerManager = class
	private
		FTuchaPath: WideString;
		FServerUrl: WideString;
		FServerPort: Integer;
		FWorkDir: WideString;
		FConfigPath: WideString;
		FStarted: Boolean;

		{Execute tucha CLI command and wait for completion.
			@param Args Command-line arguments to pass to tucha
			@param AllowedExitCodes Exit codes treated as success besides 0
			@return Process exit code
			@raises Exception if execution fails or exit code is unexpected}
		function ExecuteTucha(const Args: WideString; const AllowedExitCodes: array of DWORD): DWORD;

		{Poll the server until it responds or timeout expires.
			@param TimeoutMs Maximum wait time in milliseconds
			@return True if server became available within timeout}
		function WaitForServer(TimeoutMs: Integer = 10000): Boolean;

		{Create the work directory and write config.yaml for tucha}
		procedure GenerateConfig;

		{Extract port number from a URL like http://localhost:9090.
			@return Port number, or DEFAULT_MOCK_PORT if parsing fails}
		function ExtractPort(const Url: WideString): Integer;

		{Convert a Windows path to forward slashes for YAML compatibility}
		function ToYamlPath(const Path: WideString): WideString;
	public
		{@param TuchaPath Full path to tucha.exe
			@param ServerUrl Server URL (determines listen port and external URL)}
		constructor Create(const TuchaPath, ServerUrl: WideString);
		destructor Destroy; override;

		{Start the mock server in background mode.
			Generates config, stops any stale instance, starts fresh.}
		procedure Start;

		{Stop the mock server}
		procedure Stop;

		{Delete the mock server data directory for a clean slate.
			Call before Start to ensure no leftover state from previous runs.}
		procedure CleanData;

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

const
	DEFAULT_MOCK_PORT = 9090;
	DEFAULT_MOCK_SERVER_URL = 'http://localhost:9090';
	DEFAULT_MOCK_PRIMARY_EMAIL = 'primary@test.local';
	DEFAULT_MOCK_PRIMARY_PASSWORD = 'test123';
	DEFAULT_MOCK_SECONDARY_EMAIL = 'secondary@test.local';
	DEFAULT_MOCK_SECONDARY_PASSWORD = 'test123';
	MOCK_WORK_DIR_NAME = 'tucha_testdata';

implementation

uses
	System.IOUtils;

{TMockServerManager}

constructor TMockServerManager.Create(const TuchaPath, ServerUrl: WideString);
begin
	inherited Create;
	FTuchaPath := TuchaPath;
	FServerUrl := ServerUrl;
	FServerPort := ExtractPort(ServerUrl);
	FWorkDir := TPath.Combine(ExtractFilePath(ParamStr(0)), MOCK_WORK_DIR_NAME);
	FConfigPath := TPath.Combine(FWorkDir, 'config.yaml');
	FStarted := False;
end;

destructor TMockServerManager.Destroy;
begin
	if FStarted then
		Stop;
	inherited;
end;

function TMockServerManager.ExtractPort(const Url: WideString): Integer;
var
	LastColon: Integer;
	PortStr: string;
begin
	{Find the rightmost colon -- in http://host:port it precedes the port}
	LastColon := LastDelimiter(':', String(Url));
	if LastColon > 0 then
	begin
		PortStr := Copy(String(Url), LastColon + 1);
		{Strip trailing slash or path}
		PortStr := Trim(PortStr.TrimRight(['/']));
		if TryStrToInt(PortStr, Result) and (Result > 0) then
			Exit;
	end;
	Result := DEFAULT_MOCK_PORT;
end;

function TMockServerManager.ToYamlPath(const Path: WideString): WideString;
begin
	Result := StringReplace(Path, '\', '/', [rfReplaceAll]);
end;

procedure TMockServerManager.GenerateConfig;
var
	DataDir, ConfigContent: WideString;
begin
	{Ensure work directory exists}
	if not TDirectory.Exists(FWorkDir) then
		TDirectory.CreateDirectory(FWorkDir);

	DataDir := TPath.Combine(FWorkDir, 'data');
	if not TDirectory.Exists(DataDir) then
		TDirectory.CreateDirectory(DataDir);

	ConfigContent :=
		'server:' + sLineBreak +
		'  host: "0.0.0.0"' + sLineBreak +
		'  port: ' + IntToStr(FServerPort) + sLineBreak +
		'  external_url: "' + String(FServerUrl) + '"' + sLineBreak +
		'' + sLineBreak +
		'admin:' + sLineBreak +
		'  login: "admin"' + sLineBreak +
		'  password: "admin"' + sLineBreak +
		'' + sLineBreak +
		'storage:' + sLineBreak +
		'  db_path: "' + String(ToYamlPath(TPath.Combine(DataDir, 'tucha.db'))) + '"' + sLineBreak +
		'  content_dir: "' + String(ToYamlPath(TPath.Combine(DataDir, 'storage'))) + '"' + sLineBreak +
		'  quota_bytes: 17179869184' + sLineBreak +  {16 GiB}
		'' + sLineBreak +
		'logging:' + sLineBreak +
		'  level: "info"' + sLineBreak +
		'  output: "stdout"' + sLineBreak;

	TFile.WriteAllText(FConfigPath, ConfigContent, TEncoding.UTF8);
	WriteLn('[MockServer] Generated config: ' + FConfigPath);
end;

function TMockServerManager.ExecuteTucha(const Args: WideString; const AllowedExitCodes: array of DWORD): DWORD;
var
	SI: TStartupInfoW;
	PI: TProcessInformation;
	CmdLine: WideString;
	WaitResult: DWORD;
	I: Integer;
begin
	CmdLine := '"' + FTuchaPath + '" -config "' + FConfigPath + '" ' + Args;

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

	{--background already waits until the server is listening,
		so this is a safety net. Poll with --status (exit code 0 = running).}
	Sleep(500);
	Elapsed := GetTickCount - StartTime;

	while Elapsed < Cardinal(TimeoutMs) do
	begin
		try
			ExecuteTucha('--status', []);
			Result := True;
			Exit;
		except
			Sleep(500);
			Elapsed := GetTickCount - StartTime;
		end;
	end;
end;

procedure TMockServerManager.Start;
const
	EXIT_ALREADY_RUNNING = 3;
	EXIT_NOT_RUNNING = 4;
var
	ExitCode: DWORD;
begin
	if FStarted then
		Exit;

	WriteLn('[MockServer] Starting tucha: ' + FTuchaPath);

	{Stop any stale instance from a previous run.
		GenerateConfig writes the same config path every time, so any leftover
		PID file from a previous run points to the right process.}
	GenerateConfig;
	try
		ExecuteTucha('--stop', [EXIT_NOT_RUNNING]);
	except
		on E: Exception do
			WriteLn('[MockServer] Note: pre-start stop attempt: ' + E.Message);
	end;

	{Clean data and regenerate config so data directories exist for tucha}
	CleanData;
	GenerateConfig;

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
		ExecuteTucha('--stop', [EXIT_NOT_RUNNING]);
	except
		on E: Exception do
			WriteLn('[MockServer] WARNING: Stop failed: ' + E.Message);
	end;
	FStarted := False;
	WriteLn('[MockServer] Server stopped');
end;

procedure TMockServerManager.CleanData;
var
	DataDir: WideString;
begin
	DataDir := TPath.Combine(FWorkDir, 'data');
	if TDirectory.Exists(DataDir) then
	begin
		TDirectory.Delete(DataDir, True);
		WriteLn('[MockServer] Cleaned data directory');
	end;
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

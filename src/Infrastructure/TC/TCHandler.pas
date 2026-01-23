unit TCHandler;

{Total Commander handler - provides access to TC window, settings, and configuration.
 All consumers must receive ITCHandler via dependency injection.}

interface

uses
	Windows,
	SysUtils,
	IniFiles,
	MultiMon,
	Description,
	WindowsEnvironment;

const
	{Total Commander panel refresh message constants}
	TC_REFRESH_MESSAGE = 51; {WM_USER + TC_REFRESH_MESSAGE to refresh panel}
	TC_REFRESH_PARAM = 540; {wParam for panel refresh message}

	{Default values when TC is not available}
	TC_DEFAULT_ICON_SIZE = 16;
	TC_WINDOW_CLASS = 'TTOTAL_CMD';

type
	{Interface for Total Commander handler operations}
	ITCHandler = interface
		['{F2658BDE-3735-4231-80C0-765EC0477B0C}']

		{Returns handle to Total Commander main window, 0 if not found}
		function FindTCWindow: HWND;

		{Returns path to Total Commander INI file from COMMANDER_INI env var}
		function GetTCIniPath: WideString;

		{Returns icon size configured in Total Commander (resolution-aware)}
		function GetTCIconsSize: Integer;

		{Returns preferred encoding for descript.ion files from TC settings}
		function GetTCCommentPreferredFormat: Integer;
	end;

	{Null implementation - returns safe defaults, useful for testing}
	TNullTCHandler = class(TInterfacedObject, ITCHandler)
	public
		function FindTCWindow: HWND;
		function GetTCIniPath: WideString;
		function GetTCIconsSize: Integer;
		function GetTCCommentPreferredFormat: Integer;
	end;

	{In-memory implementation for testing - allows configuring return values}
	TMemoryTCHandler = class(TInterfacedObject, ITCHandler)
	private
		FTCWindow: HWND;
		FTCIniPath: WideString;
		FIconsSize: Integer;
		FCommentFormat: Integer;
	public
		constructor Create;

		function FindTCWindow: HWND;
		function GetTCIniPath: WideString;
		function GetTCIconsSize: Integer;
		function GetTCCommentPreferredFormat: Integer;

		{Test configuration}
		procedure SetTCWindow(Value: HWND);
		procedure SetTCIniPath(const Value: WideString);
		procedure SetIconsSize(Value: Integer);
		procedure SetCommentFormat(Value: Integer);
	end;

	{Windows implementation - uses real Windows API and file system}
	TTCHandler = class(TInterfacedObject, ITCHandler)
	private
		FEnvironment: IEnvironment;
	public
		constructor Create(Environment: IEnvironment);

		function FindTCWindow: HWND;
		function GetTCIniPath: WideString;
		function GetTCIconsSize: Integer;
		function GetTCCommentPreferredFormat: Integer;
	end;

implementation

{TNullTCHandler}

function TNullTCHandler.FindTCWindow: HWND;
begin
	Result := 0;
end;

function TNullTCHandler.GetTCIniPath: WideString;
begin
	Result := '';
end;

function TNullTCHandler.GetTCIconsSize: Integer;
begin
	Result := TC_DEFAULT_ICON_SIZE;
end;

function TNullTCHandler.GetTCCommentPreferredFormat: Integer;
begin
	Result := ENCODING_UTF8;
end;

{TMemoryTCHandler}

constructor TMemoryTCHandler.Create;
begin
	inherited Create;
	FTCWindow := 0;
	FTCIniPath := '';
	FIconsSize := TC_DEFAULT_ICON_SIZE;
	FCommentFormat := ENCODING_UTF8;
end;

function TMemoryTCHandler.FindTCWindow: HWND;
begin
	Result := FTCWindow;
end;

function TMemoryTCHandler.GetTCIniPath: WideString;
begin
	Result := FTCIniPath;
end;

function TMemoryTCHandler.GetTCIconsSize: Integer;
begin
	Result := FIconsSize;
end;

function TMemoryTCHandler.GetTCCommentPreferredFormat: Integer;
begin
	Result := FCommentFormat;
end;

procedure TMemoryTCHandler.SetTCWindow(Value: HWND);
begin
	FTCWindow := Value;
end;

procedure TMemoryTCHandler.SetTCIniPath(const Value: WideString);
begin
	FTCIniPath := Value;
end;

procedure TMemoryTCHandler.SetIconsSize(Value: Integer);
begin
	FIconsSize := Value;
end;

procedure TMemoryTCHandler.SetCommentFormat(Value: Integer);
begin
	FCommentFormat := Value;
end;

{TTCHandler}

constructor TTCHandler.Create(Environment: IEnvironment);
begin
	inherited Create;
	FEnvironment := Environment;
end;

function TTCHandler.FindTCWindow: HWND;
begin
	Result := Windows.FindWindow(TC_WINDOW_CLASS, nil);
end;

function TTCHandler.GetTCIniPath: WideString;
begin
	Result := FEnvironment.GetEnvironmentVariable('COMMANDER_INI');
end;

function TTCHandler.GetTCIconsSize: Integer;
var
	TC_INI: TIniFile;
	ResolutionSpecific: Boolean;
	IconsSizeSectionName: WideString;
	MonInfo: TMonitorInfo;
	IniPath: WideString;
	TCWindow: HWND;
begin
	Result := TC_DEFAULT_ICON_SIZE;
	IniPath := GetTCIniPath;

	if (IniPath <> '') and FEnvironment.FileExists(IniPath) then
	begin
		TC_INI := TIniFile.Create(IniPath);
		try
			ResolutionSpecific := TC_INI.ReadBool('Configuration', 'ResolutionSpecific', True);
			if ResolutionSpecific then
			begin
				TCWindow := FindTCWindow;
				MonInfo.cbSize := SizeOf(MonInfo);
				GetMonitorInfo(MonitorFromWindow(TCWindow, MONITOR_DEFAULTTONEAREST), @MonInfo);

				IconsSizeSectionName := Format('%dx%d', [
					MonInfo.rcMonitor.Right - MonInfo.rcMonitor.Left,
					MonInfo.rcMonitor.Bottom - MonInfo.rcMonitor.Top
				]) + ' (8x16)'; {normal font section}

				if not TC_INI.SectionExists(IconsSizeSectionName) then
				begin
					IconsSizeSectionName := Format('%dx%d', [
						MonInfo.rcMonitor.Right - MonInfo.rcMonitor.Left,
						MonInfo.rcMonitor.Bottom - MonInfo.rcMonitor.Top
					]) + ' (10x20)'; {large font section}

					if not TC_INI.SectionExists(IconsSizeSectionName) then
						IconsSizeSectionName := 'AllResolutions';
				end;
			end
			else
				IconsSizeSectionName := 'AllResolutions';

			Result := TC_INI.ReadInteger(IconsSizeSectionName, 'Iconsize32', Result);
		finally
			TC_INI.Free;
		end;
	end;
end;

function TTCHandler.GetTCCommentPreferredFormat: Integer;
var
	TC_INI: TIniFile;
	IniPath: WideString;
begin
	Result := ENCODING_UTF8;
	IniPath := GetTCIniPath;

	if (IniPath <> '') and FEnvironment.FileExists(IniPath) then
	begin
		TC_INI := TIniFile.Create(IniPath);
		try
			Result := TC_INI.ReadInteger('Configuration', 'CommentPreferredFormat', Result);
		finally
			TC_INI.Free;
		end;
	end;

	if not (Result in [ENCODING_DEFAULT, ENCODING_UNICODE, ENCODING_UNCODE_BE, ENCODING_UTF8]) then
		Result := ENCODING_UTF8; {ignore "combined" TC encodings}
end;

end.

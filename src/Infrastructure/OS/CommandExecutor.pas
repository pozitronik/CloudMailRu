unit CommandExecutor;

{External command execution via Windows CreateProcess}
interface

uses
	SysUtils,
	Windows;

type
	{Interface for executing external commands.
		Enables dependency injection and testing without running real processes.}
	ICommandExecutor = interface
		['{C5D8E2A1-9F3B-4C7E-A1D6-8B2F5E9C3A7D}']

		{Execute an external command.
			@param Command Path to the executable
			@param Params Command parameters
			@param StartPath Working directory for the process
			@return True if process was started successfully}
		function Execute(Command, Params, StartPath: WideString): Boolean;
	end;

	{Default implementation using Windows CreateProcess.}
	TWindowsCommandExecutor = class(TInterfacedObject, ICommandExecutor)
	public
		function Execute(Command, Params, StartPath: WideString): Boolean;
	end;

implementation

function TWindowsCommandExecutor.Execute(Command, Params, StartPath: WideString): Boolean;
var
	lpStartupInfo: TStartUpInfo;
	lpProcessInformation: TProcessInformation;
	lpCurrentDirectory: PWideChar;
begin
	lpStartupInfo := Default (TStartUpInfo);
	lpStartupInfo.cb := SizeOf(lpStartupInfo);
	if EmptyWideStr = StartPath then
		lpCurrentDirectory := nil
	else
		lpCurrentDirectory := PWideChar(StartPath);

	Result := CreateProcessW(nil, PWideChar(Command + ' "' + Params + '"'), nil, nil, False, NORMAL_PRIORITY_CLASS, nil, lpCurrentDirectory, lpStartupInfo, lpProcessInformation);
	if Result then
		with lpProcessInformation do
		begin
			WaitForInputIdle(hProcess, INFINITE); //ждем завершения инициализации
			CloseHandle(hThread); //закрываем дескриптор процесса
			CloseHandle(hProcess); //закрываем дескриптор потока
		end
end;

end.

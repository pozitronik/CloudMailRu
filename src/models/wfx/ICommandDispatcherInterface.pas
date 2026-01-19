unit ICommandDispatcherInterface;

{Interface for plugin command dispatching.
 Extracts command routing logic from MailRuCloudWFX to enable testing
 and separation of concerns.}

interface

uses
	PLUGIN_TYPES;

type
	{Result of command execution with optional symlink path for navigation commands}
	TCommandResult = record
		ResultCode: Integer;
		SymlinkPath: WideString;

		class function OK: TCommandResult; static;
		class function Error: TCommandResult; static;
		class function Symlink(const Path: WideString): TCommandResult; static;
	end;

	ICommandDispatcher = interface
		['{B8F2D4A1-3E7C-4D9B-8A1F-5C6E9D2B4A8F}']

		{Execute a plugin command.
		 Supported commands: rmdir, share, hash, clone, trash, shared, invites
		 @param RemoteName Current remote path (used to determine account context)
		 @param Command Command name (should be lowercase)
		 @param Parameter Command-specific parameter
		 @return TCommandResult with result code and optional symlink path}
		function Execute(const RemoteName, Command, Parameter: WideString): TCommandResult;
	end;

implementation

class function TCommandResult.OK: TCommandResult;
begin
	Result.ResultCode := FS_EXEC_OK;
	Result.SymlinkPath := '';
end;

class function TCommandResult.Error: TCommandResult;
begin
	Result.ResultCode := FS_EXEC_ERROR;
	Result.SymlinkPath := '';
end;

class function TCommandResult.Symlink(const Path: WideString): TCommandResult;
begin
	Result.ResultCode := FS_EXEC_SYMLINK;
	Result.SymlinkPath := Path;
end;

end.

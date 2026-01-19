unit IFileExecutionDispatcherInterface;

{Interface for routing file execution verbs to appropriate handlers.
 Determines WHAT action to take based on path type and verb, returns
 action context for the caller to execute.}

interface

uses
	RealPath,
	StreamingSettings;

type
	{Execution action types that FsExecuteFile can route to}
	TExecutionActionType = (
		eatNone,              {No action required, return FS_EXEC_OK}
		eatTrashbinProperties,{Show trashbin item properties dialog}
		eatSharedAction,      {Handle shared folder actions (open/properties)}
		eatInvitesAction,     {Handle invites folder actions}
		eatProperties,        {Show regular file/folder properties}
		eatStream,            {Stream file via configured player}
		eatCommand,           {Execute quote command}
		eatOpenYourself       {Tell TC to handle the open action}
	);

	{Result of dispatch decision - action type plus context needed by handler}
	TExecutionAction = record
		ActionType: TExecutionActionType;
		RealPath: TRealPath;
		ActionOpen: Boolean;            {For shared actions: true = open, false = properties}
		StreamingSettings: TStreamingSettings; {For streaming action}
		Command: WideString;            {For quote commands: the command name}
		Parameter: WideString;          {For quote commands: the parameter}

		class function None: TExecutionAction; static;
		class function TrashbinProperties(const ARealPath: TRealPath): TExecutionAction; static;
		class function SharedAction(const ARealPath: TRealPath; AOpen: Boolean): TExecutionAction; static;
		class function InvitesAction(const ARealPath: TRealPath): TExecutionAction; static;
		class function Properties(const ARealPath: TRealPath): TExecutionAction; static;
		class function Stream(const ARealPath: TRealPath; const ASettings: TStreamingSettings): TExecutionAction; static;
		class function QuoteCommand(const ARealPath: TRealPath; const ACommand, AParameter: WideString): TExecutionAction; static;
		class function OpenYourself: TExecutionAction; static;
	end;

	{Callback type for getting streaming settings for a path extension}
	TStreamingSettingsGetter = function(const Path: WideString): TStreamingSettings of object;

	IFileExecutionDispatcher = interface
		['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
		{Determines appropriate action for execution request.
		 @param RemoteName The path being executed
		 @param Verb The verb (open, properties, quote ...)
		 @param StreamingGetter Callback to get streaming settings for file extension
		 @returns TExecutionAction with action type and context for handler}
		function GetAction(const RemoteName, Verb: WideString;
			StreamingGetter: TStreamingSettingsGetter): TExecutionAction;
	end;

implementation

class function TExecutionAction.None: TExecutionAction;
begin
	Result := Default(TExecutionAction);
	Result.ActionType := eatNone;
end;

class function TExecutionAction.TrashbinProperties(const ARealPath: TRealPath): TExecutionAction;
begin
	Result := Default(TExecutionAction);
	Result.ActionType := eatTrashbinProperties;
	Result.RealPath := ARealPath;
end;

class function TExecutionAction.SharedAction(const ARealPath: TRealPath; AOpen: Boolean): TExecutionAction;
begin
	Result := Default(TExecutionAction);
	Result.ActionType := eatSharedAction;
	Result.RealPath := ARealPath;
	Result.ActionOpen := AOpen;
end;

class function TExecutionAction.InvitesAction(const ARealPath: TRealPath): TExecutionAction;
begin
	Result := Default(TExecutionAction);
	Result.ActionType := eatInvitesAction;
	Result.RealPath := ARealPath;
end;

class function TExecutionAction.Properties(const ARealPath: TRealPath): TExecutionAction;
begin
	Result := Default(TExecutionAction);
	Result.ActionType := eatProperties;
	Result.RealPath := ARealPath;
end;

class function TExecutionAction.Stream(const ARealPath: TRealPath; const ASettings: TStreamingSettings): TExecutionAction;
begin
	Result := Default(TExecutionAction);
	Result.ActionType := eatStream;
	Result.RealPath := ARealPath;
	Result.StreamingSettings := ASettings;
end;

class function TExecutionAction.QuoteCommand(const ARealPath: TRealPath; const ACommand, AParameter: WideString): TExecutionAction;
begin
	Result := Default(TExecutionAction);
	Result.ActionType := eatCommand;
	Result.RealPath := ARealPath;
	Result.Command := ACommand;
	Result.Parameter := AParameter;
end;

class function TExecutionAction.OpenYourself: TExecutionAction;
begin
	Result := Default(TExecutionAction);
	Result.ActionType := eatOpenYourself;
end;

end.

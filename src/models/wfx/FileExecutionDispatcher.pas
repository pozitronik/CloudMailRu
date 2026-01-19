unit FileExecutionDispatcher;

{Routes file execution verbs to appropriate action types.
 Analyzes path and verb to determine what handler should process the request.}

interface

uses
	IFileExecutionDispatcherInterface;

type
	TFileExecutionDispatcher = class(TInterfacedObject, IFileExecutionDispatcher)
	public
		function GetAction(const RemoteName, Verb: WideString;
			StreamingGetter: TStreamingSettingsGetter): TExecutionAction;
	end;

implementation

uses
	SysUtils,
	RealPath,
	PathHelper,
	StringHelper,
	StreamingSettings,
	CMRConstants,
	PLUGIN_TYPES;

function TFileExecutionDispatcher.GetAction(const RemoteName, Verb: WideString;
	StreamingGetter: TStreamingSettingsGetter): TExecutionAction;
var
	RealPath: TRealPath;
	TargetStreamingSettings: TStreamingSettings;
begin
	RealPath.FromPath(RemoteName);
	TargetStreamingSettings := Default(TStreamingSettings);

	{Handle parent directory item - adjust path to parent for properties}
	if RealPath.upDirItem then
		RealPath.Path := ExtractFilePath(RealPath.Path);

	{Trashbin items show properties dialog for both open and properties verbs}
	if RealPath.trashDir and ((Verb = VERB_OPEN) or (Verb = VERB_PROPERTIES)) then
		Exit(TExecutionAction.TrashbinProperties(RealPath));

	{Shared folder has special handling for open/properties}
	if RealPath.sharedDir then
		Exit(TExecutionAction.SharedAction(RealPath, Verb = VERB_OPEN));

	{Invites folder always shows accept/reject dialog}
	if RealPath.invitesDir then
		Exit(TExecutionAction.InvitesAction(RealPath));

	{Properties verb for regular items}
	if Verb = VERB_PROPERTIES then
		Exit(TExecutionAction.Properties(RealPath));

	{Open verb - check streaming or let TC handle}
	if Verb = VERB_OPEN then
	begin
		{Only check streaming for files, not directories}
		if (RealPath.isDir <> ID_True) and Assigned(StreamingGetter) then
			TargetStreamingSettings := StreamingGetter(RealPath.Path);

		{Stream if format is configured and not unset/none}
		if (TargetStreamingSettings.Format <> STREAMING_FORMAT_UNSET) and
		   (TargetStreamingSettings.Format <> STREAMING_FORMAT_NONE) then
			Exit(TExecutionAction.Stream(RealPath, TargetStreamingSettings));

		{Let TC handle the open action}
		Exit(TExecutionAction.OpenYourself);
	end;

	{Quote commands - parse command and parameter}
	if Copy(Verb, 1, 5) = VERB_QUOTE then
		Exit(TExecutionAction.QuoteCommand(RealPath, LowerCase(GetWord(Verb, 1)), GetWord(Verb, 2)));

	{Unknown verb - no action needed}
	Result := TExecutionAction.None;
end;

end.

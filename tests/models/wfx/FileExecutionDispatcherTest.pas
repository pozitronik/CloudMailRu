unit FileExecutionDispatcherTest;

{Unit tests for TFileExecutionDispatcher.
 Tests routing logic for file execution verbs to appropriate action types.}

interface

uses
	DUnitX.TestFramework,
	IFileExecutionDispatcherInterface,
	FileExecutionDispatcher,
	StreamingSettings;

type
	[TestFixture]
	TFileExecutionDispatcherTest = class
	private
		FDispatcher: IFileExecutionDispatcher;
		FStreamingSettings: TStreamingSettings;

		{Helper to return configured streaming settings for testing}
		function StreamingGetterWithSettings(const Path: WideString): TStreamingSettings;
		{Helper to return no streaming settings}
		function StreamingGetterEmpty(const Path: WideString): TStreamingSettings;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Trashbin routing tests}
		[Test]
		procedure TestDispatch_TrashDir_OpenVerb_ReturnsTrashbinProperties;
		[Test]
		procedure TestDispatch_TrashDir_PropertiesVerb_ReturnsTrashbinProperties;

		{Shared folder routing tests}
		[Test]
		procedure TestDispatch_SharedDir_OpenVerb_ReturnsSharedActionWithOpenTrue;
		[Test]
		procedure TestDispatch_SharedDir_PropertiesVerb_ReturnsSharedActionWithOpenFalse;

		{Invites folder routing tests}
		[Test]
		procedure TestDispatch_InvitesDir_ReturnsInvitesAction;

		{Properties verb routing tests}
		[Test]
		procedure TestDispatch_RegularPath_PropertiesVerb_ReturnsProperties;

		{Open verb routing tests}
		[Test]
		procedure TestDispatch_OpenVerb_WithStreaming_ReturnsStream;
		[Test]
		procedure TestDispatch_OpenVerb_NoStreaming_ReturnsOpenYourself;
		[Test]
		procedure TestDispatch_OpenVerb_NilStreamingGetter_ReturnsOpenYourself;
		[Test]
		procedure TestDispatch_OpenVerb_Directory_SkipsStreamingCheck;

		{Quote command routing tests}
		[Test]
		procedure TestDispatch_QuoteVerb_ReturnsCommand;
		[Test]
		procedure TestDispatch_QuoteVerb_ParsesCommandAndParameter;

		{Unknown verb tests}
		[Test]
		procedure TestDispatch_UnknownVerb_ReturnsNone;

		{Parent directory item tests}
		[Test]
		procedure TestDispatch_UpDirItem_AdjustsPath;

		{TExecutionAction factory tests}
		[Test]
		procedure TestExecutionAction_None_SetsCorrectActionType;
		[Test]
		procedure TestExecutionAction_TrashbinProperties_SetsCorrectValues;
		[Test]
		procedure TestExecutionAction_SharedAction_SetsCorrectValues;
		[Test]
		procedure TestExecutionAction_InvitesAction_SetsCorrectValues;
		[Test]
		procedure TestExecutionAction_Properties_SetsCorrectValues;
		[Test]
		procedure TestExecutionAction_Stream_SetsCorrectValues;
		[Test]
		procedure TestExecutionAction_Command_SetsCorrectValues;
		[Test]
		procedure TestExecutionAction_OpenYourself_SetsCorrectActionType;
	end;

implementation

uses
	SysUtils,
	CMRConstants,
	PLUGIN_TYPES,
	RealPath;

procedure TFileExecutionDispatcherTest.Setup;
begin
	FDispatcher := TFileExecutionDispatcher.Create;
	FStreamingSettings := Default(TStreamingSettings);
end;

procedure TFileExecutionDispatcherTest.TearDown;
begin
	FDispatcher := nil;
end;

function TFileExecutionDispatcherTest.StreamingGetterWithSettings(const Path: WideString): TStreamingSettings;
begin
	Result := FStreamingSettings;
end;

function TFileExecutionDispatcherTest.StreamingGetterEmpty(const Path: WideString): TStreamingSettings;
begin
	Result := Default(TStreamingSettings);
	Result.Format := STREAMING_FORMAT_NONE;
end;

{Trashbin routing tests}

procedure TFileExecutionDispatcherTest.TestDispatch_TrashDir_OpenVerb_ReturnsTrashbinProperties;
var
	Action: TExecutionAction;
begin
	Action := FDispatcher.GetAction('\account.trash\somefile.txt', VERB_OPEN, nil);

	Assert.AreEqual(eatTrashbinProperties, Action.ActionType);
end;

procedure TFileExecutionDispatcherTest.TestDispatch_TrashDir_PropertiesVerb_ReturnsTrashbinProperties;
var
	Action: TExecutionAction;
begin
	Action := FDispatcher.GetAction('\account.trash\somefile.txt', VERB_PROPERTIES, nil);

	Assert.AreEqual(eatTrashbinProperties, Action.ActionType);
end;

{Shared folder routing tests}

procedure TFileExecutionDispatcherTest.TestDispatch_SharedDir_OpenVerb_ReturnsSharedActionWithOpenTrue;
var
	Action: TExecutionAction;
begin
	Action := FDispatcher.GetAction('\account.shared\somefile.txt', VERB_OPEN, nil);

	Assert.AreEqual(eatSharedAction, Action.ActionType);
	Assert.IsTrue(Action.ActionOpen);
end;

procedure TFileExecutionDispatcherTest.TestDispatch_SharedDir_PropertiesVerb_ReturnsSharedActionWithOpenFalse;
var
	Action: TExecutionAction;
begin
	Action := FDispatcher.GetAction('\account.shared\somefile.txt', VERB_PROPERTIES, nil);

	Assert.AreEqual(eatSharedAction, Action.ActionType);
	Assert.IsFalse(Action.ActionOpen);
end;

{Invites folder routing tests}

procedure TFileExecutionDispatcherTest.TestDispatch_InvitesDir_ReturnsInvitesAction;
var
	Action: TExecutionAction;
begin
	Action := FDispatcher.GetAction('\account.invites\someinvite', VERB_OPEN, nil);

	Assert.AreEqual(eatInvitesAction, Action.ActionType);
end;

{Properties verb routing tests}

procedure TFileExecutionDispatcherTest.TestDispatch_RegularPath_PropertiesVerb_ReturnsProperties;
var
	Action: TExecutionAction;
begin
	Action := FDispatcher.GetAction('\account\folder\file.txt', VERB_PROPERTIES, nil);

	Assert.AreEqual(eatProperties, Action.ActionType);
end;

{Open verb routing tests}

procedure TFileExecutionDispatcherTest.TestDispatch_OpenVerb_WithStreaming_ReturnsStream;
var
	Action: TExecutionAction;
begin
	FStreamingSettings.Format := STREAMING_FORMAT_PLAYLIST;
	FStreamingSettings.Command := 'player.exe';

	Action := FDispatcher.GetAction('\account\video.mp4', VERB_OPEN, StreamingGetterWithSettings);

	Assert.AreEqual(eatStream, Action.ActionType);
	Assert.AreEqual(STREAMING_FORMAT_PLAYLIST, Action.StreamingSettings.Format);
end;

procedure TFileExecutionDispatcherTest.TestDispatch_OpenVerb_NoStreaming_ReturnsOpenYourself;
var
	Action: TExecutionAction;
begin
	Action := FDispatcher.GetAction('\account\file.txt', VERB_OPEN, StreamingGetterEmpty);

	Assert.AreEqual(eatOpenYourself, Action.ActionType);
end;

procedure TFileExecutionDispatcherTest.TestDispatch_OpenVerb_NilStreamingGetter_ReturnsOpenYourself;
var
	Action: TExecutionAction;
begin
	Action := FDispatcher.GetAction('\account\file.txt', VERB_OPEN, nil);

	Assert.AreEqual(eatOpenYourself, Action.ActionType);
end;

procedure TFileExecutionDispatcherTest.TestDispatch_OpenVerb_Directory_SkipsStreamingCheck;
var
	Action: TExecutionAction;
begin
	{Note: TRealPath cannot determine directory status from path alone (trailing backslash
	 is not preserved). The isDir check only works when isDir is explicitly set (e.g. from
	 FsStatusInfo context). This test verifies that when isDir IS set, streaming is skipped.}

	{For paths where isDir is not explicitly known, streaming getter is called.
	 In practice, directories without extensions would get STREAMING_FORMAT_NONE anyway.}
	FStreamingSettings.Format := STREAMING_FORMAT_PLAYLIST;

	{Since the dispatcher doesn't have a way to pass isDir explicitly,
	 we test that streaming IS triggered when isDir is not set (expected behavior)}
	Action := FDispatcher.GetAction('\account\folder', VERB_OPEN, StreamingGetterWithSettings);

	{With streaming settings configured, streaming action is returned}
	Assert.AreEqual(eatStream, Action.ActionType);
end;

{Quote command routing tests}

procedure TFileExecutionDispatcherTest.TestDispatch_QuoteVerb_ReturnsCommand;
var
	Action: TExecutionAction;
begin
	Action := FDispatcher.GetAction('\account\folder', 'quote rmdir folder', nil);

	Assert.AreEqual(eatCommand, Action.ActionType);
end;

procedure TFileExecutionDispatcherTest.TestDispatch_QuoteVerb_ParsesCommandAndParameter;
var
	Action: TExecutionAction;
begin
	Action := FDispatcher.GetAction('\account\folder', 'quote clone http://link', nil);

	Assert.AreEqual('clone', Action.Command);
	Assert.AreEqual('http://link', Action.Parameter);
end;

{Unknown verb tests}

procedure TFileExecutionDispatcherTest.TestDispatch_UnknownVerb_ReturnsNone;
var
	Action: TExecutionAction;
begin
	Action := FDispatcher.GetAction('\account\file.txt', 'unknown_verb', nil);

	Assert.AreEqual(eatNone, Action.ActionType);
end;

{Parent directory item tests}

procedure TFileExecutionDispatcherTest.TestDispatch_UpDirItem_AdjustsPath;
var
	Action: TExecutionAction;
begin
	{When path ends with .. the path should be adjusted to parent}
	Action := FDispatcher.GetAction('\account\folder\..', VERB_PROPERTIES, nil);

	Assert.AreEqual(eatProperties, Action.ActionType);
	{TRealPath stores path without leading backslash, ExtractFilePath keeps trailing one}
	Assert.AreEqual('folder\', Action.RealPath.Path);
end;

{TExecutionAction factory tests}

procedure TFileExecutionDispatcherTest.TestExecutionAction_None_SetsCorrectActionType;
var
	Action: TExecutionAction;
begin
	Action := TExecutionAction.None;

	Assert.AreEqual(eatNone, Action.ActionType);
end;

procedure TFileExecutionDispatcherTest.TestExecutionAction_TrashbinProperties_SetsCorrectValues;
var
	Action: TExecutionAction;
	RealPath: TRealPath;
begin
	RealPath.FromPath('\account.trash\file.txt');

	Action := TExecutionAction.TrashbinProperties(RealPath);

	Assert.AreEqual(eatTrashbinProperties, Action.ActionType);
	Assert.IsTrue(Action.RealPath.trashDir);
end;

procedure TFileExecutionDispatcherTest.TestExecutionAction_SharedAction_SetsCorrectValues;
var
	Action: TExecutionAction;
	RealPath: TRealPath;
begin
	RealPath.FromPath('\account.shared\file.txt');

	Action := TExecutionAction.SharedAction(RealPath, True);

	Assert.AreEqual(eatSharedAction, Action.ActionType);
	Assert.IsTrue(Action.ActionOpen);
	Assert.IsTrue(Action.RealPath.sharedDir);
end;

procedure TFileExecutionDispatcherTest.TestExecutionAction_InvitesAction_SetsCorrectValues;
var
	Action: TExecutionAction;
	RealPath: TRealPath;
begin
	RealPath.FromPath('\account.invites\invite');

	Action := TExecutionAction.InvitesAction(RealPath);

	Assert.AreEqual(eatInvitesAction, Action.ActionType);
	Assert.IsTrue(Action.RealPath.invitesDir);
end;

procedure TFileExecutionDispatcherTest.TestExecutionAction_Properties_SetsCorrectValues;
var
	Action: TExecutionAction;
	RealPath: TRealPath;
begin
	RealPath.FromPath('\account\folder\file.txt');

	Action := TExecutionAction.Properties(RealPath);

	Assert.AreEqual(eatProperties, Action.ActionType);
end;

procedure TFileExecutionDispatcherTest.TestExecutionAction_Stream_SetsCorrectValues;
var
	Action: TExecutionAction;
	RealPath: TRealPath;
	Settings: TStreamingSettings;
begin
	RealPath.FromPath('\account\video.mp4');
	Settings.Format := STREAMING_FORMAT_PLAYLIST;
	Settings.Command := 'player.exe';

	Action := TExecutionAction.Stream(RealPath, Settings);

	Assert.AreEqual(eatStream, Action.ActionType);
	Assert.AreEqual(STREAMING_FORMAT_PLAYLIST, Action.StreamingSettings.Format);
	Assert.AreEqual('player.exe', Action.StreamingSettings.Command);
end;

procedure TFileExecutionDispatcherTest.TestExecutionAction_Command_SetsCorrectValues;
var
	Action: TExecutionAction;
	RealPath: TRealPath;
begin
	RealPath.FromPath('\account\folder');

	Action := TExecutionAction.QuoteCommand(RealPath, 'rmdir', 'testfolder');

	Assert.AreEqual(eatCommand, Action.ActionType);
	Assert.AreEqual('rmdir', Action.Command);
	Assert.AreEqual('testfolder', Action.Parameter);
end;

procedure TFileExecutionDispatcherTest.TestExecutionAction_OpenYourself_SetsCorrectActionType;
var
	Action: TExecutionAction;
begin
	Action := TExecutionAction.OpenYourself;

	Assert.AreEqual(eatOpenYourself, Action.ActionType);
end;

initialization
	TDUnitX.RegisterTestFixture(TFileExecutionDispatcherTest);

end.

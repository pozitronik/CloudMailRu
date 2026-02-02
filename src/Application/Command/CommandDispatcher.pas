unit CommandDispatcher;

{Handles plugin command dispatching for Total Commander quote commands.
	Extracts command routing logic from MailRuCloudWFX.}

interface

uses
	SysUtils,
	Logger,
	PluginSettingsManager,
	ConnectionManager,
	CloudMailRu,
	RealPath,
	HashInfo,
	CloudConstants,
	SettingsConstants,
	LanguageStrings,
	WFXTypes;

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

	TCommandDispatcher = class(TInterfacedObject, ICommandDispatcher)
	private
		FConnectionManager: IConnectionManager;
		FLogger: ILogger;
		FSettingsManager: IPluginSettingsManager;

		function ExecuteRmdir(const RemoteName, Parameter: WideString): TCommandResult;
		function ExecuteShare(Cloud: TCloudMailRu; const Path, Parameter: WideString): TCommandResult;
		function ExecuteHash(Cloud: TCloudMailRu; const Path, Parameter: WideString): TCommandResult;
		function ExecuteClone(Cloud: TCloudMailRu; const Path, Parameter: WideString): TCommandResult;
		function ExecuteVirtualNavigation(Cloud: TCloudMailRu; const Account: WideString; IsInAccount: Boolean; const Postfix: WideString): TCommandResult;
	public
		{Create command dispatcher with required dependencies.
			@param AConnectionManager Connection manager for cloud access
			@param ALogger Logger for error reporting
			@param ASettingsManager Settings manager for configuration access}
		constructor Create(AConnectionManager: IConnectionManager; ALogger: ILogger; ASettingsManager: IPluginSettingsManager);

		function Execute(const RemoteName, Command, Parameter: WideString): TCommandResult;
	end;

implementation

{Converts public link URL to the required format if needed}
function ExtractLinkFromUrl(URL: WideString): WideString;
const
	pulicPrefix = 'https://cloud.mail.ru/public';
begin
	Result := URL;
	if Pos(WideString(pulicPrefix), URL) <> 0 then
		Result := Copy(URL, Length(pulicPrefix) + 1, Length(URL) - Length(pulicPrefix));
end;

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

constructor TCommandDispatcher.Create(AConnectionManager: IConnectionManager; ALogger: ILogger; ASettingsManager: IPluginSettingsManager);
begin
	inherited Create;
	FConnectionManager := AConnectionManager;
	FLogger := ALogger;
	FSettingsManager := ASettingsManager;
end;

function TCommandDispatcher.Execute(const RemoteName, Command, Parameter: WideString): TCommandResult;
var
	Path: TRealPath;
	Cloud: TCloudMailRu;
begin
	Result := TCommandResult.OK;

	{rmdir has special path handling - combines RemoteName with Parameter}
	if Command = 'rmdir' then
		exit(ExecuteRmdir(RemoteName, Parameter));

	{All other commands use default path from RemoteName}
	Path.FromPath(RemoteName);
	Cloud := FConnectionManager.Get(Path.Account);

	if Command = 'share' then
		exit(ExecuteShare(Cloud, Path.Path, Parameter));

	if Command = 'hash' then
		exit(ExecuteHash(Cloud, Path.Path, Parameter));

	if Command = 'clone' then
		exit(ExecuteClone(Cloud, Path.Path, Parameter));

	if Command = 'trash' then
		exit(ExecuteVirtualNavigation(Cloud, Path.Account, Path.IsInAccount(false), TrashPostfix));

	if Command = 'shared' then
		exit(ExecuteVirtualNavigation(Cloud, Path.Account, Path.IsInAccount(false), SharedPostfix));

	if Command = 'invites' then
		exit(ExecuteVirtualNavigation(Cloud, Path.Account, Path.IsInAccount(false), InvitesPostfix));
end;

function TCommandDispatcher.ExecuteRmdir(const RemoteName, Parameter: WideString): TCommandResult;
var
	Path: TRealPath;
begin
	Path.FromPath(RemoteName + Parameter);
	if FConnectionManager.Get(Path.Account).FileOperations.RemoveDirectory(Path.Path) then
		Result := TCommandResult.OK
	else
		Result := TCommandResult.Error;
end;

function TCommandDispatcher.ExecuteShare(Cloud: TCloudMailRu; const Path, Parameter: WideString): TCommandResult;
begin
	{Undocumented command: share current folder to email}
	if Cloud.shareFolder(Path, ExtractLinkFromUrl(Parameter), CLOUD_SHARE_RW) then
		Result := TCommandResult.OK
	else
		Result := TCommandResult.Error;
end;

function TCommandDispatcher.ExecuteHash(Cloud: TCloudMailRu; const Path, Parameter: WideString): TCommandResult;
var
	Info: THashInfo;
begin
	{Add file by hash and filesize}
	Info := THashInfo.Create(Parameter);
	try
		if Info.valid then
		begin
			Cloud.Uploader.AddFileByIdentity(Info.CloudFileIdentity, IncludeTrailingPathDelimiter(Path) + Info.name, CLOUD_CONFLICT_RENAME);
			Result := TCommandResult.OK;
		end else begin
			FLogger.Log(LOG_LEVEL_DEBUG, msgtype_details, ERR_CLONE_BY_HASH, [Info.errorString, Parameter]);
			Result := TCommandResult.Error;
		end;
	finally
		Info.Free;
	end;
end;

function TCommandDispatcher.ExecuteClone(Cloud: TCloudMailRu; const Path, Parameter: WideString): TCommandResult;
begin
	{Clone file by weblink}
	if Cloud.ShareService.CloneWeblink(Path, ExtractLinkFromUrl(Parameter)) = CLOUD_OPERATION_OK then
	begin
		if FSettingsManager.GetSettings.LogUserSpace then
			Cloud.logUserSpaceInfo;
		Result := TCommandResult.OK;
	end
	else
		Result := TCommandResult.Error;
end;

function TCommandDispatcher.ExecuteVirtualNavigation(Cloud: TCloudMailRu; const Account: WideString; IsInAccount: Boolean; const Postfix: WideString): TCommandResult;
begin
	{Navigate to virtual directory (trash, shared, invites)}
	if Cloud.IsPublicAccount then
		exit(TCommandResult.Error);

	if IsInAccount then
		Result := TCommandResult.Symlink('\' + Account + Postfix)
	else
		Result := TCommandResult.OK;
end;

end.

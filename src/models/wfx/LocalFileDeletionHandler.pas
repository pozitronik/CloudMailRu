unit LocalFileDeletionHandler;

{Local file deletion handler with retry and error mode handling.
 Handles configurable behavior when file deletion fails during move operations:
 - Ask: Prompt user with Retry/Abort/Ignore options
 - Ignore: Log and continue
 - Abort: Log and stop operation
 - DeleteIgnore/DeleteAbort: Try clearing readonly attribute, then ignore or abort}

interface

uses
	Windows,
	SysUtils,
	ILocalFileDeletionHandlerInterface,
	IPluginSettingsManagerInterface,
	ILoggerInterface,
	PLUGIN_TYPES,
	CMRConstants,
	SETTINGS_CONSTANTS,
	LANGUAGE_STRINGS,
	PathHelper;

type
	TLocalFileDeletionHandler = class(TInterfacedObject, ILocalFileDeletionHandler)
	private
		FSettings: IPluginSettingsManager;
		FLogger: ILogger;
		FDeleteFile: TDeleteFileFunc;
		FGetFileAttr: TGetFileAttrFunc;
		FSetFileAttr: TSetFileAttrFunc;
		FAskUser: TAskDeleteModeFunc;

		{Tries to delete readonly file by clearing attribute first}
		function TryDeleteReadonlyFile(const UNCPath, DisplayPath: WideString): Boolean;
		{Handles the case when initial delete fails}
		function HandleDeleteFailure(const UNCPath, DisplayPath: WideString; Mode: Integer): Integer;
	public
		{Create handler with dependencies.
		 @param Settings Plugin settings for getting delete mode
		 @param Logger For logging delete operations
		 @param DeleteFile Callback to delete file (returns true on success)
		 @param GetFileAttr Callback to get file attributes
		 @param SetFileAttr Callback to set file attributes (returns true on success)
		 @param AskUser Callback to prompt user (returns IDRETRY/IDABORT/IDIGNORE)}
		constructor Create(
			Settings: IPluginSettingsManager;
			Logger: ILogger;
			DeleteFile: TDeleteFileFunc;
			GetFileAttr: TGetFileAttrFunc;
			SetFileAttr: TSetFileAttrFunc;
			AskUser: TAskDeleteModeFunc
		);

		function DeleteLocalFile(const LocalPath: WideString): Integer;
	end;

implementation

constructor TLocalFileDeletionHandler.Create(
	Settings: IPluginSettingsManager;
	Logger: ILogger;
	DeleteFile: TDeleteFileFunc;
	GetFileAttr: TGetFileAttrFunc;
	SetFileAttr: TSetFileAttrFunc;
	AskUser: TAskDeleteModeFunc
);
begin
	inherited Create;
	FSettings := Settings;
	FLogger := Logger;
	FDeleteFile := DeleteFile;
	FGetFileAttr := GetFileAttr;
	FSetFileAttr := SetFileAttr;
	FAskUser := AskUser;
end;

function TLocalFileDeletionHandler.TryDeleteReadonlyFile(const UNCPath, DisplayPath: WideString): Boolean;
var
	Attr: Integer;
begin
	Result := False;
	Attr := FGetFileAttr(UNCPath);
	{Check if file has readonly attribute (using OR to check, not AND - preserves original logic)}
	if ((Attr or faReadOnly) <> 0) and FSetFileAttr(UNCPath, not faReadOnly) and FDeleteFile(UNCPath) then
	begin
		FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_IMPORTANTERROR, ERR_DELETE_FILE_DELETE, [DisplayPath]);
		Result := True;
	end;
end;

function TLocalFileDeletionHandler.HandleDeleteFailure(const UNCPath, DisplayPath: WideString; Mode: Integer): Integer;
begin
	case Mode of
		DeleteFailOnUploadAbort:
			begin
				FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_IMPORTANTERROR, ERR_DELETE_FILE_ABORT, [DisplayPath]);
				Result := FS_FILE_NOTSUPPORTED;
			end;
		DeleteFailOnUploadDeleteIgnore, DeleteFailOnUploadDeleteAbort:
			begin
				{Try clearing readonly attribute and deleting}
				if TryDeleteReadonlyFile(UNCPath, DisplayPath) then
					Result := FS_FILE_OK
				else begin
					{Couldn't delete even after clearing readonly - check configured fallback}
					if FSettings.GetSettings.DeleteFailOnUploadMode = DeleteFailOnUploadDeleteIgnore then
					begin
						FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_IMPORTANTERROR, ERR_DELETE_FILE_IGNORE, [DisplayPath]);
						Result := FS_FILE_OK;
					end else begin
						FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_IMPORTANTERROR, ERR_DELETE_FILE_ABORT, [DisplayPath]);
						Result := FS_FILE_NOTSUPPORTED;
					end;
				end;
			end;
		else
			begin
				{Ignore mode or unknown - just log and continue}
				FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_IMPORTANTERROR, ERR_DELETE_FILE_IGNORE, [DisplayPath]);
				Result := FS_FILE_OK;
			end;
	end;
end;

function TLocalFileDeletionHandler.DeleteLocalFile(const LocalPath: WideString): Integer;
var
	UNCLocalName: WideString;
	DeleteMode, UserChoice: Integer;
begin
	Result := FS_FILE_OK;
	UserChoice := IDRETRY;
	UNCLocalName := GetUNCFilePath(LocalPath);

	{Retry loop - continues while delete fails and user chooses retry}
	while (not FDeleteFile(UNCLocalName)) and (UserChoice = IDRETRY) do
	begin
		DeleteMode := FSettings.GetSettings.DeleteFailOnUploadMode;

		{Ask mode - prompt user for action}
		if DeleteMode = DeleteFailOnUploadAsk then
		begin
			UserChoice := FAskUser(LocalPath);
			case UserChoice of
				IDRETRY:
					continue; {Loop will retry deletion}
				IDABORT:
					DeleteMode := DeleteFailOnUploadAbort;
				IDIGNORE:
					DeleteMode := DeleteFailOnUploadIgnore;
			end;
		end;

		{Handle based on resolved mode}
		Result := HandleDeleteFailure(UNCLocalName, LocalPath, DeleteMode);
		Exit; {Exit loop after handling}
	end;
end;

end.

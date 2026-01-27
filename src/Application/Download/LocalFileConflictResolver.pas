unit LocalFileConflictResolver;

{Resolves local file conflicts during download operations.
	Handles OverwriteLocalMode setting (Ask/Ignore/Overwrite) when target exists.}

interface

uses
	WFXTypes,
	TCLogger;

type
	{Result of conflict resolution}
	TConflictResolution = record
		ShouldProceed: Boolean; {True to continue with download}
		ResultCode: Integer; {FS_FILE_* code to return if not proceeding}
	end;

	ILocalFileConflictResolver = interface
		['{C7F4E2A9-8D1B-4C5E-B3A6-2F9E1D8C7B5A}']

		{Resolves conflict when local file exists.
			@param LocalPath Path to local file
			@param CopyFlags TC copy flags (check for FS_COPYFLAGS_OVERWRITE)
			@param OverwriteMode Setting: Ask/Ignore/Overwrite
			@return Resolution with ShouldProceed and ResultCode}
		function Resolve(const LocalPath: WideString; CopyFlags: Integer; OverwriteMode: Integer): TConflictResolution;
	end;

	TLocalFileConflictResolver = class(TInterfacedObject, ILocalFileConflictResolver)
	private
		FLogger: ILogger;
	public
		constructor Create(Logger: ILogger);

		function Resolve(const LocalPath: WideString; CopyFlags: Integer; OverwriteMode: Integer): TConflictResolution;
	end;

implementation

uses
	SysUtils,
	CMRConstants,
	LANGUAGE_STRINGS,
	SettingsConstants,
	SystemHelper,
	PathHelper;

constructor TLocalFileConflictResolver.Create(Logger: ILogger);
begin
	inherited Create;
	FLogger := Logger;
end;

function TLocalFileConflictResolver.Resolve(const LocalPath: WideString; CopyFlags: Integer; OverwriteMode: Integer): TConflictResolution;
begin
	Result.ShouldProceed := True;
	Result.ResultCode := FS_FILE_OK;

	{If overwrite flag is set, always proceed}
	if CheckFlag(FS_COPYFLAGS_OVERWRITE, CopyFlags) then
		Exit;

	{If file doesn't exist, proceed}
	if not FileExists(GetUNCFilePath(LocalPath)) then
		Exit;

	{File exists and overwrite not requested - check mode setting}
	case OverwriteMode of
		OverwriteLocalModeAsk:
			begin
				{Return EXISTS to let TC ask user}
				Result.ShouldProceed := False;
				Result.ResultCode := FS_FILE_EXISTS;
			end;
		OverwriteLocalModeIgnore:
			begin
				{Skip this file silently}
				FLogger.Log(LOG_LEVEL_DETAIL, msgtype_details, FILE_EXISTS_IGNORE, [LocalPath]);
				Result.ShouldProceed := False;
				Result.ResultCode := FS_FILE_OK;
			end;
		OverwriteLocalModeOverwrite:
			begin
				{Proceed with overwrite}
				FLogger.Log(LOG_LEVEL_DETAIL, msgtype_details, FILE_EXISTS_OVERWRITE, [LocalPath]);
				Result.ShouldProceed := True;
			end;
	end;
end;

end.

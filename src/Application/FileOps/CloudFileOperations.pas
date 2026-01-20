unit CloudFileOperations;

{Service for file operations on Cloud.Mail.ru (create/delete/move/copy/rename)}

interface

uses
	CMRConstants,
	CloudHTTP,
	LANGUAGE_STRINGS,
	PLUGIN_TYPES,
	TCLogger,
	TokenRetryHelper,
	PathHelper,
	System.SysUtils;

type
	{Callback types for dynamic state access}
	TIsPublicAccountFunc = reference to function: Boolean;
	TGetUnitedParamsFunc = reference to function: WideString;

	{Interface for file operations service}
	ICloudFileOperations = interface
		['{9CF7178B-77CC-401A-BD7C-61E34A4C1181}']
		{Create a directory at specified path}
		function CreateDirectory(Path: WideString): Boolean;
		{Remove a directory at specified path}
		function RemoveDirectory(Path: WideString): Boolean;
		{Delete a file at specified path}
		function Delete(Path: WideString): Boolean;
		{Rename a file (same directory only)}
		function Rename(OldName, NewName: WideString): Integer;
		{Move a file to a different directory (no rename)}
		function MoveToPath(OldName, ToPath: WideString): Integer;
		{Copy a file to a different directory (no rename)}
		function CopyToPath(OldName, ToPath: WideString): Integer;
		{Move a file with optional rename (combines move + rename if needed)}
		function Move(OldName, NewName: WideString): Integer;
		{Copy a file with optional rename (combines copy + rename if needed)}
		function Copy(OldName, NewName: WideString): Integer;
	end;

	{Implementation of file operations service}
	TCloudFileOperations = class(TInterfacedObject, ICloudFileOperations)
	private
		FHTTP: ICloudHTTP;
		FLogger: ILogger;
		FRetryOperation: TRetryOperation;
		FIsPublicAccount: TIsPublicAccountFunc;
		FGetUnitedParams: TGetUnitedParamsFunc;
	public
		constructor Create(HTTP: ICloudHTTP; Logger: ILogger; RetryOperation: TRetryOperation; IsPublicAccount: TIsPublicAccountFunc; GetUnitedParams: TGetUnitedParamsFunc);

		{ICloudFileOperations implementation}
		function CreateDirectory(Path: WideString): Boolean;
		function RemoveDirectory(Path: WideString): Boolean;
		function Delete(Path: WideString): Boolean;
		function Rename(OldName, NewName: WideString): Integer;
		function MoveToPath(OldName, ToPath: WideString): Integer;
		function CopyToPath(OldName, ToPath: WideString): Integer;
		function Move(OldName, NewName: WideString): Integer;
		function Copy(OldName, NewName: WideString): Integer;
	end;

implementation

{TCloudFileOperations}

constructor TCloudFileOperations.Create(HTTP: ICloudHTTP; Logger: ILogger; RetryOperation: TRetryOperation; IsPublicAccount: TIsPublicAccountFunc; GetUnitedParams: TGetUnitedParamsFunc);
begin
	inherited Create;
	FHTTP := HTTP;
	FLogger := Logger;
	FRetryOperation := RetryOperation;
	FIsPublicAccount := IsPublicAccount;
	FGetUnitedParams := GetUnitedParams;
end;

function TCloudFileOperations.CreateDirectory(Path: WideString): Boolean;
begin
	Result := False;
	if FIsPublicAccount() then
		Exit;
	FHTTP.SetProgressNames(CREATE_DIRECTORY, Path);
	Result := FRetryOperation.PostFormBoolean(API_FOLDER_ADD + '?' + FGetUnitedParams(), Format('home=/%s&conflict', [PathToUrl(Path)]), EmptyWideStr);
end;

function TCloudFileOperations.RemoveDirectory(Path: WideString): Boolean;
begin
	Result := False;
	if FIsPublicAccount() then
		Exit;
	FHTTP.SetProgressNames(DELETE_DIR, Path);
	{API always returns true even if path doesn't exist}
	Result := FRetryOperation.PostFormBoolean(API_FILE_REMOVE + '?' + FGetUnitedParams(), Format('home=/%s&conflict', [IncludeSlash(PathToUrl(Path))]), PREFIX_ERR_DELETE_DIR);
end;

function TCloudFileOperations.Delete(Path: WideString): Boolean;
begin
	Result := False;
	if FIsPublicAccount() then
		Exit;
	FHTTP.SetProgressNames(DELETE_FILE, Path);
	Result := FRetryOperation.PostFormBoolean(API_FILE_REMOVE + '?' + FGetUnitedParams(), Format('home=/%s&conflict', [PathToUrl(Path)]), PREFIX_ERR_DELETE_FILE);
end;

function TCloudFileOperations.Rename(OldName, NewName: WideString): Integer;
begin
	Result := FS_FILE_WRITEERROR;
	if FIsPublicAccount() then
		Exit;
	Result := FRetryOperation.PostFormInteger(API_FILE_RENAME + '?' + FGetUnitedParams(), Format('home=%s&name=%s', [PathToUrl(OldName), PathToUrl(NewName)]), PREFIX_ERR_FILE_RENAME);
end;

function TCloudFileOperations.MoveToPath(OldName, ToPath: WideString): Integer;
begin
	if FIsPublicAccount() then
		Exit(FS_FILE_NOTSUPPORTED);
	Result := FRetryOperation.PostFormInteger(API_FILE_MOVE + '?' + FGetUnitedParams(), Format('home=%s&folder=%s&conflict', [PathToUrl(OldName), PathToUrl(ToPath)]), PREFIX_ERR_FILE_MOVE);
end;

function TCloudFileOperations.CopyToPath(OldName, ToPath: WideString): Integer;
begin
	if FIsPublicAccount() then
		Exit(FS_FILE_NOTSUPPORTED);
	FHTTP.SetProgressNames(OldName, Format('%s%s', [IncludeSlash(ToPath), ExtractUniversalFileName(OldName)]));
	Result := FRetryOperation.PostFormInteger(API_FILE_COPY + '?' + FGetUnitedParams(), Format('home=/%s&folder=/%s&conflict', [PathToUrl(OldName), PathToUrl(ToPath)]), PREFIX_ERR_FILE_COPY);
end;

function TCloudFileOperations.Move(OldName, NewName: WideString): Integer;
var
	NewPath: WideString;
	SameDir, SameName: Boolean;
begin
	{Rename and move are different operations in the cloud API}
	NewPath := ExtractUniversalFilePath(NewName);
	SameDir := ExtractUniversalFilePath(OldName) = ExtractUniversalFilePath(NewName);
	SameName := ExtractUniversalFileName(OldName) = ExtractUniversalFileName(NewName);
	if SameDir then
	begin {Same directory - just rename}
		Result := Rename(OldName, ExtractUniversalFileName(NewName));
	end else begin
		{Move will fail if file with old name exists in target directory}
		Result := MoveToPath(OldName, ExtractUniversalFilePath(NewName));
		if Result <> CLOUD_OPERATION_OK then
			Exit;
		if not(SameName) then
		begin {File now sits in new directory with old name - rename it}
			Result := Rename(Format('%s%s', [NewPath, ExtractUniversalFileName(OldName)]), ExtractUniversalFileName(NewName));
		end;
	end;
end;

function TCloudFileOperations.Copy(OldName, NewName: WideString): Integer;
var
	NewPath: WideString;
	SameDir, SameName: Boolean;
begin
	{Cloud can copy files but cannot rename during copy - workaround needed}
	NewPath := ExtractUniversalFilePath(NewName);
	SameDir := ExtractUniversalFilePath(OldName) = ExtractUniversalFilePath(NewName);
	SameName := ExtractUniversalFileName(OldName) = ExtractUniversalFileName(NewName);
	if (SameDir) then
	begin {Copy to same directory not supported - would need temp dir workaround}
		FLogger.Log(LOG_LEVEL_WARNING, MSGTYPE_IMPORTANTERROR, ERR_COPY_SAME_DIR_NOT_SUPPORTED);
		Exit(FS_FILE_NOTSUPPORTED);
	end else begin
		{TODO: issue #219 - handle existing file case}
		Result := CopyToPath(OldName, NewPath);
		if Result <> CLOUD_OPERATION_OK then
			Exit;
	end;
	if not(SameName) then
	begin {Copied file now sits in new directory with old name - rename it}
		Result := Rename(Format('%s%s', [NewPath, ExtractUniversalFileName(OldName)]), ExtractUniversalFileName(NewName));
	end;
end;

end.

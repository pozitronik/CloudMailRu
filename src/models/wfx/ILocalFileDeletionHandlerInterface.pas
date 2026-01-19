unit ILocalFileDeletionHandlerInterface;

{Interface for local file deletion with retry and error mode handling.
 Encapsulates the complex retry logic, user prompts, and readonly attribute
 handling that occurs when deleting local files after move operations.}

interface

uses
	PLUGIN_TYPES;

type
	{Callback types for testability - allows mocking file operations and user dialogs}
	TDeleteFileFunc = reference to function(const Path: WideString): Boolean;
	TGetFileAttrFunc = reference to function(const Path: WideString): Integer;
	TSetFileAttrFunc = reference to function(const Path: WideString; Attr: Integer): Boolean;
	{Ask user what to do when delete fails. Returns IDRETRY, IDABORT, or IDIGNORE}
	TAskDeleteModeFunc = reference to function(const FileName: WideString): Integer;

	ILocalFileDeletionHandler = interface
		['{D5E6F7A8-B9C0-1D2E-3F4A-5B6C7D8E9F0A}']

		{Attempts to delete a local file with retry and error mode handling.
		 @param LocalPath Path to the local file to delete
		 @return FS_FILE_OK on success, FS_FILE_NOTSUPPORTED on abort}
		function DeleteLocalFile(const LocalPath: WideString): Integer;
	end;

	{Null implementation for testing - always succeeds}
	TNullLocalFileDeletionHandler = class(TInterfacedObject, ILocalFileDeletionHandler)
	public
		function DeleteLocalFile(const LocalPath: WideString): Integer;
	end;

implementation

{TNullLocalFileDeletionHandler}

function TNullLocalFileDeletionHandler.DeleteLocalFile(const LocalPath: WideString): Integer;
begin
	Result := FS_FILE_OK;
end;

end.

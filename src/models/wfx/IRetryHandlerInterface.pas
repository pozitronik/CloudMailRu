unit IRetryHandlerInterface;

{Interface for operation retry handling.
 Abstracts the common error handling pattern used in file operations.}

interface

uses
	SysUtils;

type
	{Identifies which retry counter to use for tracking attempts}
	TRetryOperationType = (rotDownload, rotUpload, rotRenMov);

	{Callback type for the operation to retry}
	TRetryOperation = reference to function: Integer;

	{Callback type for checking if user aborted}
	TAbortCheck = reference to function: Boolean;

	IRetryHandler = interface
		['{EAFF9068-C0C3-41AE-B0A0-AC2C13793E5E}']

		{Handle operation error based on configured error mode.
		 Implements the common retry pattern: Ask/Ignore/Abort/Retry modes.
		 @param CurrentResult The failed operation result code
		 @param OperationType Which retry counter to use (Download/Upload/RenMov)
		 @param AskMessage Message template for Ask mode dialog
		 @param AskTitle Title for Ask mode dialog
		 @param RetryLogMessage Log message template for retry attempts
		 @param FormatParam Single format parameter for messages (filename or error text)
		 @param RetryOperation Callback that performs the actual operation
		 @param AbortCheck Callback that checks if user requested abort
		 @return Final operation result after handling}
		function HandleOperationError(
			CurrentResult: Integer;
			OperationType: TRetryOperationType;
			const AskMessage, AskTitle, RetryLogMessage, FormatParam: WideString;
			RetryOperation: TRetryOperation;
			AbortCheck: TAbortCheck
		): Integer;
	end;

implementation

end.

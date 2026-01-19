unit IOverwritePreparationHandlerInterface;

{Interface for handling file overwrite preparation.
 Deletes existing remote file when overwrite is required.}

interface

uses
	RealPath;

type
	TOverwritePreparationResult = record
		Success: Boolean;
		ResultCode: Integer;
	end;

	IOverwritePreparationHandler = interface
		['{4F6DB3F8-9A3C-4AA2-8C03-ABF2189FF5B0}']

		{Prepares for overwrite by deleting existing file if required.
		 @param Path Remote path to prepare
		 @param RequiresOverwrite Whether overwrite was requested
		 @return Result with Success=True if ready to proceed, or ResultCode on failure}
		function Prepare(const Path: TRealPath; RequiresOverwrite: Boolean): TOverwritePreparationResult;
	end;

implementation

end.

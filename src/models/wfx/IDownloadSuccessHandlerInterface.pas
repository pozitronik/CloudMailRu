unit IDownloadSuccessHandlerInterface;

{Interface for post-download success operations.
 Encapsulates CRC verification, timestamp preservation, move cleanup,
 progress reporting, logging, and description sync.}

interface

uses
	PLUGIN_TYPES,
	RealPath,
	CMRDirItem,
	CloudMailRu;

type
	{Context for download success handling}
	TDownloadContext = record
		RemotePath: TRealPath;
		LocalName: WideString;
		RemoteName: WideString;
		CopyFlags: Integer;
		ResultHash: WideString;  {Hash calculated during download, empty if not calculated}
		Item: TCMRDirItem;       {Directory item with expected hash and mtime}
		Cloud: TCloudMailRu;     {Cloud connection for move operations}
	end;

	IDownloadSuccessHandler = interface
		['{E7F8A9B0-C1D2-3E4F-5A6B-7C8D9E0F1A2B}']

		{Handles all post-download success operations.
		 @param Context Download context with all required data
		 @return FS_FILE_OK on success, FS_FILE_READERROR if CRC mismatch}
		function HandleSuccess(const Context: TDownloadContext): Integer;
	end;

	{Null implementation for testing - always succeeds}
	TNullDownloadSuccessHandler = class(TInterfacedObject, IDownloadSuccessHandler)
	public
		function HandleSuccess(const Context: TDownloadContext): Integer;
	end;

implementation

{TNullDownloadSuccessHandler}

function TNullDownloadSuccessHandler.HandleSuccess(const Context: TDownloadContext): Integer;
begin
	Result := FS_FILE_OK;
end;

end.

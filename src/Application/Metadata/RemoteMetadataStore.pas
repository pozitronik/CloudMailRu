unit RemoteMetadataStore;

{Common interface for remote metadata file stores.
	Abstracts key-value entry operations so that the shared sync lifecycle
	(download-modify-reupload) in TBaseRemoteMetadataSyncManager can work
	with any metadata type (descriptions, timestamps, etc.) uniformly.}

interface

type
	IRemoteMetadataStore = interface
		['{F8A2D7E1-3B94-4C6F-A5E8-1D9F7B3C6E2A}']

		{Load entries from the backing file}
		procedure Read;

		{Persist entries to the backing file. Deletes file if no entries remain.}
		procedure Write;

		{Remove entry by filename key.
			@return True if entry existed}
		function DeleteEntry(const Key: WideString): Boolean;

		{Rename entry key in-place.
			@return True if old key existed and was renamed}
		function RenameEntry(const OldKey, NewKey: WideString): Boolean;

		{Get entry value serialized as string (empty = not found).
			Used for cross-store transfers where the concrete entry type is unknown.}
		function GetEntryAsString(const Key: WideString): WideString;

		{Set entry from a serialized string value (empty = delete).
			Inverse of GetEntryAsString.}
		procedure SetEntryFromString(const Key, Value: WideString);

		{Path of the backing file}
		function GetFileName: WideString;
	end;

implementation

end.

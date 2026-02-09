unit CloudFileVersion;

{Represents a single version entry from the file/history API response.
	Paid accounts include hash and rev; free accounts only have timestamps and sizes.}

interface

type
	TCloudFileVersion = record
		Hash: WideString;     {Empty for free accounts}
		Name: WideString;
		Path: WideString;
		Size: Int64;
		Time: Int64;          {Unix timestamp}
		Rev: Integer;         {Revision number, 0 if absent}
		UID: Integer;

		function HasHash: Boolean;
	end;

	TCloudFileVersionList = TArray<TCloudFileVersion>;

implementation

{TCloudFileVersion}

function TCloudFileVersion.HasHash: Boolean;
begin
	Result := Hash <> '';
end;

end.

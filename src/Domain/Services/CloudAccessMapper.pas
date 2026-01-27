unit CloudAccessMapper;

{Utility functions for converting between cloud access level representations.
	Handles conversion between API constants (read_only/read_write), human-readable
	strings (read only/read and write), and integer access codes (CLOUD_SHARE_RO/RW).}

interface

uses
	CMRConstants;

type
	TCloudAccessMapper = class
	public
		{Converts access level to human-readable string.
			Accepts both API constants (read_only) and human strings (read only).
			Invert=True swaps read-only <-> read-write before converting.}
		class function AccessToString(Access: WideString; Invert: Boolean = False): WideString; static;

		{Converts access string to integer access code (CLOUD_SHARE_RO/RW).
			Accepts both API constants (read_only) and human strings (read only).
			Invert=True swaps read-only <-> read-write before converting.}
		class function StringToAccess(AccessString: WideString; Invert: Boolean = False): Integer; static;
	end;

implementation

class function TCloudAccessMapper.AccessToString(Access: WideString; Invert: Boolean): WideString;
begin
	{Normalize human-readable input to API constants}
	if Access = 'read only' then
		Access := CLOUD_SHARE_ACCESS_READ_ONLY;
	if Access = 'read and write' then
		Access := CLOUD_SHARE_ACCESS_READ_WRITE;

	{Apply inversion if requested}
	if Invert then
	begin
		if Access = CLOUD_SHARE_ACCESS_READ_ONLY then
			Access := CLOUD_SHARE_ACCESS_READ_WRITE
		else
			Access := CLOUD_SHARE_ACCESS_READ_ONLY;
	end;

	{Convert to human-readable output}
	if Access = CLOUD_SHARE_ACCESS_READ_ONLY then
		Result := 'read only'
	else
		Result := 'read and write';
end;

class function TCloudAccessMapper.StringToAccess(AccessString: WideString; Invert: Boolean): Integer;
begin
	{Normalize human-readable input to API constants}
	if AccessString = 'read only' then
		AccessString := CLOUD_SHARE_ACCESS_READ_ONLY;
	if AccessString = 'read and write' then
		AccessString := CLOUD_SHARE_ACCESS_READ_WRITE;

	{Apply inversion if requested}
	if Invert then
	begin
		if AccessString = CLOUD_SHARE_ACCESS_READ_ONLY then
			AccessString := CLOUD_SHARE_ACCESS_READ_WRITE
		else
			AccessString := CLOUD_SHARE_ACCESS_READ_ONLY;
	end;

	{Convert to integer access code}
	if AccessString = CLOUD_SHARE_ACCESS_READ_ONLY then
		Result := CLOUD_SHARE_RO
	else
		Result := CLOUD_SHARE_RW;
end;

end.

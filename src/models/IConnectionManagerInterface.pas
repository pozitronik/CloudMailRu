unit IConnectionManagerInterface;

{Interface for managing cloud connections by account name.
 Provides connection pooling - creates connections on first access,
 returns existing connections on subsequent calls.}

interface

uses
	CloudMailRu;

type
	IConnectionManager = interface
		['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']

		{Returns cloud connection for account, creating if needed.
		 @param ConnectionName Account name to get connection for
		 @param OperationResult Output status code (CLOUD_OPERATION_OK on success)
		 @return TCloudMailRu instance or nil on failure}
		function Get(ConnectionName: WideString; var OperationResult: Integer): TCloudMailRu;

		{Releases connection for account if it exists.
		 @param ConnectionName Account name to release connection for}
		procedure Free(ConnectionName: WideString);
	end;

implementation

end.

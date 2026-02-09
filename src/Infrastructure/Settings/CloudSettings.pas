unit CloudSettings;

interface

uses
	AccountSettings,
	ConnectionSettings,
	PluginSettings,
	CloudEndpoints;

type
	{Aggregated settings required by the cloud class}
	TCloudSettings = record
		{Settings inherited from global plugin settings}
		ConnectionSettings: TConnectionSettings;
		AccountSettings: TAccountSettings;

		PrecalculateHash: boolean;
		ForcePrecalculateSize: int64;
		CheckCRC: boolean;
		HashCalculatorStrategy: integer;
		CloudMaxFileSize: int64;
		OperationErrorMode: integer;
		RetryAttempts: integer;
		AttemptWait: integer;

		CryptFilesPassword: WideString;

		Endpoints: TCloudEndpoints; {Resolved endpoints for this cloud instance}

		{Factory method to create CloudSettings from plugin and account settings}
		class function CreateFromSettings(const PluginSettings: TPluginSettings; const AccSettings: TAccountSettings): TCloudSettings; overload; static;
		class function CreateFromSettings(const PluginSettings: TPluginSettings; const AccSettings: TAccountSettings; const AEndpoints: TCloudEndpoints): TCloudSettings; overload; static;
	end;

implementation

class function TCloudSettings.CreateFromSettings(const PluginSettings: TPluginSettings; const AccSettings: TAccountSettings): TCloudSettings;
begin
	Result := CreateFromSettings(PluginSettings, AccSettings, TCloudEndpoints.CreateDefaults);
end;

class function TCloudSettings.CreateFromSettings(const PluginSettings: TPluginSettings; const AccSettings: TAccountSettings; const AEndpoints: TCloudEndpoints): TCloudSettings;
begin
	Result := Default (TCloudSettings);
	Result.ConnectionSettings := PluginSettings.ConnectionSettings;
	Result.AccountSettings := AccSettings;
	Result.PrecalculateHash := PluginSettings.PrecalculateHash;
	Result.ForcePrecalculateSize := PluginSettings.ForcePrecalculateSize;
	Result.CheckCRC := PluginSettings.CheckCRC;
	Result.HashCalculatorStrategy := PluginSettings.HashCalculatorStrategy;
	Result.CloudMaxFileSize := AccSettings.CloudMaxFileSize;
	Result.OperationErrorMode := PluginSettings.OperationErrorMode;
	Result.RetryAttempts := PluginSettings.RetryAttempts;
	Result.AttemptWait := PluginSettings.AttemptWait;
	Result.Endpoints := AEndpoints;
	{CryptFilesPassword is set at runtime after password retrieval by FileEncryptionResolver}
end;

end.

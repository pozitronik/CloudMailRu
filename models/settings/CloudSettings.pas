unit CloudSettings;

interface

uses
	AccountSettings,
	ConnectionSettings;

type
	{Прототипирую сюда все параметры, которые требуются классом облака}
	TCloudSettings = record
		{Параметры, наследуемые от глобальных настроек}
		ConnectionSettings: TConnectionSettings;
		AccountSettings: TAccountSettings;

		PrecalculateHash: boolean;
		ForcePrecalculateSize: int64;
		CheckCRC: boolean;
		CloudMaxFileSize: int64;
		OperationErrorMode: integer;
		RetryAttempts: integer;
		AttemptWait: integer;

		CryptFilesPassword: WideString;

	end;

implementation

end.

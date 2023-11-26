unit CloudSettings;

interface

uses
	AccountSettings,
	ConnectionSettings;

type
	{Прототипирую сюда все параметры, которые требуются классом облака}
	TCloudSettings = record
		{Параметры конкретного аккаунта}
		AccountSettings: TAccountSettings;
		{Параметры, наследуемые от глобальных настроек}
		ConnectionSettings: TConnectionSettings;
		PrecalculateHash: boolean;
		ForcePrecalculateSize: int64;
		CheckCRC: boolean;
		CloudMaxFileSize: int64;
		OperationErrorMode: integer;
		RetryAttempts: integer;
		AttemptWait: integer;
	end;

implementation

end.

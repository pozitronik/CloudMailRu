unit CloudSettings;

interface

uses
	NewAccountSettings,
	ConnectionSettings;

type
	{Прототипирую сюда все параметры, которые требуются классом облака}
	TCloudSettings = record
		{Параметры, наследуемые от глобальных настроек}
		ConnectionSettings: TConnectionSettings;
		PrecalculateHash: boolean;
		ForcePrecalculateSize: int64;
		CheckCRC: boolean;
		CloudMaxFileSize: int64;
		OperationErrorMode: integer;
		RetryAttempts: integer;
		AttemptWait: integer;

		{Account settings}
		Email: WideString;
		Password: WideString;
		UseTCPasswordManager: boolean;
		TwostepAuth: boolean;
		UnlimitedFilesize: boolean;
		SplitLargeFiles: boolean;
		PublicAccount: boolean;
		PublicUrl: WideString;
		Description: WideString;
		EncryptFilesMode: integer;
		EncryptFilenames: boolean;
		ShardOverride: WideString;
		UploadUrlOverride: WideString;
		CryptedGUIDFiles: WideString;
		CryptFilesPassword: WideString;

	end;

implementation

end.

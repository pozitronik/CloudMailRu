unit AbstractAccountSettings;

{This base class contains and handles only the set of abstract parameters variables}
interface

type
	TAbstractAccountSettings = record
		Email: WideString;
		Password: WideString;
		UseTCPasswordManager: boolean;
		TwostepAuth: boolean;
		UnlimitedFileSize: boolean;
		SplitLargeFiles: boolean;
		PublicAccount: boolean;
		PublicUrl: WideString;
		Description: WideString;
		EncryptFilesMode: integer;
		EncryptFileNames: boolean;
		ShardOverride: WideString; //hidden option, allows to override working shard for account
		UploadUrlOverride: WideString; //hidden option, alows to override upload server for account
		CryptedGUIDFiles: WideString; //Шифрованная строка для проверки пароля шифрования
	end;

implementation

end.

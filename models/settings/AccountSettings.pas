unit AccountSettings;

interface

type
	{Account-related options set}
	TAccountSettings = record
		name, email, password: WideString;
		use_tc_password_manager, twostep_auth: boolean;
		user, domain: WideString; //parsed values from email
		unlimited_filesize: boolean;
		split_large_files: boolean;
		public_account: boolean;
		public_url: WideString;
		description: WideString;
		encrypt_files_mode: integer;
		encrypt_filenames: boolean;
		shard_override: WideString; //hidden option, allows to override working shard for account
		upload_url_override: WideString; //hidden option, alows to override upload server for account
		self_ini_path: WideString; //runtime parameter, contains path to ini file, used for various manipulations
		crypt_files_password: WideString; //runtime parameter
		CryptedGUID_files: WideString; //Шифрованная строка для проверки пароля шифрования
	end;

implementation

end.

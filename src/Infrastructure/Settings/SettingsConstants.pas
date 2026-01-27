unit SettingsConstants;

interface

const
{$IFDEF WIN64}
	PlatformX = 'x64';
{$ENDIF}
{$IFDEF WIN32}
	PlatformX = 'x32';
{$ENDIF}
	ProxyNone = 0;
	ProxySocks5 = 1;
	ProxySocks4 = 2;
	ProxyHTTP = 3;

	SocksProxyTypes = [ProxySocks5, ProxySocks4];

	CLOUD_MAX_FILESIZE_DEFAULT = 2147483392; //$80000000-256
	CLOUD_PRECALCULATE_LIMIT_DEFAULT = 20; //issue #231

	ChunkOverwrite = 0;
	ChunkOverwriteIgnore = 1;
	ChunkOverwriteAbort = 2;

	DeleteFailOnUploadAsk = 0;
	DeleteFailOnUploadIgnore = 1;
	DeleteFailOnUploadAbort = 2;
	DeleteFailOnUploadDeleteIgnore = 3;
	DeleteFailOnUploadDeleteAbort = 4;

	OverwriteLocalModeAsk = 0; //default
	OverwriteLocalModeIgnore = 1;
	OverwriteLocalModeOverwrite = 2;

	CopyBetweenAccountsModeDisabled = 0;
	CopyBetweenAccountsModeViaHash = 1; //default
	CopyBetweenAccountsModeViaPublicLink = 2; //old mode

	OperationErrorModeAsk = 0;
	OperationErrorModeIgnore = 1;
	OperationErrorModeAbort = 2;
	OperationErrorModeRetry = 3;

	IconsModeDisabled = 0;
	IconsModeInternal = 1;
	IconsModeInternalOverlay = 2;
	IconsModeExternal = 3;
	IconsModeExternalOverlay = 4;

	INI_DIR_PLUGIN = 0; //use ini files only from the plugin dir
	INI_DIR_APPDATA = 1; //use ini files only from %AppData%
	INI_DIR_AUTO = 2; // plugin dir, if writeable, else %AppData%

	EncryptModeNone = 0; //Без шифрования
	EncryptModeAlways = 1; //С прозрачным шифрованием
	EncryptModeAskOnce = 2; //С прозрачным шифрованием, без хранения пароля
	//EncryptModeAskAlways = 3; //не буду поддерживать без необходимости

	{Hash calculator strategy selection - allows choosing between different SHA1 implementations}
	HashStrategyAuto = 0; {Auto-select best available (BCrypt > OpenSSL > Delphi)}
	HashStrategyDelphi = 1; {Use Delphi's System.Hash.THashSHA1}
	HashStrategyBCrypt = 2; {Use Windows BCrypt/CNG API (hardware-accelerated)}
	HashStrategyOpenSSL = 3; {Use OpenSSL EVP functions (already loaded for HTTPS)}

	DOT = '.'; {Just a dot, it used to trail files extensions in some situations}

	TrashPostfix = '.trash';
	SharedPostfix = '.shared';
	InvitesPostfix = '.invites';

	StreamingPrefix = 'Streaming:';

	APPDATA_DIR_NAME = 'MailRuCloud'; //the name of %AppData% subdirectory to use for config search
	PLUGIN_CONFIG_FILE_NAME = 'MailRuCloud.global.ini'; //default filenames should be changes to avoid confusions (current names are bad design)
	ACCOUNTS_CONFIG_FILE_NAME = 'MailRuCloud.ini';

	{User-Agent handling:
		- Browser-like UA (DEFAULT_USERAGENT) is used for most API calls (cloud.mail.ru/api/v2/*)
		- OAuth download endpoint (cloclo*.datacloudmail.ru/oauth-get/*) BLOCKS browser-like UAs,
		  requires 'cloud-win' UA - see CloudFileDownloader.DownloadRegular for override
		- OAuth upload endpoint (oauth-upload/*) works with any UA
		Configurable UA exists for edge cases; don't remove without testing all endpoints.}
	DEFAULT_USERAGENT = 'Mozilla/5.0 (Windows NT 10.0; Win64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.150 Safari/537.36/TCWFX(' + PlatformX + ')';

type
	TIniDirTypes = INI_DIR_PLUGIN .. INI_DIR_AUTO;

implementation

end.

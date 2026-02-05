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

	CLOUD_MAX_FILESIZE_DEFAULT = 2147483392; { $80000000-256 }
	CLOUD_PRECALCULATE_LIMIT_DEFAULT = 20; {issue #231}
	DEFAULT_SOCKET_TIMEOUT = 30000; {30 seconds; 0 = unlimited}
	DEFAULT_SPEED_LIMIT = 0; {0 = unlimited; positive value = bits per second}

	ChunkOverwrite = 0;
	ChunkOverwriteIgnore = 1;
	ChunkOverwriteAbort = 2;

	DeleteFailOnUploadAsk = 0;
	DeleteFailOnUploadIgnore = 1;
	DeleteFailOnUploadAbort = 2;
	DeleteFailOnUploadDeleteIgnore = 3;
	DeleteFailOnUploadDeleteAbort = 4;

	OverwriteLocalModeAsk = 0; {default}
	OverwriteLocalModeIgnore = 1;
	OverwriteLocalModeOverwrite = 2;

	CopyBetweenAccountsModeDisabled = 0;
	CopyBetweenAccountsModeViaHash = 1; {default}
	CopyBetweenAccountsModeViaPublicLink = 2; {old mode}

	OperationErrorModeAsk = 0;
	OperationErrorModeIgnore = 1;
	OperationErrorModeAbort = 2;
	OperationErrorModeRetry = 3;

	IconsModeDisabled = 0;
	IconsModeInternal = 1;
	IconsModeInternalOverlay = 2;
	IconsModeExternal = 3;
	IconsModeExternalOverlay = 4;

	INI_DIR_PLUGIN = 0; {Use INI files only from the plugin dir}
	INI_DIR_APPDATA = 1; {Use INI files only from %AppData%}
	INI_DIR_AUTO = 2; {Plugin dir if writeable, else %AppData%}

	EncryptModeNone = 0; {No encryption}
	EncryptModeAlways = 1; {Transparent encryption}
	EncryptModeAskOnce = 2; {Transparent encryption, password not stored}
	{EncryptModeAskAlways = 3; not supporting unless needed}

	{Hash calculator strategy selection - allows choosing between different SHA1 implementations}
	HashStrategyAuto = 0; {Auto-select best available (BCrypt > OpenSSL > Delphi)}
	HashStrategyDelphi = 1; {Use Delphi's System.Hash.THashSHA1}
	HashStrategyBCrypt = 2; {Use Windows BCrypt/CNG API (hardware-accelerated)}
	HashStrategyOpenSSL = 3; {Use OpenSSL EVP functions (already loaded for HTTPS)}

	{SSL backend selection - allows switching between different SSL/TLS implementations}
	SSLBackendAuto = 0; {Auto-detect best available (IndySec if OpenSSL 3.x found, else standard Indy)}
	SSLBackendIndy = 1; {Standard Indy SSL (OpenSSL 1.0.x/1.1.x)}
	SSLBackendIndySec = 2; {IndySecOpenSSL (OpenSSL 1.1.x/3.x with TLS 1.3 support)}

	{Extensions for which the plugin requests cloud thumbnail previews.
		Comma-separated, with leading dots. Configurable via INI ThumbnailExtensions key.}
	DEFAULT_THUMBNAIL_EXTENSIONS = '.jpg,.jpeg,.png,.gif,.bmp,.heic,.heif,.webp,.tiff,.tif,.mp4,.avi,.mov,.mkv,.wmv,.flv,.3gp,.m4v,.mpg,.mpeg';

	DOT = '.'; {Just a dot, it used to trail files extensions in some situations}

	TrashPostfix = '.trash';
	SharedPostfix = '.shared';
	InvitesPostfix = '.invites';

	StreamingPrefix = 'Streaming:';

	APPDATA_DIR_NAME = 'MailRuCloud'; {The name of %AppData% subdirectory to use for config search}
	PLUGIN_CONFIG_FILE_NAME = 'MailRuCloud.global.ini'; {Default filenames should be changed to avoid confusions (current names are bad design)}
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

unit CMLStrings;

interface

const
	ASK_CONTINUE = 'Continue operation?';

	ERR_ACCOUNT_HAS_INVALID_SYMBOL = 'File name must contain only valid symbols';
	ERR_INSUFFICIENT_STORAGE = 'Insufficient Storage';
	ERR_NAME_TOO_LONG = 'Name too long';
	ERR_COPY_SAME_DIR = 'Copying to the same dir is not supported';
	ERR_WRONG_ENCRYPT_PASSWORD = 'Wrong encryption password, encryption support disabled';
	ERR_CLOUD_ERROR_EXISTS = 'Объект с таким названием уже существует. Попробуйте другое название.';
	ERR_CLOUD_ERROR_REQUIRED = 'Название папки не может быть пустым.';
	ERR_CLOUD_ERROR_INVALID = 'Неправильное название папки. В названии папок нельзя использовать символы «" * / : < > ?  \\ |».';
	ERR_CLOUD_ERROR_READONLY = 'Невозможно создать. Доступ только для просмотра.';
	ERR_CLOUD_ERROR_NAME_LENGTH_EXCEEDED = 'Превышена длина имени папки.';
	ERR_CLOUD_ERROR_OVERQUOTA = 'Невозможно скопировать, в вашем Облаке недостаточно места.';
	ERR_CLOUD_ERROR_NOT_EXISTS = 'Копируемая ссылка не существует.';
	ERR_CLOUD_ERROR_OWN = 'Невозможно клонировать собственную ссылку.';
	ERR_CLOUD_ERROR_NAME_TOO_LONG = 'Превышена длина имени файла.';
	ERR_CLOUD_ERROR_VIRUS_SCAN_FAIL = 'Файл заражен вирусом.';
	ERR_CLOUD_ERROR_OWNER = 'Нельзя использовать собственный email.';
	ERR_CLOUD_ERROR_FAHRENHEIT = 'Невозможно создать ссылку. Публикация контента заблокирована по требованию правообладателя или уполномоченного государственного ведомства.';
	ERR_CLOUD_ERROR_BAD_REQUEST = 'Ошибка запроса к серверу.';
	ERR_CLOUD_ERROR_TREES_CONFLICT = 'Нельзя сделать папку общей, если она содержит другие общие папки или находится в общей папке.';
	ERR_CLOUD_ERROR_UNPROCESSABLE_ENTRY = 'Нельзя открыть доступ к файлу.';
	ERR_CLOUD_ERROR_USER_LIMIT_EXCEEDED = 'Невозможно добавить пользователя. Вы можете иметь не более 200 пользователей в одной общей папке.';
	ERR_CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED = 'Невозможно добавить пользователя. Вы можете создать не более 50 общих папок.';
	ERR_CLOUD_ERROR_NOT_ACCEPTABLE = 'Нельзя добавить этого пользователя';
	ERR_CLOUD_ERROR_UNKNOWN = 'Неизвестная ошибка (%d)';
	ERR_TOKEN_UPDATE = 'Token update error!';
	ERR_GET_PUBLIC_SHARE = 'Can''t get public share download share';
	ERR_TWOSTEP_AUTH = 'error: twostep auth failed';
	ERR_SECURITY_KEY = 'error: security key not provided';
	ERR_PARSE_AUTH_DATA = 'error: parsing authorization data';
	ERR_GET_FIRST_STEP_AUTH_TOKEN = 'error: getting first step auth token for %s';
	ERR_PARSING_AUTH_TOKEN = 'error: parsing auth token for %s';
	ERR_GET_AUTH_TOKEN = 'error: getting auth token for %s';
	ERR_GET_USER_SPACE = 'error: getting user space information for %s';
	ERR_UPLOAD_INFO = 'error: uploading to cloud: %s with message: %s';
	ERR_UPLOAD = 'Upload error';
	ERR_PARTIAL_UPLOAD_ASK = 'Partial upload error, code: %d' + sLineBreak + 'partname: %s' + sLineBreak + ASK_CONTINUE;
	ERR_PARTIAL_UPLOAD_IGNORE = 'Partial upload error, code: %d, ignored';
	ERR_PARTIAL_UPLOAD_ABORT = 'Partial upload error, code: %d, aborted';
	ERR_PARTIAL_UPLOAD_RETRY = 'Partial upload error, code: %d, retry attempt %d %s';
	ERR_PARTIAL_UPLOAD_RETRY_EXCEED = 'Partial upload error, code: %d, retry attempt limit exceed, aborted';

	PREFIX_ERR_CLOUD_INIT = 'Cloud initialization error: ';
	PREFIX_ERR_FILE_UPLOADING = 'File uploading error: ';
	PREFIX_ERR_FILE_PUBLISH = 'File publish error: ';
	PREFIX_ERR_FILE_COPY = 'File copy error: ';
	PREFIX_ERR_DELETE_FILE = 'Delete file error: ';
	PREFIX_ERR_SHARED_LINKS_LISTING = 'Shared links listing error: ';
	PREFIX_ERR_INCOMING_REQUESTS_LISTING = 'Incoming requests listing error: ';
	PREFIX_ERR_TRASH_LISTING = 'Trashbin listing error: ';
	PREFIX_ERR_TRASH_CLEAN = 'Trashbin clearing error: ';
	PREFIX_ERR_DIR_LISTING = 'Directory listing error: ';
	PREFIX_ERR_PATH_NOT_EXISTS = 'Path not exists: ';
	PREFIX_ERR_SHARD_RECEIVE = 'Shard receive error: ';
	PREFIX_ERR_REMOVE_TEMP_PUBLIC_LINK = 'Can''t remove temporary public link on ';
	PREFIX_ERR_GET_USER_SPACE = 'User space receiving error: ';
	PREFIX_ERR_OAUTH = 'OAuth error: %s (%s).';
	PREFIX_ERR_FILE_MOVE = 'File move error: ';
	PREFIX_ERR_INVITE_MEMBER = 'Invite member error: ';
	PREFIX_ERR_FILE_RESTORE = 'File restore error: ';
	PREFIX_ERR_FOLDER_MOUNT = 'Folder mount error: ';
	PREFIX_ERR_FOLDER_UNMOUNT = 'Folder unmount error: ';
	PREFIX_ERR_INVITE_REJECT = 'Invite rejection error: ';
	PREFIX_ERR_DELETE_DIR = 'Directory deletion error: ';
	PREFIX_ERR_FILE_RENAME = 'File renaming error: ';
	PREFIX_ERR_FILE_STATUS = 'File status error: ';

	PREFIX_STATUS = 'Status: ';
	PREFIX_REDIRECTION_LIMIT = 'Redirection limit reached when trying to download ';
	PREFIX_SHARD_RECEIVED = 'Shard received: %s, type: %s';

	CALCULATING_HASH = 'Calculating cloud hash';
	CREATE_DIRECTORY = 'Create directory';
	DELETE_FILE = 'Delete file';
	SHARED_LINKS_LISTING = 'Shared links listing';
	INCOMING_LINKS_LISTING = 'Incoming links listing';
	TRASH_LISTING = 'Trashbin listing';
	DIR_LISTING = 'Directory listing';
	UNDEFINED_DOWNLOAD_SHARD = 'Current download shard is undefined, trying to get one';
	UNDEFINED_UPLOAD_SHARD = 'Current upload shard is undefined, trying to get one';
	REDIRECTION_LIMIT = 'Redirection limit';
	TRY_ANOTHER_SHARD = 'Try with another shard?';
	SHARD_OVERRIDEN = 'Shard overriden via config!';
	UPLOAD_URL_OVERRIDEN = 'Upload url overriden via config!';
	TOKEN_UPDATED = 'Token updated';
	LOGIN_IN_PROGRESS = 'Login to account...';
	LOGIN_TO = 'Login to %s';
	//	DOMAIN = 'Domain';
	//	LOGIN_STR = 'Login';
	//	PASSWORD = 'Password';
	REQUESTING_FIRST_STEP_AUTH_TOKEN = 'Requesting first step auth token for %s';
	SECOND_STEP_AUTH = 'Performing second step auth...';
	PARSING_AUTH_DATA = 'Parsing authorization data...';
	ASK_AUTH_APP_CODE = 'Enter code from authentication app.';
	SMS_TIMEOUT = 'SMS timeout to %s (%d sec).';
	ASK_SENT_CODE = 'Enter code sent to %s.';
	AWAIT_SECURITY_KEY = 'Awaiting for security key...';
	ASK_AUTH_KEY = 'Enter auth key';
	CONNECTED_TO = 'Connected to %s';
	REQUESTING_AUTH_TOKEN = 'Requesting auth token for %s';
	PARSING_TOKEN_DATA = 'Parsing token data...';
	URL_OPEN = 'Open %s';
	USER_SPACE_INFO = 'Total space: %s, used: %s, free: %s.%s';
	PARTIAL_UPLOAD_INFO = 'Partial upload of %s part %d of %d => %s';
	PARTIAL_UPLOAD_ABORTED = 'Partial upload aborted';
	CHUNK_OWERWRITE = 'Chunk %s already exists, overwriting';
	CHUNK_SKIP = 'Chunk %s already exists, skipping';
	CHUNK_ABORT = 'Chunk %s already exists, aborting';
	SPLIT_LARGE_FILE = 'File size > %d bytes, file will be splitted';
	SPLIT_LARGE_FILE_IGNORE = 'File size > %d bytes, ignored';
	DELETE_DIR = 'Remove directory';
	CALC_HASH = 'Calculating cloud hash';

	WARN_QUOTA_EXAUSTED = ' Warning: space quota exhausted!';

	UNKNOWN_ITEM = '...';
	EMPTY_STR = '';

implementation

end.

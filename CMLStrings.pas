﻿unit CMLStrings;

interface

const
	ASK_AUTH_APP_CODE = 'Enter code from authentication app.';
	ASK_AUTH_KEY = 'Enter auth key';
	ASK_CONTINUE = 'Continue operation?';
	ASK_SENT_CODE = 'Enter code sent to %s.';
	AWAIT_SECURITY_KEY = 'Awaiting for security key...';
	CALCULATING_HASH = 'Calculating cloud hash';
	CHUNK_ABORT = 'Chunk %s already exists, aborting';
	CHUNK_OVERWRITE = 'Chunk %s already exists, overwriting';
	CHUNK_SKIP = 'Chunk %s already exists, skipping';
	CONNECTED_TO = 'Connected to %s';
	CREATE_DIRECTORY = 'Create directory';
	DELETE_DIR = 'Remove directory';
	DELETE_FILE = 'Delete file';
	DIR_LISTING = 'Directory listing';
	EMPTY_STR = '';
	ERR_ACCOUNT_HAS_INVALID_SYMBOL = 'File name must contain only valid symbols';
	ERR_CLOUD_ERROR_BAD_REQUEST = 'Request to the server failed.';
	ERR_CLOUD_ERROR_EXISTS = 'An object with this name already exists. Please try another name.';
	ERR_CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED = 'Unable to add user. You can create a maximum of 50 shared folders.';
	ERR_CLOUD_ERROR_FAHRENHEIT = 'Unable to create a link. Content publication is blocked upon the demand of the rights holder or an authorized state authority.';
	ERR_CLOUD_ERROR_INVALID = 'Invalid folder name. Folder names cannot contain characters such as "\" * / : < > ? |".';
	ERR_CLOUD_ERROR_NAME_LENGTH_EXCEEDED = 'Folder name length exceeded.';
	ERR_CLOUD_ERROR_NAME_TOO_LONG = 'File name length exceeded.';
	ERR_CLOUD_ERROR_NOT_ACCEPTABLE = 'Cannot add this user.';
	ERR_CLOUD_ERROR_NOT_EXISTS = 'The copied link does not exist.';
	ERR_CLOUD_ERROR_OVERQUOTA = 'Unable to copy; there''s insufficient space in your Cloud.';
	ERR_CLOUD_ERROR_OWN = 'Unable to clone your own link.';
	ERR_CLOUD_ERROR_OWNER = 'Cannot use your own email.';
	ERR_CLOUD_ERROR_READONLY = 'Unable to create. Access is read-only.';
	ERR_CLOUD_ERROR_REQUIRED = 'Folder name cannot be empty.';
	ERR_CLOUD_ERROR_TREES_CONFLICT = 'Cannot make the folder shared if it contains other shared folders or resides in a shared folder.';
	ERR_CLOUD_ERROR_UNKNOWN = 'Unknown error (%d).';
	ERR_CLOUD_ERROR_UNPROCESSABLE_ENTRY = 'Cannot grant access to the file.';
	ERR_CLOUD_ERROR_USER_LIMIT_EXCEEDED = 'Unable to add user. You can have a maximum of 200 users in one shared folder.';
	ERR_CLOUD_ERROR_VIRUS_SCAN_FAIL = 'File is infected with a virus.';
	ERR_COPY_SAME_DIR = 'Copying to the same dir is not supported';
	ERR_GET_AUTH_TOKEN = 'error: getting auth token for %s';
	ERR_GET_FIRST_STEP_AUTH_TOKEN = 'error: getting first step auth token for %s';
	ERR_GET_PUBLIC_SHARE = 'Can''t get public share download share';
	ERR_GET_USER_SPACE = 'error: getting user space information for %s';
	ERR_INSUFFICIENT_STORAGE = 'Insufficient Storage';
	ERR_NAME_TOO_LONG = 'Name too long';
	ERR_PARSE_AUTH_DATA = 'error: parsing authorization data';
	ERR_PARSING_AUTH_TOKEN = 'error: parsing auth token for %s';
	ERR_PARTIAL_UPLOAD_ABORT = 'Partial upload error, code: %d, aborted';
	ERR_PARTIAL_UPLOAD_ASK = 'Partial upload error, code: %d' + sLineBreak + 'part name: %s' + sLineBreak + ASK_CONTINUE;
	ERR_PARTIAL_UPLOAD_IGNORE = 'Partial upload error, code: %d, ignored';
	ERR_PARTIAL_UPLOAD_RETRY = 'Partial upload error, code: %d, retry attempt %d %s';
	ERR_PARTIAL_UPLOAD_RETRY_EXCEED = 'Partial upload error, code: %d, retry attempt limit exceed, aborted';
	ERR_SECURITY_KEY = 'error: security key not provided';
	ERR_TOKEN_UPDATE = 'Token update error!';
	ERR_TWOSTEP_AUTH = 'error: two-step auth failed';
	ERR_UPLOAD = 'Upload error';
	ERR_UPLOAD_INFO = 'error: uploading to cloud: %s with message: %s';
	ERR_WRONG_ENCRYPT_PASSWORD = 'Incorrect encryption password, encryption support disabled';
	FILE_FOUND_BY_HASH = 'File "%s" found by hash';
	INCOMING_LINKS_LISTING = 'Incoming links listing';
	LOGIN_IN_PROGRESS = 'Login to account...';
	LOGIN_TO = 'Login to %s';
	PARSING_AUTH_DATA = 'Parsing authorization data...';
	PARSING_TOKEN_DATA = 'Parsing token data...';
	PARTIAL_UPLOAD_ABORTED = 'Partial upload aborted';
	PARTIAL_UPLOAD_INFO = 'Partial upload of %s part %d of %d => %s';
	PREFIX_ERR_CLOUD_INIT = 'Cloud initialization error: ';
	PREFIX_ERR_DELETE_DIR = 'Directory deletion error: ';
	PREFIX_ERR_DELETE_FILE = 'Delete file error: ';
	PREFIX_ERR_DIR_LISTING = 'Directory listing error: ';
	PREFIX_ERR_FILE_COPY = 'File copy error: ';
	PREFIX_ERR_FILE_MOVE = 'File move error: ';
	PREFIX_ERR_FILE_PUBLISH = 'File publish error: ';
	PREFIX_ERR_FILE_RENAME = 'File renaming error: ';
	PREFIX_ERR_FILE_RESTORE = 'File restore error: ';
	PREFIX_ERR_FILE_STATUS = 'File status error: ';
	PREFIX_ERR_FILE_UPLOADING = 'File uploading error: ';
	PREFIX_ERR_FOLDER_MOUNT = 'Folder mount error: ';
	PREFIX_ERR_FOLDER_UNMOUNT = 'Folder unmount error: ';
	PREFIX_ERR_GET_USER_SPACE = 'User space receiving error: ';
	PREFIX_ERR_INCOMING_REQUESTS_LISTING = 'Incoming requests listing error: ';
	PREFIX_ERR_INVITE_MEMBER = 'Invite member error: ';
	PREFIX_ERR_INVITE_REJECT = 'Invite rejection error: ';
	PREFIX_ERR_OAUTH = 'OAuth error: %s (%s).';
	PREFIX_ERR_PATH_NOT_EXISTS = 'Path not exists: ';
	PREFIX_ERR_REMOVE_TEMP_PUBLIC_LINK = 'Can''t remove temporary public link on ';
	PREFIX_ERR_SHARD_RECEIVE = 'Shard receive error: ';
	PREFIX_ERR_SHARED_LINKS_LISTING = 'Shared links listing error: ';
	PREFIX_ERR_TRASH_CLEAN = 'Trash bin clear error: ';
	PREFIX_ERR_TRASH_LISTING = 'Trash bin listing error: ';
	PREFIX_REDIRECTION_LIMIT = 'Redirection limit reached when trying to download ';
	PREFIX_SHARD_RECEIVED = 'Shard received: %s, type: %s';
	PREFIX_STATUS = 'Status: ';
	REDIRECTION_LIMIT = 'Redirection limit';
	REQUESTING_AUTH_TOKEN = 'Requesting auth token for %s';
	REQUESTING_FIRST_STEP_AUTH_TOKEN = 'Requesting first step auth token for %s';
	SECOND_STEP_AUTH = 'Performing second step auth...';
	SHARD_OVERRIDDEN = 'Shard is overridden via config!';
	SHARED_LINKS_LISTING = 'Shared links listing';
	SMS_TIMEOUT = 'SMS timeout to %s (%d sec).';
	SPLIT_LARGE_FILE = 'File size > %d bytes, file will be split';
	SPLIT_LARGE_FILE_IGNORE = 'File size > %d bytes, ignored';
	TOKEN_UPDATED = 'Token updated';
	TRASH_LISTING = 'Trash bin listing';
	TRY_ANOTHER_SHARD = 'Try with another shard?';
	UNDEFINED_DOWNLOAD_SHARD = 'Current download shard is undefined, trying to get one';
	UNDEFINED_UPLOAD_SHARD = 'Current upload shard is undefined, trying to get one';
	UNKNOWN_ITEM = '...';
	UPLOAD_URL_OVERRIDDEN = 'Upload url is overridden via config!';
	URL_OPEN = 'Open %s';
	USER_SPACE_INFO = 'Total space: %s, used: %s, free: %s.%s';
	WARN_QUOTA_EXHAUSTED = ' Warning: space quota exhausted!';

	METHOD_STR_RECEIVE = 'получении данных с';
	METHOD_STR_POST = 'отправке данных на';
	METHOD_STR_OPTIONS = 'запросе параметров с';
	CSRF_UPDATE_REQUIRED = 'Требуется обновить CSRF-токен при %s %s';
	ERR_HTTP_GENERAL = '%s ошибка с сообщением: %s при %s %s, ответ сервера: %s';
	ERR_SOCKET_GENERAL = '%s ошибка сети: %s при %s %s';
	ERR_OTHER_GENERAL = '%s ошибка с сообщением: %s при %s %s';
	ERR_NO_MASTER_PASSWORD = 'No master password entered yet';
	ERR_NO_PASSWORDS_STORED = 'CryptProc returns an error: No password found in the password store';
	ERR_DECRYPT_FAILED = 'CryptProc returns an error: Decrypt failed';
	PASSWORD_SAVED = '%s: the password saved in the TC password manager';
	ERR_ENCRYPT_FAILED = '%s: CryptProc returns an error: Encrypt failed';
	ERR_WRITE_FAILED = '%s: password is not saved: Can''t write the password to the password store';
	ERR_WRITE_NO_MASTER_PASSWORD = '%s: password is not saved: ' + ERR_NO_MASTER_PASSWORD;
	PREFIX_ASK_PASSWORD = 'Enter account password:';
	ASK_PASSWORD = '%s password';
	ASK_PROXY_PASSWORD = 'User %s proxy password';
	PREFIX_ASK_PROXY_PASSWORD = 'Enter proxy password:';
	ASK_ENCRYPTION_PASSWORD =  '%s encryption password';
	PREFIX_ASK_ENCRYPTION_PASSWORD = 'Enter encryption password for current session:';
	VERB_UPDATE = 'Update';
	VERB_SET = 'Set';
	PREFIX_ASK_NEW_PASSWORD = 'New password:';


implementation

end.

﻿unit CMLStrings;

interface

(*NOTE: Those can be converted to resourcestrings  when i18n applied*)
const
	ACCOUNT_TRASH = '%s trash';
	ASK_AUTH_APP_CODE = 'Enter code from authentication app.';
	ASK_AUTH_KEY = 'Enter auth key';
	ASK_CONTINUE = 'Continue operation?';
	ASK_ENCRYPTION_PASSWORD = '%s encryption password';
	ASK_PASSWORD = '%s password';
	ASK_PROXY_PASSWORD = 'User %s proxy password';
	ASK_SENT_CODE = 'Enter code sent to %s.';
	AWAIT_SECURITY_KEY = 'Awaiting for security key...';
	CALCULATING_HASH = 'Calculating cloud hash';
	CANCEL = 'Cancel';
	CHUNK_ABORT = 'Chunk %s already exists, aborting';
	CHUNK_OVERWRITE = 'Chunk %s already exists, overwriting';
	CHUNK_SKIP = 'Chunk %s already exists, skipping';
	CLONE_FILE_RETRY = 'File cloning error: %s, retry attempt %d of %d';
	CONNECTED_TO = 'Connected to %s';
	CONTINUE_ASK = 'Continue operation?';
	CREATE_DIRECTORY = 'Create directory';
	CSRF_UPDATE_REQUIRED = 'CSRF token update required during %s %s';
	DELETED_ITEM = 'Deleted item: %s';
	DELETE_DIR = 'Remove directory';
	DELETE_FILE = 'Delete file';
	DESCRIPTION_FROM = 'Description from %s';
	DIR_LISTING = 'Directory listing';
	DONE = 'Done';
	DOWNLOAD_FILE_RETRY = 'Error downloading file %s, retry attempt %d of %d';
	EMPTY = 'Empty';
	EMPTY_STR = '';
	ERR_ACCOUNT_HAS_INVALID_SYMBOL = 'File name must contain only valid symbols';
	ERR_CLONE_BY_HASH = 'Error clone by hash: %s, parameter: %s';
	ERR_CLONE_FILE_ASK = 'File cloning error: %s' + sLineBreak + CONTINUE_ASK;
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
	ERR_CONFIRMATION = 'Confirmation error';
	ERR_COPY_SAME_DIR_NOT_SUPPORTED = 'Copying to the same dir is not supported';
	ERR_DECRYPT_FAILED = 'CryptProc returns an error: Decrypt failed';
	ERR_DELETE = 'Can''t delete %s';
	ERR_DELETE_FILE = 'File deletion error';
	ERR_DELETE_FILE_ABORT = 'Can''t delete file %s, aborted';
	ERR_DELETE_FILE_ASK = 'Can''t delete file %s. Continue operation?';
	ERR_DELETE_FILE_DELETE = 'Read only file %s deleted';
	ERR_DELETE_FILE_IGNORE = 'Can''t delete file %s, ignore';
	ERR_DIRECT_COPY_SUPPORT = 'Direct copying from public accounts is not supported';
	ERR_DIRECT_OPERATIONS_DISABLED = 'Direct operations between accounts are disabled';
	ERR_DIRECT_OPERATIONS_NOT_SUPPORTED = 'Direct operations from public accounts are not supported';
	ERR_DOWNLOAD = 'Download error';
	ERR_DOWNLOAD_FILE_ASK = 'Error downloading file' + sLineBreak + '%s' + sLineBreak + CONTINUE_ASK;
	ERR_ENCRYPT_FAILED = '%s: CryptProc returns an error: Encrypt failed';
	ERR_FILE_NOT_EXISTS = 'File not exists';
	ERR_GET_AUTH_TOKEN = 'error: getting auth token for %s';
	ERR_GET_FIRST_STEP_AUTH_TOKEN = 'error: getting first step auth token for %s';
	ERR_GET_PUBLIC_SHARE = 'Can''t get public share download share';
	ERR_GET_TEMP_PUBLIC_LINK = 'Can''t get temporary public link on %s';
	ERR_GET_USER_SPACE = 'error: getting user space information for %s';
	ERR_HTTP_GENERAL = '%s error with message: %s at %s %s, server response: %s';
	ERR_INI_GENERAL = 'INI file error';
	ERR_INSUFFICIENT_STORAGE = 'Insufficient Storage';
	ERR_INVALID_IDENTIFIER_NAME = 'Invalid identifier name %s';
	ERR_INVALID_SECTION_NAME = 'Invalid section name %s';
	ERR_INVITE_MSG = 'Error while inviting %s to %s folder, see the main log';
	ERR_LINE_HASH = 'Line %d[%s]: %s';
	ERR_LIST_INVITES_MSG = 'Error while retrieving file %s invites list, see the main log';
	ERR_LOAD_CAPTCHA = 'Can''t load captcha image!';
	ERR_NAME_TOO_LONG = 'Name too long';
	ERR_NO_MASTER_PASSWORD = 'No master password entered yet';
	ERR_NO_PASSWORDS_STORED = 'CryptProc returns an error: No password found in the password store';
	ERR_OPERATION = 'Operation error';
	ERR_OTHER_GENERAL = '%s error with message: %s at %s %s';
	ERR_PARSE_AUTH_DATA = 'error: parsing authorization data';
	ERR_PARSING_AUTH_TOKEN = 'error: parsing auth token for %s';
	ERR_PARTIAL_UPLOAD_ABORT = 'Partial upload error, code: %d, aborted';
	ERR_PARTIAL_UPLOAD_ASK = 'Partial upload error, code: %d' + sLineBreak + 'part name: %s' + sLineBreak + ASK_CONTINUE;
	ERR_PARTIAL_UPLOAD_IGNORE = 'Partial upload error, code: %d, ignored';
	ERR_PARTIAL_UPLOAD_RETRY = 'Partial upload error, code: %d, retry attempt %d of %d';
	ERR_PARTIAL_UPLOAD_RETRY_EXCEED = 'Partial upload error, code: %d, retry attempt limit exceed, aborted';
	ERR_PUBLISH_FILE = 'File publishing error';
	ERR_PUBLISH_FILE_ASK = 'File publish error: %s' + sLineBreak + CONTINUE_ASK;
	ERR_PUBLISH_MSG = 'Error while publishing file %s, see the main log';
	ERR_READ_BYTES_FROM = 'Can''t read from %s %d bytes at %d';
	ERR_REGISTRATION = 'Registration error';
	ERR_SECURITY_KEY = 'error: security key not provided';
	ERR_SHARE_FOLDER_MSG = 'Error while share access to %s from %s folder, see the main log';
	ERR_SOCKET_GENERAL = '%s network error: %s at %s %s';
	ERR_TOKEN_UPDATE = 'Token update error!';
	ERR_TWOSTEP_AUTH = 'error: two-step auth failed';
	ERR_UNPUBLISH_FILE = 'File unpublishing error';
	ERR_UNPUBLISH_MSG = 'Error while unpublishing file %s, see the main log';
	ERR_UNSHARE_FOLDER_MSG = 'Error while remove access to %s to %s folder, see the main log';
	ERR_UPLOAD = 'Upload error';
	ERR_UPLOAD_FILE_ASK = 'Error uploading file' + sLineBreak + '%s' + sLineBreak + CONTINUE_ASK;
	ERR_UPLOAD_INFO = 'error: uploading to cloud: %s with message: %s';
	ERR_WHERE_IS_THE_FILE = 'Can''t find the file %s';
	ERR_WRITE_FAILED = '%s: password is not saved: Can''t write the password to the password store';
	ERR_WRITE_NO_MASTER_PASSWORD = '%s: password is not saved: ' + ERR_NO_MASTER_PASSWORD;
	ERR_WRONG_ENCRYPT_PASSWORD = 'Incorrect encryption password, encryption support disabled';
	ERR_WRONG_FORMAT = 'Parameter should be in hash:size:name or hash:size format.';
	ERR_WRONG_HASH_LENGTH = 'Hash length should be exactly 40 symbols.';
	ERR_WRONG_SIZE_FORMAT = 'Size should be in numeric format.';
	FILE_EXISTS_IGNORE = 'Local file %s exists, ignored';
	FILE_EXISTS_OVERWRITE = 'Local file %s exists, and will be overwritten';
	FILE_FOUND_BY_HASH = 'File "%s" found by hash';
	INCOMING_LINKS_LISTING = 'Incoming links listing';
	INVITE_FORM_TITLE = '%s invite: %s';
	LOGIN_IN_PROGRESS = 'Login to account...';
	LOGIN_TO = 'Login to %s';
	METHOD_STR_OPTIONS = 'request parameters from';
	METHOD_STR_POST = 'post data to';
	METHOD_STR_RECEIVE = 'receive data from';
	MOUNTED_AS = 'Mounted as:';
	MULTIPLE_ITEMS = '<Multiple items>';
	MULTIPLE_ITEMS_DELETED = 'Multiple deleted items';
	OK = 'Ok';
	PARSING_AUTH_DATA = 'Parsing authorization data...';
	PARSING_TOKEN_DATA = 'Parsing token data...';
	PARTIAL_UPLOAD_ABORTED = 'Partial upload aborted';
	PARTIAL_UPLOAD_INFO = 'Partial upload of %s part %d of %d => %s';
	PASSWORD_SAVED = '%s: the password saved in the TC password manager';
	PREFIX_ACCESS_CHANGE = 'Change access to %s';
	PREFIX_ASK_ENCRYPTION_PASSWORD = 'Enter encryption password for current session:';
	PREFIX_ASK_NEW_PASSWORD = 'New password:';
	PREFIX_ASK_PASSWORD = 'Enter account password:';
	PREFIX_ASK_PROXY_PASSWORD = 'Enter proxy password:';
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
	PREFIX_ERR_INVITE = 'Folder invite error';
	PREFIX_ERR_INVITES_LISTING = 'Invite listing error';
	PREFIX_ERR_INVITE_MEMBER = 'Invite member error: ';
	PREFIX_ERR_INVITE_REJECT = 'Invite rejection error: ';
	PREFIX_ERR_OAUTH = 'OAuth error: %s (%s).';
	PREFIX_ERR_PATH_NOT_EXISTS = 'Path not exists: ';
	PREFIX_ERR_REMOVE_TEMP_PUBLIC_LINK = 'Can''t remove temporary public link on ';
	PREFIX_ERR_SHARD_RECEIVE = 'Shard receive error: ';
	PREFIX_ERR_SHARED_LINKS_LISTING = 'Shared links listing error: ';
	PREFIX_ERR_SHARE_FOLDER = 'Folder share error';
	PREFIX_ERR_TRASH_CLEAN = 'Trash bin clear error: ';
	PREFIX_ERR_TRASH_LISTING = 'Trash bin listing error: ';
	PREFIX_ERR_UNSHARE_FOLDER = 'Folder unshare error';
	PREFIX_REDIRECTION_LIMIT = 'Redirection limit reached when trying to download ';
	PREFIX_SCAN = 'Scanning %s';
	PREFIX_SHARD_RECEIVED = 'Shard received: %s, type: %s';
	PREFIX_STATUS = 'Status: ';
	PUBLISH_FILE_RETRY = 'File publish error: %s, retry attempt %d of %d';
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
	UNSET_ITEM = '-';
	UPLOAD_FILE_RETRY = 'Error uploading file %s, retry attempt %d of %d';
	UPLOAD_URL_OVERRIDDEN = 'Upload url is overridden via config!';
	URL_OPEN = 'Open %s';
	USER_SPACE_INFO = 'Total space: %s, used: %s, free: %s.%s';
	VERB_SET = 'Set';
	VERB_UPDATE = 'Update';
	WAIT = 'Wait for it...';
	WARN_QUOTA_EXHAUSTED = 'Warning: space quota exhausted!';

implementation

end.

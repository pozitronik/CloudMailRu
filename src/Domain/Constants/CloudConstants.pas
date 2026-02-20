unit CloudConstants;

interface

const
	PUBLIC_ACCESS_URL = 'https://cloud.mail.ru/public/';
	OAUTH_TOKEN_URL = 'https://o2.mail.ru/token';
	OAUTH_DISPATCHER_URL = 'https://dispatcher.cloud.mail.ru';
	THUMB_CLOUD_URL = 'https://thumb.cloud.mail.ru/thumb'; {Thumbnail server - fallback if dispatcher doesn't return shard}

	API_CSRF = 'https://cloud.mail.ru/api/v2/tokens/csrf';
	API_FILE = 'https://cloud.mail.ru/api/v2/file';
	API_FILE_MOVE = 'https://cloud.mail.ru/api/v2/file/move';
	API_FILE_PUBLISH = 'https://cloud.mail.ru/api/v2/file/publish';
	API_FILE_UNPUBLISH = 'https://cloud.mail.ru/api/v2/file/unpublish';
	API_FILE_RENAME = 'https://cloud.mail.ru/api/v2/file/rename';
	API_FILE_ADD = 'https://cloud.mail.ru/api/v2/file/add';
	API_FILE_REMOVE = 'https://cloud.mail.ru/api/v2/file/remove';
	API_FILE_COPY = 'https://cloud.mail.ru/api/v2/file/copy';
	API_FOLDER = 'https://cloud.mail.ru/api/v2/folder?sort={%22type%22%3A%22name%22%2C%22order%22%3A%22asc%22}';
	API_FOLDER_LIMIT = 65535;
	API_FOLDER_ADD = 'https://cloud.mail.ru/api/v2/folder/add';
	API_FOLDER_SHARED_INFO = 'https://cloud.mail.ru/api/v2/folder/shared/info'; {GET}
	API_FOLDER_INVITES = 'https://cloud.mail.ru/api/v2/folder/invites'; {TODO: not used in production code -- investigate if needed for invite listing vs /folder/shared/incoming}
	API_FOLDER_SHARE = 'https://cloud.mail.ru/api/v2/folder/share';
	API_FOLDER_UNSHARE = 'https://cloud.mail.ru/api/v2/folder/unshare';
	API_FOLDER_MOUNT = 'https://cloud.mail.ru/api/v2/folder/mount';
	API_FOLDER_UNMOUNT = 'https://cloud.mail.ru/api/v2/folder/unmount';
	API_FOLDER_SHARED_LINKS = 'https://cloud.mail.ru/api/v2/folder/shared/links';
	API_FOLDER_SHARED_INCOMING = 'https://cloud.mail.ru/api/v2/folder/shared/incoming';
	API_TRASHBIN = 'https://cloud.mail.ru/api/v2/trashbin';
	API_TRASHBIN_RESTORE = 'https://cloud.mail.ru/api/v2/trashbin/restore';
	API_TRASHBIN_EMPTY = 'https://cloud.mail.ru/api/v2/trashbin/empty';
	API_AB_CONTACTS = '';
	API_DISPATCHER = 'https://cloud.mail.ru/api/v2/dispatcher/';
	API_USER_SPACE = 'https://cloud.mail.ru/api/v2/user/space';
	API_CLONE = 'https://cloud.mail.ru/api/v2/clone';
	API_INVITE_REJECT = 'https://cloud.mail.ru/api/v2/folder/invites/reject';

	TYPE_DIR = 'folder';
	TYPE_FILE = 'file';

	KIND_SHARED = 'shared';
	{Error codes returned when parsing cloud responses. Extended as new errors are discovered}
	CLOUD_ERROR_TOKEN_OUTDATED = -3; {Token expired (custom code)}
	CLOUD_ERROR_UNKNOWN = -2; {unknown: server error}
	CLOUD_OPERATION_ERROR_STATUS_UNKNOWN = -1;
	CLOUD_OPERATION_OK = 0;
	CLOUD_OPERATION_FAILED = 1;
	CLOUD_OPERATION_CANCELLED = 5;

	CLOUD_ERROR_EXISTS = 1; {exists: folder with this name already exists}
	CLOUD_ERROR_REQUIRED = 2; {required: folder name cannot be empty}
	CLOUD_ERROR_INVALID = 3; {invalid: invalid folder name, forbidden characters}
	CLOUD_ERROR_READONLY = 4; {readonly|read_only: read-only access}
	CLOUD_ERROR_NAME_LENGTH_EXCEEDED = 5; {name_length_exceeded: folder name too long}
	CLOUD_ERROR_OVERQUOTA = 7; {overquota: not enough cloud space}
	CLOUD_ERROR_QUOTA_EXCEEDED = 7; {quota_exceeded: not enough cloud space}
	CLOUD_ERROR_NOT_EXISTS = 8; {not_exists: copied link does not exist}
	CLOUD_ERROR_OWN = 9; {own: cannot clone your own link}
	CLOUD_ERROR_NAME_TOO_LONG = 10; {name_too_long: file name too long}
	CLOUD_ERROR_VIRUS_SCAN_FAIL = 11; {virus_scan_fail: file is infected}
	CLOUD_ERROR_OWNER = 12; {Cannot use your own email}
	CLOUD_ERROR_FAHRENHEIT = 451; {Content blocked by rights holder or authorized state authority}
	CLOUD_ERROR_BAD_REQUEST = 400;
	CLOUD_ERROR_TREES_CONFLICT = 15; {Cannot share a folder that contains or resides in shared folders}
	CLOUD_ERROR_UNPROCESSABLE_ENTRY = 16; {Cannot grant access to the file}
	CLOUD_ERROR_USER_LIMIT_EXCEEDED = 17; {User limit: max 200 users per shared folder}
	CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED = 18; {Export limit: max 50 shared folders}
	CLOUD_ERROR_NOT_ACCEPTABLE = 406; {Cannot add this user}

	{Copy conflict modes}
	CLOUD_CONFLICT_STRICT = 'strict'; {Return error if file exists}
	CLOUD_CONFLICT_IGNORE = 'ignore'; {Apparently not implemented in API}
	CLOUD_CONFLICT_RENAME = 'rename'; {Rename the new file}
	{CLOUD_CONFLICT_REPLACE = 'overwrite'; unknown, this key is not documented}

	CLOUD_SHARE_ACCESS_READ_ONLY = 'read_only';
	CLOUD_SHARE_ACCESS_READ_WRITE = 'read_write';

	CLOUD_MAX_NAME_LENGTH = 255;
	CLOUD_PUBLISH = true;
	CLOUD_UNPUBLISH = false;

	CLOUD_SHARE_RW = 0;
	CLOUD_SHARE_RO = 1;
	CLOUD_SHARE_NO = 2;

	{Authentication method}
	CLOUD_AUTH_METHOD_OAUTH_APP = 4; {OAuth with app password (recommended)}
	CLOUD_AUTH_METHOD_VKID = 5; {VK ID browser login (cookie-based auth)}

	{OAuth client credentials for app password auth}
	OAUTH_CLIENT_ID = 'cloud-win';

	HTTP_FOUND_REDIRECT = 302;
	HTTP_ERROR_FORBIDDEN = 403; {Authentication failure — token expired or session invalid}
	HTTP_ERROR_NOT_FOUND = 404;
	{HTTP error codes for POST requests}
	HTTP_ERROR_BAD_REQUEST = 400;
	HTTP_ERROR_OVERQUOTA = 507;
	HTTP_ERROR_EXISTS = 500;

	{HTTP methods}
	HTTP_METHOD_GET = 0;
	HTTP_METHOD_POST = 1;
	HTTP_METHOD_PUT = 2;
	{Shard types}
	SHARD_TYPE_DEFAULT = ''; {Non-system shard, means use the shard obtained during initialization}
	SHARD_TYPE_VIDEO = 'video';
	SHARD_TYPE_VIEW_DIRECT = 'view_direct';
	SHARD_TYPE_WEBLINK_VIEW = 'weblink_view';
	SHARD_TYPE_WEBLINK_VIDEO = 'weblink_video';
	SHARD_TYPE_WEBLINK_GET = 'weblink_get';
	SHARD_TYPE_STOCK = 'stock';
	SHARD_TYPE_WEBLINK_THUMBNAILS = 'weblink_thumbnails';
	SHARD_TYPE_PUBLIC_UPLOAD = 'public_upload';
	SHARD_TYPE_AUTH = 'auth';
	SHARD_TYPE_WEB = 'web';
	SHARD_TYPE_UPLOAD = 'upload';
	SHARD_TYPE_GET = 'get';
	SHARD_TYPE_THUMBNAILS = 'thumbnails';

	{JSON names}
	NAME_TOKEN = 'token';
	NAME_BODY = 'body';
	NAME_LIST = 'list';
	NAME_SIZE = 'size';
	NAME_KIND = 'kind';
	NAME_WEBLINK = 'weblink';
	NAME_TYPE = 'type';
	NAME_HOME = 'home';
	NAME_NAME = 'name'; {funny}
	NAME_DELETED_AT = 'deleted_at';
	NAME_DELETED_FROM = 'deleted_from';
	NAME_DELETED_BY = 'deleted_by';
	NAME_GREV = 'grev';
	NAME_REV = 'rev';
	NAME_MTIME = 'mtime';
	NAME_VIRUS_SCAN = 'virus_scan';
	NAME_HASH = 'hash';
	NAME_TREE = 'tree';
	NAME_COUNT = 'count';
	NAME_EMAIL = 'email';
	NAME_STATUS = 'status';
	NAME_ACCESS = 'access';
	NAME_OVERQUOTA = 'overquota';
	NAME_TOTAL = 'bytes_total';
	NAME_USED = 'bytes_used';
	NAME_FOLDERS = 'folders';
	NAME_FILES = 'files';
	NAME_INVITED = 'invited';
	NAME_OWNER = 'owner';
	NAME_INVITE_TOKEN = 'invite_token';
	NAME_ERROR = 'error';
	NAME_ERROR_CODE = 'error_code';
	NAME_ERROR_DESCRIPTION = 'error_description';
	NAME_EXPIRES_IN = 'expires_in';
	NAME_REFRESH_TOKEN = 'refresh_token';
	NAME_ACCESS_TOKEN = 'access_token';
	NAME_INVITE = 'invite';
	NAME_INVITE_EMAIL = 'invite_email';
	NAME_GET = 'get';
	NAME_URL = 'url';

	{Error values from API responses}
	NAME_ERROR_NOT_AUTHORIZED = 'NOT/AUTHORIZED';

	{Streaming formats}
	STREAMING_FORMAT_UNSET = -1; {not a format, it set when the streaming settings cannot be retrieved for an extension}
	STREAMING_FORMAT_NONE = 0;
	STREAMING_FORMAT_DISABLED = 1;
	STREAMING_FORMAT_PLAYLIST = 2;
	STREAMING_FORMAT_DEFAULT = 3;
	STREAMING_FORMAT_WEBLINK_VIEW = 4;
	{Does not work externally}
	STREAMING_FORMAT_VIEW_DIRECT = 5;
	STREAMING_FORMAT_VIDEO = 6;
	STREAMING_FORMAT_THUMBNAILS = 7;
	STREAMING_FORMAT_WEBLINK_THUMBNAILS = 8;

	{Log levels (powers of two)}
	LOG_LEVEL_CONNECT = 1; {connection}
	LOG_LEVEL_FILE_OPERATION = 2; {file operations and free space}
	LOG_LEVEL_DETAIL = 4; {detailed info (retries etc.)}
	LOG_LEVEL_WARNING = 8; {non-critical warnings}
	LOG_LEVEL_ERROR = 16; {error details}
	LOG_LEVEL_DEBUG = 32; {internal debugging info}
	LOG_LEVEL_HTTP = 64; {HTTP request/response tracing}

	{FsFindFirst* success return codes (INVALID_HANDLE_VALUE on error)}
	FIND_NO_MORE_FILES = 0;
	FIND_OK = 1;
	FIND_ROOT_DIRECTORY = 2;
	FIND_SHARED_LINKS = 3; {.shared folder}

	TYPE_BYTES = 0;

implementation

end.

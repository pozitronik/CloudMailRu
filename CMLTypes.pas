unit CMLTypes;

interface

const
	PUBLIC_ACCESS_URL = 'https://cloud.mail.ru/public/';
	OAUTH_TOKEN_URL = 'https://o2.mail.ru/token';
	TOKEN_HOME_URL = 'https://cloud.mail.ru/home';
	TOKEN_URL = 'https://cloud.mail.ru/?from=promo&from=authpopup';
	LOGIN_URL = 'https://auth.mail.ru/cgi-bin/auth?from=splash';
	SECSTEP_URL = 'https://auth.mail.ru/cgi-bin/secstep';

	API_CSRF = 'https://cloud.mail.ru/api/v2/tokens/csrf';
	API_FILE = 'https://cloud.mail.ru/api/v2/file';
	API_FILE_MOVE = 'https://cloud.mail.ru/api/v2/file/move';
	API_FILE_PUBLISH = 'https://cloud.mail.ru/api/v2/file/publish';
	API_FILE_UNPUBLISH = 'https://cloud.mail.ru/api/v2/file/unpublish';
	API_FILE_RENAME = 'https://cloud.mail.ru/api/v2/file/rename';
	API_FILE_ADD = 'https://cloud.mail.ru/api/v2/file/add';
	API_FILE_REMOVE = 'https://cloud.mail.ru/api/v2/file/remove';
	API_FILE_COPY = 'https://cloud.mail.ru/api/v2/file/copy';
	API_FOLDER = 'https://cloud.mail.ru/api/v2/folder?sort={%22type%22%3A%22name%22%2C%22order%22%3A%22asc%22}&offset=0&limit=65535';
	API_FOLDER_ADD = 'https://cloud.mail.ru/api/v2/folder/add';
	API_FOLDER_SHARED_INFO = 'https://cloud.mail.ru/api/v2/folder/shared/info'; //get
	API_FOLDER_INVITES = 'https://cloud.mail.ru/api/v2/folder/invites';
	API_FOLDER_SHARE = 'https://cloud.mail.ru/api/v2/folder/share';
	API_FOLDER_UNSHARE = 'https://cloud.mail.ru/api/v2/folder/unshare';
	API_FOLDER_MOUNT = 'https://cloud.mail.ru/api/v2/folder/mount';
	API_FOLDER_UNMOUNT = 'https://cloud.mail.ru/api/v2/folder/unmount';
	API_FOLDER_SHARED_LINKS = 'https://cloud.mail.ru/api/v2/folder/shared/links';
	API_FOLDER_SHARED_INCOMING = 'https://cloud.mail.ru/api/v2/folder/shared/incoming';
	API_TRASHBIN = 'https://cloud.mail.ru/api/v2/trashbin';
	API_TRASHBIN_RESTORE = 'https://cloud.mail.ru/api/v2/trashbin/restore';
	API_TRASHBIN_EMPTY = 'https://cloud.mail.ru/api/v2/trashbin/empty';
	API_AB_CONTACTS = ''; //
	API_DISPATCHER = 'https://cloud.mail.ru/api/v2/dispatcher/';
	API_USER_SPACE = 'https://cloud.mail.ru/api/v2/user/space';
	API_CLONE = 'https://cloud.mail.ru/api/v2/clone';
	API_INVITE_REJECT = 'https://cloud.mail.ru/api/v2/folder/invites/reject';

	TYPE_DIR = 'folder';
	TYPE_FILE = 'file';

	KIND_SHARED = 'shared';
	{Константы для обозначения ошибок, возвращаемых при парсинге ответов облака. Дополняем по мере обнаружения}
	CLOUD_ERROR_TOKEN_OUTDATED = -3; //протух токен (ввел самостоятельно)
	CLOUD_ERROR_UNKNOWN = -2; //unknown: 'Ошибка на сервере'
	CLOUD_OPERATION_ERROR_STATUS_UNKNOWN = -1;
	CLOUD_OPERATION_OK = 0;
	CLOUD_OPERATION_FAILED = 1;
	CLOUD_OPERATION_CANCELLED = 5;

	CLOUD_ERROR_EXISTS = 1; //exists: 'Папка с таким названием уже существует. Попробуйте другое название'
	CLOUD_ERROR_REQUIRED = 2; //required: 'Название папки не может быть пустым'
	CLOUD_ERROR_INVALID = 3; //invalid: '&laquo;' + app.escapeHTML(name) + '&raquo; это неправильное название папки. В названии папок нельзя использовать символы «" * / : < > ?  \\ |»'
	CLOUD_ERROR_READONLY = 4; //readonly|read_only: 'Невозможно создать. Доступ только для просмотра'
	CLOUD_ERROR_NAME_LENGTH_EXCEEDED = 5; //name_length_exceeded: 'Ошибка: Превышена длина имени папки. <a href="https://help.mail.ru/cloud_web/confines" target="_blank">Подробнее…</a>'
	CLOUD_ERROR_OVERQUOTA = 7; //overquota: 'Невозможно скопировать, в вашем Облаке недостаточно места'
	CLOUD_ERROR_QUOTA_EXCEEDED = 7; //"quota_exceeded": 'Невозможно скопировать, в вашем Облаке недостаточно места'
	CLOUD_ERROR_NOT_EXISTS = 8; //"not_exists": 'Копируемая ссылка не существует'
	CLOUD_ERROR_OWN = 9; //"own": 'Невозможно клонировать собственную ссылку'
	CLOUD_ERROR_NAME_TOO_LONG = 10; //"name_too_long": 'Превышен размер имени файла'
	CLOUD_ERROR_VIRUS_SCAN_FAIL = 11; //"virus_scan_fail": 'Файл заражен вирусом'
	CLOUD_ERROR_OWNER = 12; //Нельзя использовать собственный email
	CLOUD_ERROR_FAHRENHEIT = 451; //Публикация контента заблокирована по требованию правообладателя или уполномоченного государственного ведомства.
	CLOUD_ERROR_BAD_REQUEST = 400; //
	CLOUD_ERROR_TREES_CONFLICT = 15; //Нельзя сделать папку общей, если она содержит другие общие папки или находится в общей папке
	CLOUD_ERROR_UNPROCESSABLE_ENTRY = 16; //Нельзя открыть доступ к файлу
	CLOUD_ERROR_USER_LIMIT_EXCEEDED = 17; //Невозможно добавить пользователя. Вы можете иметь не более 200 пользователей в одной общей папке
	CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED = 18; //Невозможно добавить пользователя. Вы можете создать не более 50 общих папок
	CLOUD_ERROR_NOT_ACCEPTABLE = 406; //Нельзя добавить этого пользователя

	{Режимы работы при конфликтах копирования}
	CLOUD_CONFLICT_STRICT = 'strict'; //возвращаем ошибку при существовании файла
	CLOUD_CONFLICT_IGNORE = 'ignore'; //В API, видимо, не реализовано
	CLOUD_CONFLICT_RENAME = 'rename'; //Переименуем новый файл
	//CLOUD_CONFLICT_REPLACE = 'overwrite'; // хз, этот ключ не вскрыт

	CLOUD_SHARE_ACCESS_READ_ONLY = 'read_only';
	CLOUD_SHARE_ACCESS_READ_WRITE = 'read_write';

	CLOUD_MAX_NAME_LENGTH = 255;
	CLOUD_PUBLISH = true;
	CLOUD_UNPUBLISH = false;

	CLOUD_SHARE_RW = 0;
	CLOUD_SHARE_RO = 1;
	CLOUD_SHARE_NO = 2;

	{Поддерживаемые методы авторизации}
	CLOUD_AUTH_METHOD_WEB = 0; //Через парсинг HTTP-страницы
	CLOUD_AUTH_METHOD_TWO_STEP = 1; //Через парсинг HTTP-страницы, двухфакторная
	CLOUD_AUTH_METHOD_OAUTH = 2; //Через сервер OAuth-авторизации
	CLOUD_AUTH_METHOD_API = 3; //Новый двухступенчатоый метод авторизации

	{Константа использования мобильного аутентификатора для двухфакторной авторизации}
	AUTH_APP_USED = -1;

	HTTP_FOUND_REDIRECT = 302;
	HTTP_ERROR_NOT_FOUND = 404;
	{Коды HTTP-ошибок при постинге запросов}
	HTTP_ERROR_BAD_REQUEST = 400;
	HTTP_ERROR_OVERQUOTA = 507;
	HTTP_ERROR_EXISTS = 500;

	{Методы HTTP}
	HTTP_METHOD_GET = 0;
	HTTP_METHOD_POST = 1;
	HTTP_METHOD_PUT = 2;
	HTTP_METHOD_OPTIONS = 3;

	HTTP_METHODS = [HTTP_METHOD_GET, HTTP_METHOD_POST, HTTP_METHOD_PUT, HTTP_METHOD_OPTIONS];

	{Типы шардов}
	SHARD_TYPE_DEFAULT = ''; //несистемный шард, в нашей логике означает использование того шарда, что получен при инициализации
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
	SHARD_TYPE_THUMBNAILS = 'thumbnails'; {todo: thumbnails mode for tc maybe?}

	{JSON names}
	NAME_TOKEN = 'token';
	NAME_BODY = 'body';
	NAME_LIST = 'list';
	NAME_SIZE = 'size';
	NAME_KIND = 'kind';
	NAME_WEBLINK = 'weblink';
	NAME_TYPE = 'type';
	NAME_HOME = 'home';
	NAME_NAME = 'name'; //funny
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
	NAME_FORM_NAME = 'form_name';
	NAME_AUTH_HOST = 'auth_host';
	NAME_SECSTEP_PHONE = 'secstep_phone';
	NAME_SECSTEP_PAGE = 'secstep_page';
	NAME_SECSTEP_CODE_FAIL = 'secstep_code_fail';
	NAME_SECSTEP_RESEND_FAIL = 'secstep_resend_fail';
	NAME_SECSTEP_RESEND_SUCCESS = 'secstep_resend_success';
	NAME_SECSTEP_TIMEOUT = 'secstep_timeout';
	NAME_SECSTEP_LOGIN = 'secstep_login';
	NAME_SECSTEP_DISPOSABLE_FAIL = 'secstep_disposable_fail';
	NAME_SECSTEP_SMSAPI_ERROR = 'secstep_smsapi_error';
	NAME_SECSTEP_CAPTCHA = 'secstep_captcha';
	NAME_TOTP_ENABLED = 'totp_enabled';
	NAME_LOCALE = 'locale';
	NAME_CLIENT = 'client';
	NAME_CSRF = 'csrf';
	NAME_DEVICE = 'device';

	{Streaming formats}
	STREAMING_FORMAT_NONE = 0;
	STREAMING_FORMAT_DISABLED = 1;
	STREAMING_FORMAT_PLAYLIST = 2;
	STREAMING_FORMAT_DEFAULT = 3;
	STREAMING_FORMAT_WEBLINK_VIEW = 4;
	//Не работает вовне
	STREAMING_FORMAT_VIEW_DIRECT = 5;
	STREAMING_FORMAT_VIDEO = 6;
	STREAMING_FORMAT_THUMBNAILS = 7;
	STREAMING_FORMAT_WEBLINK_THUMBNAILS = 8;

function ShardTypeFromStreamingFormat(StreamingFormat: integer): string;

implementation

function ShardTypeFromStreamingFormat(StreamingFormat: integer): string;
begin
	case StreamingFormat of
		STREAMING_FORMAT_WEBLINK_VIEW:
			Result := SHARD_TYPE_WEBLINK_VIEW;
		STREAMING_FORMAT_VIDEO:
			Result := SHARD_TYPE_VIDEO;
		STREAMING_FORMAT_VIEW_DIRECT:
			Result := SHARD_TYPE_VIEW_DIRECT;
		STREAMING_FORMAT_THUMBNAILS:
			Result := SHARD_TYPE_THUMBNAILS;
		STREAMING_FORMAT_WEBLINK_THUMBNAILS:
			Result := SHARD_TYPE_WEBLINK_THUMBNAILS;
		else
			Result := SHARD_TYPE_DEFAULT;
	end;
end;

end.

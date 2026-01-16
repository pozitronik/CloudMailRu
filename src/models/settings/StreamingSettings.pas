unit StreamingSettings;

interface

type
	{Параметры стриминга для расширения}
	TStreamingSettings = record
		Command: WideString; //Вызываемое приложение
		Parameters: WideString; //параметры, передаваемые приложению
		StartPath: WideString; //каталог запуска
		Format: Integer; {see STREAMING_FORMAT_* constants}
	end;

implementation

end.

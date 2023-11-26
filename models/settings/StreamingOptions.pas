unit StreamingOptions;

interface

type

	{Параметры стриминга для расширения}
	TStreamingOptions = record
		Command: WideString; //Вызываемое приложение
		Parameters: WideString; //параметры, передаваемые приложению
		StartPath: WideString; //каталог запуска
		Format: integer;
	end;

implementation

end.

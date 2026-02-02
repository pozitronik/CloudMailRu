unit StreamingSettings;

interface

type
	{Streaming settings for file extension}
	TStreamingSettings = record
		Command: WideString; {Application to execute}
		Parameters: WideString; {Parameters passed to the application}
		StartPath: WideString; {Working directory}
		Format: Integer; {see STREAMING_FORMAT_* constants}
	end;

implementation

end.

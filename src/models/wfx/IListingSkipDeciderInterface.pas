unit IListingSkipDeciderInterface;

{Interface for deciding whether directory listing should be skipped.
 Encapsulates skip-list flag checks and user abort detection via progress callback.
 Used by FsFindFirst to determine if listing operation should proceed.}

interface

type
	{Result of skip decision with abort information}
	TListingSkipResult = record
		ShouldSkip: Boolean;   {True if listing should be skipped}
		WasAborted: Boolean;   {True if user aborted via progress callback}
	end;

	{Decides whether listing should be skipped based on thread state and user input.
	 Checks skip-list flags for delete/renmov operations and abort via progress callback.}
	IListingSkipDecider = interface
		['{7E2A9F81-C4D6-4B3E-A5F8-1D9C6E4B2A7F}']
		{Determines if listing should be skipped.
		 @param Path Directory path being listed (used for progress callback)
		 @return Skip result with ShouldSkip and WasAborted flags}
		function ShouldSkipListing(const Path: WideString): TListingSkipResult;
	end;

implementation

end.

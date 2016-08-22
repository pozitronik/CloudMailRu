unit FileSplitter;

interface

type

	SplittedFile = record
		filename: WideString;
		size: int64;
		crc32: WideString; // String representation
		parts: integer;
	end;

	TFileSplitter = class
	public
		constructor Ñreate(filename: WideString; SplitSize: int64 = $80000000);
		destructor Destroy; override;

	end;

implementation

{ TFileSplitter }

destructor TFileSplitter.Destroy;
begin

	inherited;
end;

constructor TFileSplitter.Ñreate(filename: WideString; SplitSize: int64);
begin

end;

end.

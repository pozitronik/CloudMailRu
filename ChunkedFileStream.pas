unit ChunkedFileStream;

interface

uses System.Classes, System.SysUtils;

type

	TChunkedFileStream = class(TBufferedFileStream)
	private
		ChunkStart, ChunkSize, Position: Int64;
	protected
		function GetSize(): Int64; override;
	public
		property Size: Int64 read GetSize;
		constructor Create(const AFileName: string; Mode: Word; ChunkStart, ChunkSize: Int64);
		function Read(var Buffer; Count: Longint): Longint; overload; override;
	end;

implementation

{TChunkedFileStream}

constructor TChunkedFileStream.Create(const AFileName: string; Mode: Word; ChunkStart, ChunkSize: Int64);
begin
	self.ChunkStart := ChunkStart;
	self.ChunkSize := ChunkSize;

	inherited Create(AFileName, Mode);
	self.Position := 0;;

end;

function TChunkedFileStream.GetSize: Int64;
begin
	result := ChunkSize;
end;

function TChunkedFileStream.Read(var Buffer; Count: Integer): Longint;
var
	fileStartPosition, fileEndPosition: Int64;
	BytesToRead: Int64;

begin
	fileStartPosition := self.Position + self.ChunkStart; //позиция чтения из реального файла
	fileEndPosition := self.ChunkStart + self.ChunkSize; //позиция конца чтения из реального файла

	if fileStartPosition >= fileEndPosition then
		Exit(0); //Пытаемся читать за границей чанка

	if Count > self.ChunkSize then
		BytesToRead := self.ChunkSize
	else
		BytesToRead := Count; //Количество байт для прочтения

	inherited Seek(fileStartPosition, soBeginning);
	result := inherited Read(Buffer, BytesToRead);
	self.Position := self.Position + result;
end;


end.

unit ChunkedFileStream;

interface

uses System.Classes, System.SysUtils;

type

	TChunkedFileStream = class(TStream)
	private
		FStartPos, FSize: Int64;
		FileStream: TFileStream;
	protected
		function GetSize(): Int64; override;
	public
		property Size: Int64 read GetSize;

		constructor Create(const AFileName: string; Mode: Word; ChunkStart, ChunkSize: Int64);
		destructor Destroy; override;
		function Read(var Buffer; Count: Longint): Longint; override;
		function Write(const Buffer; Count: Longint): Longint; override;
		function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
	end;

implementation

{TChunkedFileStream}

constructor TChunkedFileStream.Create(const AFileName: string; Mode: Word; ChunkStart, ChunkSize: Int64);
var

	FileSize: Int64;
begin
	inherited Create;
	self.FileStream := TFileStream.Create(AFileName, Mode);
	FileSize := FileStream.Size;
	if (ChunkSize < 0) or (ChunkSize > FileSize) or (ChunkStart < 0) or (ChunkStart > FileSize) or (FileStream.Seek(ChunkStart, soBeginning) <> ChunkStart) then
		raise EReadError.Create('Can''t read from ' + AFileName + ' ' + ChunkSize.ToString + ' bytes at ' + ChunkStart.ToString);
	self.FStartPos := ChunkStart;
	if ChunkSize = 0 then
		self.FSize := FileSize - ChunkStart
	else
		self.FSize := ChunkSize;

end;

destructor TChunkedFileStream.Destroy;
begin
	FileStream.Free;
	inherited Destroy;
end;

function TChunkedFileStream.GetSize: Int64;
begin
	result := FSize;
end;

function TChunkedFileStream.Read(var Buffer; Count: Longint): Longint;
begin
	if FileStream.Position + Count > self.FStartPos + self.FSize then
		Count := self.FStartPos + self.FSize - FileStream.Position;
	if Count > 0 then
		result := FileStream.Read(Buffer, Count)
	else
		result := 0;
end;

function TChunkedFileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
	NewPos: Int64;
begin
	case Origin of
		soBeginning:
			result := FileStream.Seek(FStartPos + Offset, soBeginning) - FStartPos;
		soCurrent:
			begin
				NewPos := FileStream.Position + Offset - FStartPos;
				if NewPos < 0 then
					NewPos := 0;
				if NewPos > FSize then
					NewPos := FSize;
				result := FileStream.Seek(FStartPos + NewPos, soBeginning) - FStartPos;
			end;
		soEnd:
			result := FileStream.Seek(FStartPos + FSize + Offset, soBeginning) - FStartPos;
		else
			result := -1;
	end;
end;

function TChunkedFileStream.Write(const Buffer; Count: Longint): Longint;
begin
	result := 0; //too lazy to implement
end;

end.

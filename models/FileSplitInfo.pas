unit FileSplitInfo;

interface

uses
	System.SysUtils,
	System.Classes,
	PluginHelper,
	CMRStrings,
	Winapi.Windows;

Const
	CRCSeed = $FFFFFFFF;
	CRC32tab: Array [0 .. 255] of DWord = ($00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535, $9E6495A3, $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91, $1DB71064, $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7, $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4, $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B, $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59, $26D930AC, $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F, $2802B89E, $5F058808, $C60CD9B2, $B10BE924, $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D, $76DC4190, $01DB7106, $98D220BC, $EFD5102A, $71B18589,
		$06B6B51F, $9FBFE4A5, $E8B8D433, $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01, $6B6B51F4, $1C6C6162, $856530D8, $F262004E, $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457, $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65, $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB, $4369E96A, $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9, $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F, $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD, $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $04DB2615, $73DC1683, $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8, $E40ECF0B,
		$9309FF9D, $0A00AE27, $7D079EB1, $F00F9344, $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB, $196C3671, $6E6B06E7, $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5, $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B, $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79, $CB61B38C, $BC66831A, $256FD2A0, $5268E236, $CC0C7795, $BB0B4703, $220216B9, $5505262F, $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D, $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9, $EB0E363F, $72076785, $05005713, $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21, $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E, $81BE16CD,
		$F6B9265B, $6FB077E1, $18B74777, $88085AE6, $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69, $616BFFD3, $166CCF45, $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7, $4969474D, $3E6E77DB, $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9, $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693, $54DE5729, $23D967BF, $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);

type

	//part of file
	PFileChunkInfo = ^TFileChunkInfo;

	TFileChunkInfo = record
		name: WideString; //filename.000
		start: Int64; //offset from beginning of original file
		size: Int64; //part size
	end;

	AFileChunkInfo = array of TFileChunkInfo;

	TFileSplitInfo = class
	private
		Chunks: AFileChunkInfo; //parts names
		ChunkSize: Int64;
		TotalChunksCount: Int64;
		filename: WideString;
		filesize: Int64;
		function AddLeadingZeroes(const aNumber, length: Integer): string;
		function crc32_update(inbuffer: pointer; buffersize, crc: DWord): DWord;
		function CRCend(crc: DWord): DWord;
		function GetCRC32File(filename: WideString = ''): WideString;
		function GetCRCFileName(): WideString;

	public
		property ChunksCount: Int64 read TotalChunksCount;
		property GetChunks: AFileChunkInfo read Chunks;
		property CRCFileName: WideString read GetCRCFileName;
		constructor Create(filename: WideString; ChunkSize: Int64);
		destructor Destroy; override;

		procedure GetCRCData(DataStream: TStringStream);

	end;

implementation

{TFileSplitter}

constructor TFileSplitInfo.Create(filename: WideString; ChunkSize: Int64);
var
	partCount: Integer;
begin
	if not(FileExists(filename)) then
		raise Exception.Create(ERR_FILE_NOT_EXISTS);

	self.ChunkSize := ChunkSize;
	self.filename := filename;
	self.filesize := SizeOfFile(filename);

	self.TotalChunksCount := self.filesize div self.ChunkSize;
	if (self.filesize mod self.ChunkSize) <> 0 then
		inc(self.TotalChunksCount);

	SetLength(self.Chunks, self.TotalChunksCount);
	for partCount := 0 to self.TotalChunksCount - 1 do
	begin
		with self.Chunks[partCount] do
		begin
			name := ExtractFileName(ChangeFileExt(self.filename, '.' + self.AddLeadingZeroes(partCount + 1, 3)));
			start := partCount * ChunkSize;
			if (partCount = self.TotalChunksCount - 1) then
				size := self.filesize - (ChunkSize * (self.TotalChunksCount - 1))
			else
				size := ChunkSize;

		end;
	end;

end;

function TFileSplitInfo.AddLeadingZeroes(const aNumber, length: Integer): string;
begin
	result := Format('%.*d', [length, aNumber]);
end;

function TFileSplitInfo.crc32_update(inbuffer: pointer; buffersize, crc: DWord): DWord;
type
	PDWord = ^DWord;
	PByte = ^Byte;
var
	currptr: pointer;
	i: Byte;
begin
	currptr := inbuffer;
	result := crc;
	while buffersize > 4 do
	begin
		result := result xor PDWord(currptr)^;
		inc(NativeInt(currptr), 4);
		result := (result shr 8) xor CRC32tab[Byte(result)];
		result := (result shr 8) xor CRC32tab[Byte(result)];
		result := (result shr 8) xor CRC32tab[Byte(result)];
		result := (result shr 8) xor CRC32tab[Byte(result)];
		dec(buffersize, 4);
	end;
	for i := 1 to buffersize do
	begin
		result := CRC32tab[Byte(result xor DWord(PByte(currptr)^))] xor (result shr 8);
		inc(NativeInt(currptr), 1);
	end;

end;

function TFileSplitInfo.CRCend(crc: DWord): DWord;
begin
	CRCend := (crc xor CRCSeed);
end;

destructor TFileSplitInfo.Destroy;
begin
	inherited;
end;

function TFileSplitInfo.GetCRC32File(filename: WideString): WideString;
var
	CRCValue: DWord;
	inbuffer: array [1 .. 32768] of Byte;
	ReadBytes: Integer;
	Stream: TBufferedFileStream;
begin
	if EmptyWideStr = filename then
		filename := self.filename;

	Stream := TBufferedFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
	CRCValue := CRCSeed;
	ReadBytes := Stream.Read(inbuffer, Sizeof(inbuffer));
	repeat
		CRCValue := crc32_update(@inbuffer, ReadBytes, CRCValue);
		ReadBytes := Stream.Read(inbuffer, Sizeof(inbuffer));
	until ReadBytes = 0;
	Stream.Destroy;
	CRCValue := CRCend(CRCValue);
	exit(IntToHex(CRCValue, 8));
end;

procedure TFileSplitInfo.GetCRCData(DataStream: TStringStream);
begin
	DataStream.WriteString(Format('filename=%s%ssize=%d%scrc32=%s', [ExtractFileName(self.filename), sLineBreak, self.filesize, sLineBreak, self.GetCRC32File(self.filename)]));
end;

function TFileSplitInfo.GetCRCFileName: WideString;
begin
	result := ExtractFileName(ChangeFileExt(self.filename, '.crc'));
end;

end.

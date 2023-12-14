unit CipherInterface;

interface

uses
	Classes,
	CMRDirItemList;

type
	ICipherInterface = interface
		['{54C0EBB7-5186-4D89-A2D6-050D4A6CD58B}']

		function CryptFile(SourceFileName, DestinationFilename: WideString): Integer;
		function CryptStream(SourceStream, DestinationStream: TStream): Integer;
		function CryptFileName(const FileName: WideString): WideString;

		function DecryptFile(SourceFileName, DestinationFilename: WideString): Integer;
		function DecryptStream(SourceStream, DestinationStream: TStream): Integer;
		function DecryptFileName(const FileName: WideString): WideString;
		procedure DecryptDirListing(var CloudMailRuDirListing: TCMRDirItemList);

	end;

implementation

end.

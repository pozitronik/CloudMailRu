unit Descriptions;

{ Simple && read-only descript.ion files support }
interface

uses
	System.Types, System.Classes, System.AnsiStrings, Generics.Collections, System.SysUtils, System.WideStrUtils;

type

	TDescription = class

	private
	var
		items: TDictionary<WideString, WideString>;

	public
		constructor Create();
		destructor Destroy; override;
		function Read(ion_filename: WideString): integer;
		function GetValue(item: WideString): WideString;
		procedure Clear;
	end;

implementation

{ TDescription }

procedure TDescription.Clear;
begin
	self.items.Clear;
end;

constructor TDescription.Create;
begin
	self.items := TDictionary<WideString, WideString>.Create;
end;

destructor TDescription.Destroy;
begin
	self.items.Free;
end;

function TDescription.GetValue(item: WideString): WideString;
begin
	if not items.TryGetValue(item, result) then exit('');

	result := WideStringReplace(WideStringReplace(result, '\n', '  ', [rfReplaceAll]), chr($04) + 'Â', '', [rfReplaceAll]);
end;

function TDescription.Read(ion_filename: WideString): integer; // Loads desript.ion file
var
	fStream: TStreamReader;
	line, key, value: WideString;
	t: integer;
begin
	self.Clear;
	fStream := TStreamReader.Create(ion_filename, TEncoding.Default, True);
	while not fStream.EndOfStream do
	begin
		line := fStream.ReadLine;
		if StartsStr('"', line) then
		begin
			t := PosEx('" ', line);
			value := copy(line, t + 2, length(line));
			key := copy(line, 2, t - 2);
		end else begin
			t := PosEx(' ', line);
			value := copy(line, t + 1, length(line));
			key := copy(line, 0, t - 1);
		end;

		items.Add(key, value);
	end;
	fStream.Free;
end;

end.

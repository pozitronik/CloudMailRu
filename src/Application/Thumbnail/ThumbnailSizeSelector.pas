unit ThumbnailSizeSelector;

{Selects optimal thumbnail size preset based on requested dimensions.
	Includes all available presets - the server handles aspect ratio scaling.}

interface

type
	{Record representing a thumbnail size preset}
	TThumbnailPreset = record
		Name: string;   {Preset identifier (e.g., 'xw14')}
		Width: Integer;
		Height: Integer;
	end;

	IThumbnailSizeSelector = interface
		['{A1B2C3D4-E5F6-4789-ABCD-EF0123456789}']
		{Select the optimal thumbnail preset for requested dimensions.
			@param RequestedWidth Maximum width requested by TC
			@param RequestedHeight Maximum height requested by TC
			@return Preset name (e.g., 'xw14') to use in thumbnail URL}
		function SelectPreset(RequestedWidth, RequestedHeight: Integer): string;
	end;

	TThumbnailSizeSelector = class(TInterfacedObject, IThumbnailSizeSelector)
	public
		function SelectPreset(RequestedWidth, RequestedHeight: Integer): string;
	end;

{Standalone function for use without interface}
function SelectThumbnailPreset(RequestedWidth, RequestedHeight: Integer): string;

implementation

uses
	Math,
	CloudConstants;

const
	{All available presets with their approximate dimensions.
		Sorted by area (width * height) ascending for selection algorithm.}
	PRESET_COUNT = 20;
	PRESETS: array[0..PRESET_COUNT-1] of TThumbnailPreset = (
		(Name: 'xw11'; Width: 26;   Height: 26),    {Square icon}
		(Name: 'xw27'; Width: 28;   Height: 38),    {Portrait tiny}
		(Name: 'xw22'; Width: 36;   Height: 24),    {Landscape tiny}
		(Name: 'xw12'; Width: 52;   Height: 35),
		(Name: 'xw28'; Width: 64;   Height: 43),
		(Name: 'xw23'; Width: 72;   Height: 48),
		(Name: 'xw14'; Width: 160;  Height: 107),
		(Name: 'xw17'; Width: 160;  Height: 120),
		(Name: 'xw10'; Width: 160;  Height: 120),
		(Name: 'xw29'; Width: 150;  Height: 150),   {Square}
		(Name: 'xw24'; Width: 168;  Height: 112),
		(Name: 'xw20'; Width: 170;  Height: 113),
		(Name: 'xw15'; Width: 206;  Height: 137),
		(Name: 'xw19'; Width: 206;  Height: 206),   {Square}
		(Name: 'xw26'; Width: 270;  Height: 365),   {Portrait}
		(Name: 'xw18'; Width: 305;  Height: 230),
		(Name: 'xw13'; Width: 320;  Height: 213),
		(Name: 'xw16'; Width: 320;  Height: 240),
		(Name: 'xw25'; Width: 336;  Height: 224),
		(Name: 'xw21'; Width: 340;  Height: 226)
	);

	{Large presets - use when requested size exceeds all standard presets}
	PRESET_XW2: TThumbnailPreset = (Name: 'xw2'; Width: 1000; Height: 667);
	PRESET_XW1: TThumbnailPreset = (Name: 'xw1'; Width: MaxInt; Height: MaxInt); {Original}

function SelectThumbnailPreset(RequestedWidth, RequestedHeight: Integer): string;
var
	I: Integer;
begin
	{Find smallest preset that can accommodate both requested dimensions}
	for I := Low(PRESETS) to High(PRESETS) do
		if (PRESETS[I].Width >= RequestedWidth) and (PRESETS[I].Height >= RequestedHeight) then
			Exit(PRESETS[I].Name);

	{Check large presets}
	if (PRESET_XW2.Width >= RequestedWidth) and (PRESET_XW2.Height >= RequestedHeight) then
		Exit(PRESET_XW2.Name);

	{Fallback to original size for very large requests}
	Result := PRESET_XW1.Name;
end;

{TThumbnailSizeSelector}

function TThumbnailSizeSelector.SelectPreset(RequestedWidth, RequestedHeight: Integer): string;
begin
	Result := SelectThumbnailPreset(RequestedWidth, RequestedHeight);
end;

end.

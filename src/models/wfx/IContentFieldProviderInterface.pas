unit IContentFieldProviderInterface;

interface

uses
	CMRDirItem;

type
	{ Context for fields that need external state (e.g., description field).
	  Passed to GetValue to provide data not available in TCMRDirItem. }
	TContentFieldContext = record
		DescriptionsEnabled: Boolean;
		FileDescription: WideString;
		AccountDescription: WideString;
		IsAccountRoot: Boolean;
	end;

	{ Provides content field metadata and value extraction for WFX plugin.
	  Separates field definitions from WFX interface implementation. }
	IContentFieldProvider = interface
		['{B8F4A2C1-5D3E-4F6A-9B7C-8D2E1F0A3B4C}']
		{ Returns field name and type for Total Commander content plugin API.
		  FieldIndex: 0-based field index
		  FieldName: buffer to receive ANSI field name
		  MaxLen: buffer size
		  Returns: field type constant (ft_stringw, ft_numeric_32, etc.) or ft_nomorefields }
		function GetSupportedField(FieldIndex: Integer; FieldName: PAnsiChar; MaxLen: Integer): Integer;

		{ Extracts field value from directory item.
		  FieldIndex: 0-based field index
		  Item: directory item to extract from
		  FieldValue: buffer to receive value (type depends on field)
		  Context: external state for context-dependent fields
		  Returns: field type constant or ft_nosuchfield if not applicable }
		function GetValue(FieldIndex: Integer; const Item: TCMRDirItem;
			FieldValue: Pointer; const Context: TContentFieldContext): Integer;

		{ Returns total number of supported fields }
		function GetFieldCount: Integer;
	end;

implementation

end.

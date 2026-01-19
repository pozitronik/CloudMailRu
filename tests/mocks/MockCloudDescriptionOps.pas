unit MockCloudDescriptionOps;

{Mock implementation of ICloudDescriptionOps for testing TDescriptionSyncManager.
 Allows verifying cloud operations without actual HTTP requests.}

interface

uses
	System.Classes,
	System.SysUtils,
	System.Generics.Collections,
	CloudDescriptionOpsAdapter;

type
	{Records a single cloud operation for verification}
	TCloudOperation = record
		Operation: WideString; {GET, PUT, DELETE}
		RemotePath: WideString;
		LocalPath: WideString;
	end;

	{Mock implementation that records operations and returns canned responses}
	TMockCloudDescriptionOps = class(TInterfacedObject, ICloudDescriptionOps)
	private
		FOperations: TList<TCloudOperation>;
		FGetFileResponses: TDictionary<WideString, WideString>; {RemotePath -> Content to write}
		FGetFileResults: TDictionary<WideString, Boolean>;       {RemotePath -> Success/Failure}
		FPutFileResults: TDictionary<WideString, Boolean>;       {RemotePath -> Success/Failure}
		FDeleteFileResults: TDictionary<WideString, Boolean>;    {RemotePath -> Success/Failure}
		FDefaultGetResult: Boolean;
		FDefaultPutResult: Boolean;
		FDefaultDeleteResult: Boolean;
	public
		constructor Create;
		destructor Destroy; override;

		{Configure responses}
		procedure SetGetFileResponse(const RemotePath, Content: WideString; Success: Boolean = True);
		procedure SetGetFileResult(const RemotePath: WideString; Success: Boolean);
		procedure SetPutFileResult(const RemotePath: WideString; Success: Boolean);
		procedure SetDeleteFileResult(const RemotePath: WideString; Success: Boolean);
		procedure SetDefaultResults(GetResult, PutResult, DeleteResult: Boolean);
		procedure ClearAll;

		{Inspection}
		function GetOperationCount: Integer;
		function GetOperation(Index: Integer): TCloudOperation;
		function WasOperationPerformed(const Operation, RemotePath: WideString): Boolean;
		function GetOperationsForPath(const RemotePath: WideString): TArray<TCloudOperation>;
		function GetLastOperation: TCloudOperation;

		{ICloudDescriptionOps implementation}
		function GetDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;
		function PutDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;
		function DeleteFile(const Path: WideString): Boolean;
	end;

implementation

{TMockCloudDescriptionOps}

constructor TMockCloudDescriptionOps.Create;
begin
	inherited Create;
	FOperations := TList<TCloudOperation>.Create;
	FGetFileResponses := TDictionary<WideString, WideString>.Create;
	FGetFileResults := TDictionary<WideString, Boolean>.Create;
	FPutFileResults := TDictionary<WideString, Boolean>.Create;
	FDeleteFileResults := TDictionary<WideString, Boolean>.Create;
	FDefaultGetResult := False;
	FDefaultPutResult := True;
	FDefaultDeleteResult := True;
end;

destructor TMockCloudDescriptionOps.Destroy;
begin
	FDeleteFileResults.Free;
	FPutFileResults.Free;
	FGetFileResults.Free;
	FGetFileResponses.Free;
	FOperations.Free;
	inherited;
end;

procedure TMockCloudDescriptionOps.SetGetFileResponse(const RemotePath, Content: WideString; Success: Boolean);
begin
	FGetFileResponses.AddOrSetValue(RemotePath, Content);
	FGetFileResults.AddOrSetValue(RemotePath, Success);
end;

procedure TMockCloudDescriptionOps.SetGetFileResult(const RemotePath: WideString; Success: Boolean);
begin
	FGetFileResults.AddOrSetValue(RemotePath, Success);
end;

procedure TMockCloudDescriptionOps.SetPutFileResult(const RemotePath: WideString; Success: Boolean);
begin
	FPutFileResults.AddOrSetValue(RemotePath, Success);
end;

procedure TMockCloudDescriptionOps.SetDeleteFileResult(const RemotePath: WideString; Success: Boolean);
begin
	FDeleteFileResults.AddOrSetValue(RemotePath, Success);
end;

procedure TMockCloudDescriptionOps.SetDefaultResults(GetResult, PutResult, DeleteResult: Boolean);
begin
	FDefaultGetResult := GetResult;
	FDefaultPutResult := PutResult;
	FDefaultDeleteResult := DeleteResult;
end;

procedure TMockCloudDescriptionOps.ClearAll;
begin
	FOperations.Clear;
	FGetFileResponses.Clear;
	FGetFileResults.Clear;
	FPutFileResults.Clear;
	FDeleteFileResults.Clear;
end;

function TMockCloudDescriptionOps.GetOperationCount: Integer;
begin
	Result := FOperations.Count;
end;

function TMockCloudDescriptionOps.GetOperation(Index: Integer): TCloudOperation;
begin
	if (Index >= 0) and (Index < FOperations.Count) then
		Result := FOperations[Index]
	else
		Result := Default(TCloudOperation);
end;

function TMockCloudDescriptionOps.WasOperationPerformed(const Operation, RemotePath: WideString): Boolean;
var
	Op: TCloudOperation;
begin
	for Op in FOperations do
		if (Op.Operation = Operation) and (Pos(RemotePath, Op.RemotePath) > 0) then
			Exit(True);
	Result := False;
end;

function TMockCloudDescriptionOps.GetOperationsForPath(const RemotePath: WideString): TArray<TCloudOperation>;
var
	Op: TCloudOperation;
	Results: TList<TCloudOperation>;
begin
	Results := TList<TCloudOperation>.Create;
	try
		for Op in FOperations do
			if Pos(RemotePath, Op.RemotePath) > 0 then
				Results.Add(Op);
		Result := Results.ToArray;
	finally
		Results.Free;
	end;
end;

function TMockCloudDescriptionOps.GetLastOperation: TCloudOperation;
begin
	if FOperations.Count > 0 then
		Result := FOperations[FOperations.Count - 1]
	else
		Result := Default(TCloudOperation);
end;

function TMockCloudDescriptionOps.GetDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;
var
	Op: TCloudOperation;
	Content: WideString;
	Stream: TStreamWriter;
begin
	Op.Operation := 'GET';
	Op.RemotePath := RemotePath;
	Op.LocalPath := LocalCopy;
	FOperations.Add(Op);

	{Check for configured result}
	if not FGetFileResults.TryGetValue(RemotePath, Result) then
		Result := FDefaultGetResult;

	{If success and content configured, write to local path}
	if Result and FGetFileResponses.TryGetValue(RemotePath, Content) then
	begin
		Stream := TStreamWriter.Create(LocalCopy, False, TEncoding.UTF8);
		try
			Stream.Write(Content);
		finally
			Stream.Free;
		end;
	end;
end;

function TMockCloudDescriptionOps.PutDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;
var
	Op: TCloudOperation;
begin
	Op.Operation := 'PUT';
	Op.RemotePath := RemotePath;
	Op.LocalPath := LocalCopy;
	FOperations.Add(Op);

	if not FPutFileResults.TryGetValue(RemotePath, Result) then
		Result := FDefaultPutResult;
end;

function TMockCloudDescriptionOps.DeleteFile(const Path: WideString): Boolean;
var
	Op: TCloudOperation;
begin
	Op.Operation := 'DELETE';
	Op.RemotePath := Path;
	Op.LocalPath := '';
	FOperations.Add(Op);

	if not FDeleteFileResults.TryGetValue(Path, Result) then
		Result := FDefaultDeleteResult;
end;

end.

unit ICloudDescriptionOpsInterface;

{Interface for cloud operations needed by description synchronization.
 Abstracts TCloudMailRu dependency to enable unit testing.}

interface

type
	ICloudDescriptionOps = interface
		['{DBE14B0E-4CEA-44E1-9D3E-21F2CDF65068}']

		{Download remote description file to local path.
		 @param RemotePath Full path to description file on cloud
		 @param LocalCopy Local path to save downloaded content
		 @return True if file downloaded, False if not exists or error}
		function GetDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;

		{Upload local description file to remote path.
		 If local file doesn't exist, deletes remote file.
		 @param RemotePath Full path for description file on cloud
		 @param LocalCopy Local path to upload from
		 @return True on success}
		function PutDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;

		{Delete file from cloud.
		 @param Path Full path to file on cloud
		 @return True on success}
		function DeleteFile(const Path: WideString): Boolean;
	end;

implementation

end.

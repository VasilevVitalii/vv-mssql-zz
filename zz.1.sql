IF NOT EXISTS (SELECT * FROM INFORMATION_SCHEMA.SCHEMATA AS s WHERE s.[SCHEMA_NAME] = 'zz') BEGIN
	EXEC ('CREATE SCHEMA [zz]')
END
GO

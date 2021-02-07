SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

--=======================================================
--SCALAR FUNCTIONS
--=======================================================


CREATE OR ALTER FUNCTION [zz].[StrIsInt](@Value NVARCHAR(MAX))
--Check a string - is it INT
--The function never returns NULL
--If NULL is applied to the input, the output is 1
--Examples:
-- SELECT zz.StrIsInt(NULL)			-- return 1
-- SELECT zz.StrIsInt('0')			-- return 1
-- SELECT zz.StrIsInt('5')			-- return 1
-- SELECT zz.StrIsInt('-5.00')		-- return 1
-- SELECT zz.StrIsInt('5,0')		-- return 1
-- SELECT zz.StrIsInt('5.00')		-- return 1
-- SELECT zz.StrIsInt('-5,0000')	-- return 1
RETURNS BIT
AS
BEGIN
	IF @Value IS NULL RETURN 1
	DECLARE @len INT; SET @Len=LEN(@Value);
	IF @len=1 BEGIN
		IF @Value LIKE '[0-9]' RETURN 1
		RETURN 0                 	
	END
	IF SUBSTRING(@Value,1,1)='-' BEGIN 
		SET @Value=RIGHT(@Value,@len-1)
		SET @len=@len-1
	END	
	IF @Value LIKE '%,%' OR @Value LIKE '%.%' BEGIN
		SET @Value=REVERSE(@Value)
		DECLARE @point INT
		SET @point=PATINDEX('%[,.]%',@Value)-1
		IF @point<>0 BEGIN
			IF SUBSTRING(@Value,1,@point) LIKE '%[^0]%' RETURN 0	
		END
		SET @Value=LEFT(REVERSE(@Value),@len-@point-1)  
	END
	IF @Value LIKE '%[^0-9]%' RETURN 0
	RETURN 1
END
GO

CREATE OR ALTER FUNCTION [zz].[StrIsMoney](@Value NVARCHAR(MAX))
-- Checking a line - is it MONEY
--The function never returns NULL
--If NULL is applied to the input, the output is 1
--Examples:
-- SELECT zz.StrIsMoney(NULL)			-- return 1
-- SELECT zz.StrIsMoney(0.0001)			-- return 1
-- SELECT zz.StrIsMoney(-0.0001)		-- return 1
-- SELECT zz.StrIsMoney(0,0001)			-- return 1
-- SELECT zz.StrIsMoney(-0,0001)		-- return 1
-- SELECT zz.StrIsMoney(22.099)			-- return 1
-- SELECT zz.StrIsMoney(-22,099)		-- return 1
-- SELECT zz.StrIsMoney(0.00009)		-- return 0
RETURNS BIT
AS
BEGIN
	IF @Value IS NULL RETURN 1
	DECLARE @len INT; SET @Len=LEN(@Value);
	IF @len=1 BEGIN
		IF @Value LIKE '[0-9]' RETURN 1
		RETURN 0                 	
	END
	IF SUBSTRING(@Value,1,1)='-' BEGIN 
		SET @Value=RIGHT(@Value,@len-1)
		SET @len=@len-1
	END	
	IF @Value LIKE '%,%' OR @Value LIKE '%.%' BEGIN
		SET @Value=REVERSE(@Value)
		DECLARE @point INT
		SET @point=PATINDEX('%[,.]%',@Value)-1
		IF @point=0 BEGIN
			SET @Value=LEFT(REVERSE(@Value),@len-1) 
		END ELSE IF @point<=4 BEGIN 
			IF LEFT(@Value,@point) LIKE '%[^0-9]%' RETURN 0	      	
		END ELSE BEGIN
			IF LEFT(@Value,@point) LIKE '%[^0-9]%' RETURN 0
			IF LEFT(@Value,@point-4) LIKE '%[^0]%' RETURN 0
		END
		SET @Value=LEFT(REVERSE(@Value),@len-@point-1)  
	END
	IF @Value LIKE '%[^0-9]%' RETURN 0
	RETURN 1
END
GO

CREATE OR ALTER [zz].[StrIsDate](@Value NVARCHAR(MAX))
--Checking a string - is it DATETIME
--The function never returns NULL
--If NULL is applied to the input, the output is 1
-- Understands dates in formats:
--		dd.mm.yy
--		yyyymmdd
--		dd.mm.yyyy
--		dd.mm.yyyy hh:mm
--		dd.mm.yyyy hh:mm:ss
--		yyyy-mm-ddThh:mm:ss 
--		yyyy-mm-ddThh:mm:ss.mmm
--		dd.mm.yyyy hh:mm:ss.mmm
RETURNS BIT
AS
BEGIN
	IF @Value IS NULL RETURN 1
	DECLARE @len INT; SET @Len=LEN(@Value);
	DECLARE @DD126 VARCHAR(23)

	IF @Len=8 BEGIN
		IF SUBSTRING(@Value,3,1)='.' AND SUBSTRING(@Value,6,1)='.' BEGIN
			--dd.mm.yy
			DECLARE @fndYear VARCHAR(4)
			SET @fndYear=SUBSTRING(@Value,7,2)
			--20 век: 51 - 99, 21 век: 00 - 50
			IF (LEFT(@fndYear,1)='5' AND RIGHT(@fndYear,1)<>'0') 
			OR (LEFT(@fndYear,1) IN ('6','7','8','9'))
			BEGIN
				SET @fndYear='19'+@fndYear
			END ELSE BEGIN
				SET @fndYear='20'+@fndYear     	
			END
			SET @DD126=@fndYear +'-'+SUBSTRING(@Value,4,2)+'-'+LEFT(@Value,2)+'T00:00:00.000'		
		END ELSE BEGIN
			--yyyymmdd
			SET @DD126=LEFT(@Value,4)+'-'+SUBSTRING(@Value,5,2)+'-'+SUBSTRING(@Value,7,2)+'T00:00:00.000'         	
		END 
	END ELSE IF @Len=10 BEGIN
		--dd.mm.yyyy
		IF SUBSTRING(@Value,3,1)='.' AND SUBSTRING(@Value,6,1)='.' BEGIN
			SET @DD126=SUBSTRING(@Value,7,4)+'-'+SUBSTRING(@Value,4,2)+'-'+LEFT(@Value,2)+'T00:00:00.000'	
		END
	END ELSE IF @Len=16 BEGIN
		--dd.mm.yyyy hh:mm
		IF SUBSTRING(@Value,3,1)='.' AND SUBSTRING(@Value,6,1)='.' AND SUBSTRING(@Value,11,1)=' ' AND SUBSTRING(@Value,14,1)=':' BEGIN
    		SET @DD126=SUBSTRING(@Value,7,4)+'-'+SUBSTRING(@Value,4,2)+'-'+LEFT(@Value,2)+'T'+SUBSTRING(@Value,12,2)+':'+SUBSTRING(@Value,15,2)+':00.000'
		END	     	
	END ELSE IF @Len=19 BEGIN
		IF SUBSTRING(@Value,3,1)='.' AND SUBSTRING(@Value,6,1)='.' AND SUBSTRING(@Value,11,1)=' ' AND SUBSTRING(@Value,14,1)=':' AND SUBSTRING(@Value,17,1)=':' BEGIN
			--dd.mm.yyyy hh:mm:ss
    		SET @DD126=SUBSTRING(@Value,7,4)+'-'+SUBSTRING(@Value,4,2)+'-'+LEFT(@Value,2)+'T'+SUBSTRING(@Value,12,2)+':'+SUBSTRING(@Value,15,2)+':'+SUBSTRING(@Value,18,2)+'.000'
		END	ELSE IF SUBSTRING(@Value,5,1)='-' AND SUBSTRING(@Value,8,1)='-' AND SUBSTRING(@Value,11,1)='T' AND SUBSTRING(@Value,14,1)=':' AND SUBSTRING(@Value,17,1)=':' BEGIN
			--yyyy-mm-ddThh:mm:ss 
		   	SET @DD126=@Value+'.000'
		END     	
	END	ELSE IF @Len=23 BEGIN
		IF SUBSTRING(@Value,5,1)='-' AND SUBSTRING(@Value,8,1)='-' AND SUBSTRING(@Value,11,1)='T' AND SUBSTRING(@Value,14,1)=':' AND SUBSTRING(@Value,17,1)=':' AND SUBSTRING(@Value,20,1)='.' BEGIN
			--yyyy-mm-ddThh:mm:ss.mmm
			SET @DD126=@Value
		END	ELSE IF SUBSTRING(@Value,3,1)='.' AND SUBSTRING(@Value,6,1)='.' AND SUBSTRING(@Value,11,1)=' ' AND SUBSTRING(@Value,14,1)=':' AND SUBSTRING(@Value,17,1)=':' AND SUBSTRING(@Value,20,1)='.' BEGIN
			--dd.mm.yyyy hh:mm:ss.mmm
		   	SET @DD126=SUBSTRING(@Value,7,4)+'-'+SUBSTRING(@Value,4,2)+'-'+LEFT(@Value,2)+'T'+SUBSTRING(@Value,12,2)+':'+SUBSTRING(@Value,15,2)+':'+SUBSTRING(@Value,18,2)+'.'+SUBSTRING(@Value,21,3)
		END 
	END	

	IF @DD126 IS NULL RETURN 0
	RETURN ISDATE(@DD126)
END
GO

CREATE OR ALTER FUNCTION [zz].[StrIsGuid](@Value NVARCHAR(MAX))
-- Checking the string - is it a GUID
--The function never returns NULL
--If NULL is applied to the input, the output is 1
RETURNS BIT
AS
BEGIN
	IF @Value IS NULL RETURN 1
	IF LEN(@Value)<>36 RETURN 0
	IF @Value NOT LIKE '[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]-[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]' RETURN 0
	RETURN 1
END
GO

CREATE OR ALTER FUNCTION [zz].[Gull](@Value UNIQUEIDENTIFIER)
RETURNS UNIQUEIDENTIFIER
AS
BEGIN
	IF @Value IS NULL RETURN NULL
	IF @Value='00000000-0000-0000-0000-000000000000' RETURN NULL
	RETURN @Value
END
GO

CREATE OR ALTER FUNCTION [zz].[StrToInt](@Value NVARCHAR(MAX))
--Converts string to INT
--If the string is not INT, it will be NULL
--What is considered INT and what is not - see function zz.StrIsInt
RETURNS INT
AS
BEGIN
	IF @Value IS NULL RETURN NULL 
	IF zz.StrIsInt(@Value)=0 RETURN NULL 
	IF @Value LIKE '%,%' OR @Value LIKE '%.%' BEGIN
		SET @Value=REVERSE(@Value)
		DECLARE @point INT
		SET @point=PATINDEX('%[,.]%',@Value)
		SET @Value=REVERSE(@Value)
		SET @Value=LEFT(@Value,LEN(@Value)-@point)
		RETURN CONVERT(INT,@Value)
	END	
	RETURN CONVERT(INT,@Value)
END
GO

CREATE OR ALTER FUNCTION [zz].[StrToMoney](@Value NVARCHAR(MAX))
--Converts a string to MONEY
--If the string is not MONEY, it will be NULL
--What is MONEY and what is not - see zz.StrIsMoney function
RETURNS MONEY
AS
BEGIN
	IF @Value IS NULL RETURN NULL 
	IF zz.StrIsMoney(@Value)=0 RETURN NULL
	SET @Value=REPLACE(@Value,',','.') 
	RETURN CONVERT(MONEY,@Value)
END
GO

CREATE OR ALTER FUNCTION [zz].[StrToDate](@Value NVARCHAR(MAX))
--Converts string to DATETIME
--If the string is not DATETIME, it will be NULL
--What is considered DATETIME and what is not - see function zz.StrIsDate
RETURNS DATETIME
AS
BEGIN
	IF @Value IS NULL RETURN NULL 

	DECLARE @len INT; SET @Len=LEN(@Value);
	DECLARE @DD126 VARCHAR(23)

	IF @Len=8 BEGIN
		IF SUBSTRING(@Value,3,1)='.' AND SUBSTRING(@Value,6,1)='.' BEGIN
			--dd.mm.yy
			DECLARE @fndYear VARCHAR(4)
			SET @fndYear=SUBSTRING(@Value,7,2)
			--20 century: 51 - 99, 21 century: 00 - 50
			IF (LEFT(@fndYear,1)='5' AND RIGHT(@fndYear,1)<>'0') 
			OR (LEFT(@fndYear,1) IN ('6','7','8','9'))
			BEGIN
				SET @fndYear='19'+@fndYear
			END ELSE BEGIN
				SET @fndYear='20'+@fndYear     	
			END
			SET @DD126=@fndYear +'-'+SUBSTRING(@Value,4,2)+'-'+LEFT(@Value,2)+'T00:00:00.000'		
		END ELSE BEGIN
			--yyyymmdd
			SET @DD126=LEFT(@Value,4)+'-'+SUBSTRING(@Value,5,2)+'-'+SUBSTRING(@Value,7,2)+'T00:00:00.000'         	
		END 
	END ELSE IF @Len=10 BEGIN
		--dd.mm.yyyy
		IF SUBSTRING(@Value,3,1)='.' AND SUBSTRING(@Value,6,1)='.' BEGIN
			SET @DD126=SUBSTRING(@Value,7,4)+'-'+SUBSTRING(@Value,4,2)+'-'+LEFT(@Value,2)+'T00:00:00.000'	
		END
	END ELSE IF @Len=16 BEGIN
		--dd.mm.yyyy hh:mm
		IF SUBSTRING(@Value,3,1)='.' AND SUBSTRING(@Value,6,1)='.' AND SUBSTRING(@Value,11,1)=' ' AND SUBSTRING(@Value,14,1)=':' BEGIN
    		SET @DD126=SUBSTRING(@Value,7,4)+'-'+SUBSTRING(@Value,4,2)+'-'+LEFT(@Value,2)+'T'+SUBSTRING(@Value,12,2)+':'+SUBSTRING(@Value,15,2)+':00.000'
		END	     	
	END ELSE IF @Len=19 BEGIN
		IF SUBSTRING(@Value,3,1)='.' AND SUBSTRING(@Value,6,1)='.' AND SUBSTRING(@Value,11,1)=' ' AND SUBSTRING(@Value,14,1)=':' AND SUBSTRING(@Value,17,1)=':' BEGIN
			--dd.mm.yyyy hh:mm:ss
    		SET @DD126=SUBSTRING(@Value,7,4)+'-'+SUBSTRING(@Value,4,2)+'-'+LEFT(@Value,2)+'T'+SUBSTRING(@Value,12,2)+':'+SUBSTRING(@Value,15,2)+':'+SUBSTRING(@Value,18,2)+'.000'
		END ELSE IF SUBSTRING(@Value,5,1)='-' AND SUBSTRING(@Value,8,1)='-' AND SUBSTRING(@Value,11,1)='T' AND SUBSTRING(@Value,14,1)=':' AND SUBSTRING(@Value,17,1)=':' BEGIN 
			--yyyy-mm-ddThh:mm:ss 
		   	SET @DD126=@Value+'.000'	
		END 
	END	ELSE IF @Len=23 BEGIN
		IF SUBSTRING(@Value,5,1)='-' AND SUBSTRING(@Value,8,1)='-' AND SUBSTRING(@Value,11,1)='T' AND SUBSTRING(@Value,14,1)=':' AND SUBSTRING(@Value,17,1)=':' AND SUBSTRING(@Value,20,1)='.' BEGIN
			--yyyy-mm-ddThh:mm:ss.mmm
			SET @DD126=@Value
		END	ELSE IF SUBSTRING(@Value,3,1)='.' AND SUBSTRING(@Value,6,1)='.' AND SUBSTRING(@Value,11,1)=' ' AND SUBSTRING(@Value,14,1)=':' AND SUBSTRING(@Value,17,1)=':' AND SUBSTRING(@Value,20,1)='.' BEGIN
			--dd.mm.yyyy hh:mm:ss.mmm
		   	SET @DD126=SUBSTRING(@Value,7,4)+'-'+SUBSTRING(@Value,4,2)+'-'+LEFT(@Value,2)+'T'+SUBSTRING(@Value,12,2)+':'+SUBSTRING(@Value,15,2)+':'+SUBSTRING(@Value,18,2)+'.'+SUBSTRING(@Value,21,3)
		END 
	END	

	IF @DD126 IS NULL RETURN NULL
	IF ISDATE(@DD126)<>1 RETURN NULL
	 
	RETURN CONVERT(DATETIME,@DD126)
END
GO
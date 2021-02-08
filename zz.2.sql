SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

--=======================================================
--=======================================================
--SCALAR FUNCTIONS
--=======================================================
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
--Checking a string - is it MONEY
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

CREATE OR ALTER FUNCTION [zz].[StrIsDate](@Value NVARCHAR(MAX))
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
--Checking the string - is it a GUID
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

CREATE OR ALTER FUNCTION [zz].[StrToGuid](@Value NVARCHAR(MAX))
--Converts string to GUID
--If there is no GUID in the string, it will be NULL
RETURNS UNIQUEIDENTIFIER
AS
BEGIN
	IF @Value IS NULL RETURN NULL
	IF LEN(@Value)<>36 RETURN NULL
	IF @Value NOT LIKE '[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]-[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]' RETURN NULL
	RETURN CONVERT(UNIQUEIDENTIFIER,@Value)
	RETURN NULL
END
GO

CREATE OR ALTER FUNCTION [zz].[StrFromInt](@Value INT,@Format VARCHAR(32))
--Converts INT to a string matching the specified pattern
--Examples:
--  SELECT [zz].[StrFromInt](5,'n')				--return '5'
--  SELECT [zz].[StrFromInt](5,'n.2')			--return '5.00'
--  SELECT [zz].[StrFromInt](5,'n.4')			--return '5.0000'
--  SELECT [zz].[StrFromInt](5,'n,2')			--return '5,00'
--  SELECT [zz].[StrFromInt](5,'n,4')			--return '5,0000'
--  SELECT [zz].[StrFromInt](NULL,'n,4')		--return NULL
RETURNS NVARCHAR(MAX)
AS
BEGIN
	IF @Value IS NULL RETURN NULL
	IF @Format ='n' BEGIN
		RETURN CONVERT(NVARCHAR(MAX),@Value)
	END ELSE IF @Format ='n.2' BEGIN
		RETURN CONVERT(NVARCHAR(MAX),@Value)+'.00'
	END ELSE IF @Format ='n,2' BEGIN
		RETURN CONVERT(NVARCHAR(MAX),@Value)+',00'
	END ELSE IF @Format ='n.4' BEGIN
		RETURN CONVERT(NVARCHAR(MAX),@Value)+'.0000'
	END ELSE IF @Format ='n,4' BEGIN
		RETURN CONVERT(NVARCHAR(MAX),@Value)+',0000'
	END
	RETURN NULL
END
GO

CREATE OR ALTER FUNCTION [zz].[StrFromMoney](@Value MONEY,@Format VARCHAR(32))
--Converts MONEY to a string matching the specified pattern
--If the formats 'n.2' or 'n, 2' are selected, it will also round the returned result
--Examples:
--  SELECT [zz].[StrFromMoney](22.099,'n.n')				--return '22.099'
--  SELECT [zz].[StrFromMoney](22.099,'n,n')				--return '22,099'
--  SELECT [zz].[StrFromMoney](22.099,'n.2')				--return '22.10'
--  SELECT [zz].[StrFromMoney](22.099,'n,2')				--return '22,10'
--  SELECT [zz].[StrFromMoney](22.099,'n.4')				--return '22.0990'
--  SELECT [zz].[StrFromMoney](22.099,'n,4')				--return '22,0990'
RETURNS NVARCHAR(MAX)
AS
BEGIN
	IF @Value IS NULL RETURN NULL
	
	IF @Format ='n.n' BEGIN
		DECLARE @r1 NVARCHAR(MAX)=CONVERT(NVARCHAR(MAX),@Value,2)
		WHILE RIGHT(@r1,1)='0' BEGIN
			SET @r1=LEFT(@r1,LEN(@r1)-1) 
		END
		IF RIGHT(@r1,1)='.' SET @r1=LEFT(@r1,LEN(@r1)-1) 
		RETURN @r1
	END IF @Format ='n,n' BEGIN
		DECLARE @r2 NVARCHAR(MAX)=CONVERT(NVARCHAR(MAX),@Value,2)
		WHILE RIGHT(@r2,1)='0' BEGIN
			SET @r2=LEFT(@r2,LEN(@r2)-1) 
		END
		IF RIGHT(@r2,1)='.' SET @r2=LEFT(@r2,LEN(@r2)-1) 
		RETURN REPLACE(@r2,'.',',')
	END ELSE  IF @Format ='n.2' BEGIN
		RETURN CONVERT(NVARCHAR(MAX),@Value,0)
	END ELSE  IF @Format ='n,2' BEGIN
		DECLARE @r3 NVARCHAR(MAX)=CONVERT(NVARCHAR(MAX),@Value,0)
		RETURN REPLACE(@r3,'.',',')
	END ELSE  IF @Format ='n.4' BEGIN
		RETURN CONVERT(NVARCHAR(MAX),@Value,2)
	END ELSE  IF @Format ='n,4' BEGIN
		DECLARE @r4 NVARCHAR(MAX)=CONVERT(NVARCHAR(MAX),@Value,2)
		RETURN REPLACE(@r4,'.',',')
	END
	RETURN NULL
END
GO

CREATE OR ALTER FUNCTION [zz].[StrFromDate](@Value DATETIME,@Format VARCHAR(32))
--Converts a DATETIME to a string matching the specified pattern
--Almost all templates have a short analogy, for example, [zz]. [StrFromDate] (GETDATE (), 'yyyymmdd') = [zz]. [StrFromDate] (GETDATE (), 'ds')
-- Templates (+ analogy):
--		'yyyymmdd'						'ds'
--		'dd.mm.yyyy'					'd'
--		'dd.mm.yyyy hh:mm'				'dm'
--		'dd.mm.yyyy hh:mm:ss'			'dms'
--		'dd.mm.yyyy hh:mm:ss.mmm'		'dmsm'
--		'dd.mm.yy'
--Attention!! The features of DATETIME are such that some milliseconds are lost:
--zz.StrFromDate ('2015-01-19T14: 03: 35.787', 'dmsm') - milliseconds will return correctly (787)
--zz.StrFromDate ('2015-01-19T14: 03: 35.789', 'dmsm') - milliseconds will return NOT correctly (790)RETURNS NVARCHAR(MAX)
RETURNS NVARCHAR(MAX)
AS
BEGIN
	IF @Value IS NULL RETURN NULL
	
	IF @Format IN ('ds','yyyymmdd') BEGIN
		RETURN CONVERT(NVARCHAR(MAX),@Value,112)
	END IF @Format IN ('d','dd.mm.yyyy') BEGIN
		RETURN CONVERT(NVARCHAR(MAX),@Value,104)
	END ELSE  IF @Format IN ('dm','dd.mm.yyyy hh:mm') BEGIN
		RETURN CONVERT(NVARCHAR(MAX),@Value,104)+' '+LEFT(CONVERT(NVARCHAR(MAX),@Value,108),5)
	END ELSE  IF @Format IN ('dms','dd.mm.yyyy hh:mm:ss') BEGIN
		RETURN CONVERT(NVARCHAR(MAX),@Value,104)+' '+CONVERT(NVARCHAR(MAX),@Value,108)
	END ELSE  IF @Format IN ('dmsm','dd.mm.yyyy hh:mm:ss.mmm') BEGIN
		RETURN CONVERT(NVARCHAR(MAX),@Value,104)+' '+CONVERT(NVARCHAR(MAX),@Value,108)+'.'+RIGHT(CONVERT(NVARCHAR,@Value,114),3)
	END ELSE  IF @Format ='dd.mm.yy' BEGIN
		RETURN CONVERT(NVARCHAR(MAX),@Value,4)
	END
	RETURN NULL
END
GO

CREATE OR ALTER FUNCTION [zz].[KillTime](@Value DATETIME)
--Crop time from DATETIME
RETURNS DATETIME
AS
BEGIN
	IF @Value IS NULL RETURN NULL
	RETURN CONVERT(DATETIME,CONVERT(NVARCHAR,@Value,112))
END
GO

CREATE OR ALTER FUNCTION [zz].[StrToMsg](@From NVARCHAR(MAX),@Value0 NVARCHAR(MAX)=NULL,@Value1 NVARCHAR(MAX)=NULL,@Value2 NVARCHAR(MAX)=NULL,@Value3 NVARCHAR(MAX)=NULL,@Value4 NVARCHAR(MAX)=NULL,@Value5 NVARCHAR(MAX)=NULL,@Value6 NVARCHAR(MAX)=NULL,@Value7 NVARCHAR(MAX)=NULL,@Value8 NVARCHAR(MAX)=NULL,@Value9 NVARCHAR(MAX)=NULL)
--A function that generates a message from a string template and a set of substitutions for it.
--Not sharpened for use by colleagues, needed for use in other internal mechanisms.
--If a substitution is expected, and it is = NULL, then the message will say <EMPTY> in its place, example
--SELECT zz.StrToMsg ('Empty value example - {a}', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)
--If an incorrect substitution is specified (for example, a date is expected but not a date has arrived, the substitution will not work, for example
--SELECT zz.StrToMsg ('Example of incorrect date substitution - {d}', 'aaa', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)

--Examples of substitution formats:
--one. {a} the function will itself determine what kind of data is in the substitution, example
-- DECLARE @d DATETIME = GETDATE ()
-- SELECT zz.StrToMsg ('Here int - {a}, here money - {a}, here date - {a}, here string {a}', 45,78.99, @ d, 'string !!', NULL , NULL, NULL, NULL, NULL, NULL)
--2. {n}, {n.n}, {n, n} {n.2} {n, 2} {n.4} {n, 4} number format (see functions zz.StrFromInt, zz.StrFromMoney).
--			Important! If the format is {n}, and a number of the money type came in, the substitution will not work.
--3. {ds}, {d}, {dm}, {dms}, {dmsm} date format (see function zz.StrFromDate).
--			Important! If we immediately pass a variable of the datetime type to the substitution (relying on an implicit SQL conversion),
-- then everything below the minutes will be cut off - this is how this transformation behaves, for example:
-- DECLARE @d DATETIME = GETDATE ()
-- SELECT zz.StrToMsg ('Example of different date formats with SQL conversion: ds = {ds}, d = {d}, dms = {dms}, dmsm = {dmsm}', @ d, @ d, @ d, @ d, NULL, NULL, NULL, NULL, NULL, NULL)
-- DECLARE @n NVARCHAR (MAX) = CONVERT (NVARCHAR (MAX), @ d, 126)
-- SELECT zz.StrToMsg ('An example of different date formats with their own conversion: ds = {ds}, d = {d}, dms = {dms}, dmsm = {dmsm}', @ n, @ n, @ n, @ n, NULL, NULL, NULL, NULL, NULL, NULL)
--4. {s} arbitrary string
RETURNS NVARCHAR(MAX)
AS
BEGIN
	DECLARE @To NVARCHAR(MAX)=@From

	DECLARE @IndexL INT=0, @IndexR INT=0, @currentStep INT=0, @currentValue NVARCHAR(MAX)
	DECLARE @findLexem NVARCHAR(MAX), @replaceLexem NVARCHAR(MAX)

	again:
	SET @currentStep=@currentStep+1
	SET @IndexL=CHARINDEX('{',@To,@IndexR)

	IF @IndexL>0 AND @currentStep<=10 BEGIN
		IF @currentStep=1 BEGIN
			SET @currentValue=@Value0
		END ELSE IF @currentStep=2 BEGIN
			SET @currentValue=@Value1
		END ELSE IF @currentStep=3 BEGIN
			SET @currentValue=@Value2
		END ELSE IF @currentStep=4 BEGIN
			SET @currentValue=@Value3
		END ELSE IF @currentStep=5 BEGIN
			SET @currentValue=@Value4
		END ELSE IF @currentStep=6 BEGIN
			SET @currentValue=@Value5
		END ELSE IF @currentStep=7 BEGIN
			SET @currentValue=@Value6
		END ELSE IF @currentStep=8 BEGIN
			SET @currentValue=@Value7
		END ELSE IF @currentStep=9 BEGIN
			SET @currentValue=@Value8
		END ELSE IF @currentStep=10 BEGIN
			SET @currentValue=@Value9
		END ELSE BEGIN
			SET @currentValue=NULL
		END

		SET @IndexR=CHARINDEX('}',@To,@IndexL)
		IF @IndexR>0 BEGIN
			SET @findLexem=LOWER(SUBSTRING(@To,@IndexL,@IndexR-@IndexL+1))
			SET @replaceLexem=null

			IF @findLexem='{a}' BEGIN
				IF @currentValue is null BEGIN
					SET @replaceLexem='<EMPTY>'
				END ELSE IF @currentValue='' BEGIN
					SET @replaceLexem=''
				END ELSE IF zz.StrIsInt(@currentValue)=1 BEGIN
					SET @replaceLexem=zz.StrFromInt(zz.StrToInt(@currentValue),'n')
				END ELSE IF zz.StrIsMoney(@currentValue)=1 BEGIN
					SET @replaceLexem=zz.StrFromMoney(zz.StrToMoney(@currentValue),'n.n')
				END ELSE IF zz.StrIsDate(@currentValue)=1 BEGIN
					SET @replaceLexem=zz.StrFromDate(zz.StrToDate(@currentValue),'d')
				END ELSE IF ISDATE(@currentValue)=1 BEGIN
					SET @replaceLexem=zz.StrFromDate(@currentValue,'d')	
				END ELSE BEGIN
					SET @replaceLexem=@currentValue	
				END
			END ELSE IF @findLexem IN ('{s}') BEGIN
				IF @currentValue IS NULL BEGIN
					SET @replaceLexem='<EMPTY>'
				END ELSE begin
					SET @replaceLexem=@currentValue
				END
			END ELSE IF @findLexem IN ('{n}') BEGIN
				IF @currentValue IS NULL BEGIN
					SET @replaceLexem='<EMPTY>'
				END ELSE BEGIN
					IF zz.StrIsInt(@currentValue)=1 BEGIN
						set @replaceLexem=zz.StrFromInt(zz.StrToInt(@currentValue),'n')
					END
				end
			END ELSE IF @findLexem IN ('{n.n}','{n,n}','{n.2}','{n,2}','{n.4}','{n,4}') BEGIN
				IF @currentValue IS NULL BEGIN
					SET @replaceLexem='<EMPTY>'
				END ELSE BEGIN			
					IF zz.StrIsMoney(@currentValue)=1 BEGIN
						SET @replaceLexem=zz.StrFromMoney(zz.StrToMoney(@currentValue),SUBSTRING(@findLexem,2,LEN(@findLexem)-2))
					END	
				END
			END ELSE IF @findLexem IN ('{ds}','{d}','{dm}','{dms}','{dmsm}') BEGIN
				IF @currentValue IS NULL BEGIN
					SET @replaceLexem='<EMPTY>'
				END ELSE BEGIN
					IF zz.StrIsDate(@currentValue)=1 BEGIN
						SET @replaceLexem=zz.StrFromDate(zz.StrToDate(@currentValue),SUBSTRING(@findLexem,2,LEN(@findLexem)-2))
					END ELSE IF ISDATE(@currentValue)=1 BEGIN
						SET @replaceLexem=zz.StrFromDate(@currentValue,SUBSTRING(@findLexem,2,LEN(@findLexem)-2))	
					END	
				END			
			END

			IF @replaceLexem IS NOT NULL BEGIN
				SET @To=LEFT(@To,@IndexL-1)+@replaceLexem+RIGHT(@To,LEN(@To)-@IndexR)
				SET @indexR=@indexR+(LEN(@replaceLexem)-(@indexR-@indexL)-1)
			END
			GOTO again
		END
	END
	RETURN @To
END
GO
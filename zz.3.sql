SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

--=======================================================
--=======================================================
--TABLE FUNCTIONS
--=======================================================
--=======================================================

CREATE OR ALTER FUNCTION [zz].[StrSplit1](
	 @Value NVARCHAR(MAX)
	,@Separator NVARCHAR(2)=';'
	,@Limit INT=1000
)
--Convert a string (sets as a 'value1;...valueN') into a table with a limit on the number of rows (@Limit parameter) in the resulting table.
--Attention! understanding that there is a separator between parts of lines is influenced not only by the @Separator value, but also by its length:
--the two examples below will return the same result
--SELECT * FROM zz.StrSplit1('lexem1;lexem2;;lexem4',';',3)
--SELECT * FROM zz.StrSplit1('{lexem1}{lexem2}{}{lexem4}','{}',3)
--		Result:
--			[IDD]						[VALUE]
--			  1							 lexem1
--			  2							 lexem2
--			  3							 NULL			-- because there is an empty parameter
--			there will be no line with lexem4, because the limit is 3
--If the function operates in the "@Separator length - one character" mode, then the separator at the end of the line is ignored:
--the two examples below will return the same result
--SELECT * FROM zz.StrSplit1('lexem1,lexem2',',',DEFAULT)
--SELECT * FROM zz.StrSplit1('lexem1,lexem2,',',',DEFAULT)
--		Result:
--			[IDD]						[VALUE]
--			  1							 lexem1
--			  2							 lexem2
RETURNS @ret TABLE (IDD INT, VALUE NVARCHAR(MAX))
AS
BEGIN
	IF @Value IS NULL OR @Separator IS NULL OR @Separator='%' GOTO exx
	DECLARE @Step INT SET @Step=1
	DECLARE @SplitVal NVARCHAR(MAX)

	IF LEN(@Separator)=1 OR @Separator=' ' BEGIN
		IF RIGHT(@Value,1)<>@Separator SET @Value=@Value+@Separator
		DECLARE @SplitIndex INT
		
		WHILE @Limit>=@Step BEGIN 
			SELECT @SplitIndex=PATINDEX('%'+@Separator+'%',@Value)
			SET @SplitIndex=ISNULL(@SplitIndex,0)
			SELECT @SplitVal=CASE WHEN @SplitIndex>0 THEN LEFT(@Value,@SplitIndex-1) ELSE @Value END
			IF ISNULL(LEN(@SplitVal),0)<=0 AND @SplitIndex<=0 GOTO exx
			INSERT INTO @ret(IDD, VALUE) SELECT @Step, NULLIF(@SplitVal,'')
			SET @Value = RIGHT(@Value,DATALENGTH(@Value)/2-@SplitIndex)
			IF @SplitIndex<=0 GOTO exx	
			SET @Step=@Step+1	
		END
	END ELSE IF LEN(@Separator)=2 BEGIN
		DECLARE @SeparatorL NVARCHAR(1)=LEFT(@Separator,1)
		DECLARE @SeparatorR NVARCHAR(1)=RIGHT(@Separator,1)
		DECLARE @SplitIndexL INT, @SplitIndexR INT
		
		WHILE @Limit>=@Step BEGIN 
			SELECT @SplitIndexL=PATINDEX('%'+@SeparatorL+'%'+@SeparatorR+'%',@Value) IF @SplitIndexL<=0 GOTO exx
			SELECT @SplitIndexR=CHARINDEX(@SeparatorR,@Value,@SplitIndexL+1)
			IF @SplitIndexL>=@SplitIndexR GOTO exx
			SELECT @SplitVal=SUBSTRING(@Value,@SplitIndexL+1,@SplitIndexR-@SplitIndexL-1)
			IF @SplitVal IS NOT NULL BEGIN
				INSERT INTO @ret(IDD, [VALUE]) SELECT @Step, NULLIF(@SplitVal,'')
				SET @Value=SUBSTRING(@Value,1,@SplitIndexL-1)+SUBSTRING(@Value,@SplitIndexR+1,LEN(@Value)-@SplitIndexR)						
			END ELSE BEGIN
				GOTO exx
			END	
			SET @Step=@Step+1	
		END	         	
	END
	 
	exx:
	RETURN
END
GO

CREATE OR ALTER FUNCTION [zz].[StrSplit2](
	 @Value NVARCHAR(MAX)
	,@Separator NVARCHAR(2)=';'
	,@Param NVARCHAR(1)='='
	,@Limit INT=1000
)
--Convert a string (sets as a 'variable1=value1;...variableN=valueN') into a table with a limited number of parameters (@Limit parameter) in the resulting table.
--Attention! understanding that there is a separator between parts of lines is influenced not only by the @Separator value, but also by its length (as well as in zz.StrSplit1)
--If the function operates in the "@Separator length - one character" mode, then the separator at the end of the line is ignored (as well as in zz.StrSplit1)
--Only those "variable-value" pairs where the variable is not empty are included in the final table.
--Example:
--SELECT * FROM zz.StrSplit2('var1=val1;=val2;var3=;v4',';','=',DEFAULT)
--will return two linesè 
--			[IDD]			[Variable]			[VALUE]
--			  1				var1				val1
--			  2				var3				NULL
--because in the substring '= val2' there is no variable name, and 'v4' it is generally not clear what it is
RETURNS @ret TABLE (IDD INT, VARIABLE NVARCHAR(MAX), VALUE NVARCHAR(MAX))
AS
BEGIN
	IF @Value IS NULL OR @Separator IS NULL OR @Separator='%' OR @Param IS NULL OR @Param='%' GOTO exx

	INSERT INTO @ret(IDD, VARIABLE, VALUE) 
	SELECT ROW_NUMBER() OVER (ORDER BY s.IDD), s1.[Value], s2.[Value]
	FROM zz.StrSplit1(@Value,@Separator,@Limit) s
	OUTER APPLY (SELECT * FROM zz.StrSplit1(s.[Value],@Param,1) WHERE IDD=1) s1
	OUTER APPLY (SELECT * FROM zz.StrSplit1(s.[Value],@Param,2) WHERE IDD=2) s2
	WHERE s1.[Value] IS NOT NULL AND LEN(s1.[Value])>0	
	exx:
	RETURN
END
GO

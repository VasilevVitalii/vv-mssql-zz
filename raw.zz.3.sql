USE [rrCatena]
GO

/****** Object:  UserDefinedFunction [zz].[StrSplit1]    Script Date: 05.02.2021 16:00:31 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


--#endregion

--#region CREATE FUNCTION [zz].[StrSplit1]
CREATE FUNCTION [zz].[StrSplit1](
	 @Value NVARCHAR(MAX)
	,@Separator NVARCHAR(2)=';'
	,@Limit INT=1000
)
--Парсинг строки в таблицу с ограничением кол-ва строк (параметр @Limit) в результирующей таблице. 
--Внимание! на понимание того - что есть разделитель между частями строк,  влияет не только значение @Separator, но и его длина:
--два примера ниже вернут один и тот же результат
--SELECT * FROM zz.StrSplit1('lexem1;lexem2;;lexem4',';',3)
--SELECT * FROM zz.StrSplit1('{lexem1}{lexem2}{}{lexem4}','{}',3)
--		Результат:
--			[IDD]						[VALUE]
--			  1							 lexem1
--			  2							 lexem2
--			  3							 NULL			-- т.к. тут пустой параметр
--Если функция работает в режиме "длина @Separator - один символ", то разделитель в конце строки игнорируется:
--два примера ниже вернут один и тот же результат
--SELECT * FROM zz.StrSplit1('lexem1,lexem2',',',DEFAULT)
--SELECT * FROM zz.StrSplit1('lexem1,lexem2,',',',DEFAULT)
--		Результат:
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
			--SET @Value = RIGHT(@Value,LEN(@Value)-@SplitIndex)
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

/****** Object:  UserDefinedFunction [zz].[StrSplit2]    Script Date: 05.02.2021 16:00:31 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


--#endregion

--#region CREATE FUNCTION [zz].[StrSplit2]
CREATE FUNCTION [zz].[StrSplit2](
	 @Value NVARCHAR(MAX)
	,@Separator NVARCHAR(2)=';'
	,@Param NVARCHAR(1)='='
	,@Limit INT=1000
)
--Парсинг строки (наборы в виде переменная-значение) в таблицу с ограничением кол-ва параметров (параметр @Limit) в результирующей таблице. 
--Внимание! на понимание того - что есть разделитель между частями строк,  влияет не только значение @Separator, но и его длина (также как и в zz.StrSplit1)
--Если функция работает в режиме "длина @Separator - один символ", то разделитель в конце строки игнорируется (также как и в zz.StrSplit1)
--В итоговую таблицу попадают только те пары "переменная-значение", где переменная не пуста.
--Пример:
--SELECT * FROM zz.StrSplit2('var1=val1;=val2;var3=;v4',';','=',DEFAULT)
--вернет две строки 
--			[IDD]			[Variable]			[VALUE]
--			  1				var1				val1
--			  2				var3				NULL
--т.к. в подстроке '=val2' нет названия переменной, а 'v4' вообще непонятно что это такое
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



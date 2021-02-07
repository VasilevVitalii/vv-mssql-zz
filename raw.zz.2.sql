SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


--#endregion

--#region CREATE FUNCTION [zz].[StrToMsg]
CREATE FUNCTION [zz].[StrToMsg](@From NVARCHAR(MAX),@Value0 NVARCHAR(MAX)=NULL,@Value1 NVARCHAR(MAX)=NULL,@Value2 NVARCHAR(MAX)=NULL,@Value3 NVARCHAR(MAX)=NULL,@Value4 NVARCHAR(MAX)=NULL,@Value5 NVARCHAR(MAX)=NULL,@Value6 NVARCHAR(MAX)=NULL,@Value7 NVARCHAR(MAX)=NULL,@Value8 NVARCHAR(MAX)=NULL,@Value9 NVARCHAR(MAX)=NULL)
--Функция, которая из шаблона строки и набора подстановок для неее формирует сообщение.
--Не заточена под использование коллегами, нужна для использования в других внутренних механизмах.
--Если ожидается подстановка, а она = NULL, то в сообщении на ее месте будет написано <ПУСТО>, пример
--SELECT zz.StrToMsg('Пример пустого значения - {a}',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL)
--Если указана неправильная подстановка (напримеро, ожидается дата а пришла не дата, подстановка не сработает, пример 
--SELECT zz.StrToMsg('Пример неправильной подстановки даты - {d}','aaa',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL)

--Примеры форматов подстановок:
--1.  {a}										функция сама определит что за тип данных в подстановке, пример
--			DECLARE @d DATETIME=GETDATE()
--			SELECT zz.StrToMsg('Тут int - {a}, тут money - {a}, тут дата - {a}, тут строка {a}',45,78.99,@d,'строка!!',NULL,NULL,NULL,NULL,NULL,NULL)
--2. {n},{n.n},{n,n}{n.2}{n,2}{n.4}{n,4}		формат числа (см. функции zz.StrFromInt, zz.StrFromMoney). 
--			Важно! Если указан формат {n}, при этом пришло число типа money, подстановка не сработает.
--3. {ds},{d},{dm},{dms},{dmsm}					формат даты (см. функцию zz.StrFromDate).
--			Важно! Если в подстанову передать сразу переменную типа datetime (понадеявшись не неявное преобразование SQL),
--			то будут отсечено все что ниже минут - вот так себя ведет это преобразование, пример:
--			DECLARE @d DATETIME=GETDATE()
--			SELECT zz.StrToMsg('Пример разных форматов дат с преобразованием SQL: ds={ds}, d={d}, dms={dms}, dmsm={dmsm}',@d,@d,@d,@d,NULL,NULL,NULL,NULL,NULL,NULL)
--			DECLARE @n NVARCHAR(MAX)=CONVERT(NVARCHAR(MAX),@d,126)
--			SELECT zz.StrToMsg('Пример разных форматов дат с собственным преобразованием: ds={ds}, d={d}, dms={dms}, dmsm={dmsm}',@n,@n,@n,@n,NULL,NULL,NULL,NULL,NULL,NULL)
--4. {s}										произвольная строка
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
					SET @replaceLexem='<ПУСТО>'
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
					SET @replaceLexem='<ПУСТО>'
				END ELSE begin
					SET @replaceLexem=@currentValue
				END
			END ELSE IF @findLexem IN ('{n}') BEGIN
				IF @currentValue IS NULL BEGIN
					SET @replaceLexem='<ПУСТО>'
				END ELSE BEGIN
					IF zz.StrIsInt(@currentValue)=1 BEGIN
						set @replaceLexem=zz.StrFromInt(zz.StrToInt(@currentValue),'n')
					END
				end
			END ELSE IF @findLexem IN ('{n.n}','{n,n}','{n.2}','{n,2}','{n.4}','{n,4}') BEGIN
				IF @currentValue IS NULL BEGIN
					SET @replaceLexem='<ПУСТО>'
				END ELSE BEGIN			
					IF zz.StrIsMoney(@currentValue)=1 BEGIN
						SET @replaceLexem=zz.StrFromMoney(zz.StrToMoney(@currentValue),SUBSTRING(@findLexem,2,LEN(@findLexem)-2))
					END	
				END
			END ELSE IF @findLexem IN ('{ds}','{d}','{dm}','{dms}','{dmsm}') BEGIN
				IF @currentValue IS NULL BEGIN
					SET @replaceLexem='<ПУСТО>'
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


--#endregion

--#region CREATE FUNCTION [zz].[StrToDate]


/****** Object:  UserDefinedFunction [zz].[StrToGuid]    Script Date: 05.02.2021 16:00:05 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


--#endregion

--#region CREATE FUNCTION [zz].[StrToGuid]
CREATE FUNCTION [zz].[StrToGuid](@Value NVARCHAR(MAX))
--Преобразует строку в GUID
--Если в строке не GUID, будет NULL
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




--#endregion

--#region CREATE FUNCTION [zz].[StrFromMoney]
CREATE FUNCTION [zz].[StrFromMoney](@Value MONEY,@Format VARCHAR(32))
--Преобразует MONEY в строку с соответстии с указанным шаблоном
--Если выбраны форматы 'n.2' или 'n,2', еще и округлит возвращаемый результат
--Примеры:
--  SELECT [zz].[StrFromMoney](22.099,'n.n')				--вернет '22.099'
--  SELECT [zz].[StrFromMoney](22.099,'n,n')				--вернет '22,099'
--  SELECT [zz].[StrFromMoney](22.099,'n.2')				--вернет '22.10'
--  SELECT [zz].[StrFromMoney](22.099,'n,2')				--вернет '22,10'
--  SELECT [zz].[StrFromMoney](22.099,'n.4')				--вернет '22.0990'
--  SELECT [zz].[StrFromMoney](22.099,'n,4')				--вернет '22,0990'
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

/****** Object:  UserDefinedFunction [zz].[StrFromDate]    Script Date: 05.02.2021 16:00:05 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


--#region CREATE FUNCTION [zz].[StrFromDate]
CREATE FUNCTION [zz].[StrFromDate](@Value DATETIME,@Format VARCHAR(32))
--Преобразует DATETIME в строку с соответстии с указанным шаблоном
--Почти все шаблоны имеют краткую аналогию, например,  [zz].[StrFromDate](GETDATE(),'yyyymmdd')=[zz].[StrFromDate](GETDATE(),'ds')
--Шаблоны (+аналогия):
--'yyyymmdd'					'ds'	
--'dd.mm.yyyy'					'd'	
--'dd.mm.yyyy hh:mm'			'dm'	
--'dd.mm.yyyy hh:mm:ss'			'dms'	
--'dd.mm.yyyy hh:mm:ss.mmm'		'dmsm'	
--'dd.mm.yy'			
--Внимание!! Особенности DATETIME таковы, что часть миллисекунд теряются:
--zz.StrFromDate('2015-01-19T14:03:35.787','dmsm')			-- миллисекунды вернутся правильно (787)
--zz.StrFromDate('2015-01-19T14:03:35.789','dmsm')			-- миллисекунды вернутся НЕ правильно (790)
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

/****** Object:  UserDefinedFunction [zz].[KillTime]    Script Date: 05.02.2021 16:00:06 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


--#endregion

--#region CREATE FUNCTION [zz].[KillTime]
CREATE FUNCTION [zz].[KillTime](@Value DATETIME)
--Обрезает время из DATETIME
RETURNS DATETIME
AS
BEGIN
	IF @Value IS NULL RETURN NULL
	RETURN CONVERT(DATETIME,CONVERT(NVARCHAR,@Value,112))
END

GO

/****** Object:  UserDefinedFunction [zz].[StrFromInt]    Script Date: 05.02.2021 16:00:06 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


--#endregion

--#region CREATE FUNCTION [zz].[StrFromInt]
CREATE FUNCTION [zz].[StrFromInt](@Value INT,@Format VARCHAR(32))
--Преобразует INT в строку с соответстии с указанным шаблоном
--Примеры:
--  SELECT [zz].[StrFromInt](5,'n')				--вернет '5'
--  SELECT [zz].[StrFromInt](5,'n.2')			--вернет '5.00'
--  SELECT [zz].[StrFromInt](5,'n.4')			--вернет '5.0000'
--  SELECT [zz].[StrFromInt](5,'n,2')			--вернет '5,00'
--  SELECT [zz].[StrFromInt](5,'n,4')			--вернет '5,0000'
--  SELECT [zz].[StrFromInt](NULL,'n,4')		--вернет NULL
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



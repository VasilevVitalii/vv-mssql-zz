USE [rrCatena]
GO

/****** Object:  StoredProcedure [zz].[Error]    Script Date: 05.02.2021 16:01:24 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


--#endregion 

CREATE PROCEDURE [zz].[Error]
	@spName NVARCHAR(MAX)=NULL
AS
--Генерация текста ошибка со стеком и ее поднятие
DECLARE @Error NVARCHAR(4000)

DECLARE @PointErrorName NVARCHAR(MAX)
IF @spName IS NOT NULL BEGIN
	SET @PointErrorName = @spName
END ELSE BEGIN
	SET @PointErrorName = ERROR_PROCEDURE()
END
	
SET @Error=LEFT(CHAR(10)+'ERROR_PROCEDURE()='+ISNULL(@PointErrorName,'NULL')+CHAR(10)+
				CHAR(9)+'ERROR_LINE()='+ISNULL(CONVERT(NVARCHAR,ERROR_LINE()),'NULL')+','+CHAR(10)+		 
				CHAR(9)+'ERROR_MESSAGE()='+ISNULL(ERROR_MESSAGE(),'NULL')
		,4000)
			
RAISERROR(@Error, 16, 1)

GO

/****** Object:  StoredProcedure [zz].[LockOff]    Script Date: 05.02.2021 16:01:24 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


--#endregion

--#region CREATE PROCEDURE [zz].[LockOff]
CREATE PROCEDURE [zz].[LockOff]
 	 @Name NVARCHAR(255)	--название блокировки
 	,@isForce BIT=1			--0 - из стека удаляется только одна блокировка с таким именем
 							--1 - из стека удаляются ВСЕ блокировки с таким именем 
	,@isLocal BIT=1					-- 1 - проверяем блокировку в текущей базе данных
	              					-- 0 - проверяем блокировку в базе данных master
AS
BEGIN TRY
SET NOCOUNT ON;
DECLARE @resultSession INT
AgainDeleteSession:
IF @isLocal=1 BEGIN
	EXEC @resultSession=sp_releaseapplock
		 @Resource=@Name
		,@LockOwner='Session'
END ELSE BEGIN
	EXEC @resultSession=master..sp_releaseapplock
		 @Resource=@Name
		,@LockOwner='Session'
END			
IF @isForce=1 AND @resultSession=0 GOTO AgainDeleteSession	
END TRY
BEGIN CATCH
END CATCH

GO

/****** Object:  StoredProcedure [zz].[LockOn]    Script Date: 05.02.2021 16:01:24 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


--#region CREATE PROCEDURE [zz].[LockOn]
CREATE PROCEDURE [zz].[LockOn]
-- =============================================
-- Возвращает:
--  0 Блокировка была успешно предоставлена в синхронном режиме.
--  1 Блокировка была предоставлена успешно после снятия других несовместимых блокировок.
-- -1 Истекло время ожидания запроса блокировки.
-- -2 Запрос блокировки был отменен.
-- -3 Запрос блокировки был выбран как жертва взаимоблокировки.
-- -999 Указывает ошибку при проверке параметра или другую ошибку вызова.
-- -1000  Хранимка свалилась в блок CATCH
-- -1001 Недопустимое название локировки
-- =============================================
	 @Name NVARCHAR(255)
	,@Timeout INT=0					--если блокировка есть, то сколько времени ждать до ее открытия,
 									--варианты:
 									-- 0 - не ждать
 									-- >0 время в миллисекундах	
	,@isLocal BIT=1					-- 1 - ставим блокировку в текущей базе данных
	              					-- 0 - ставим блокировку в базе данных master
AS
BEGIN TRY
SET NOCOUNT ON
DECLARE @result INT	

IF PATINDEX('%{LOG%',@Name)>0 RETURN -1001 
 	
IF @isLocal=1 BEGIN
	EXEC @Result=sp_getapplock 
		 @Resource=@Name 
		,@LockOwner='Session'
		,@LockMode = 'exclusive' 
		,@LockTimeOut=@Timeout
END ELSE BEGIN
	EXEC @Result=master..sp_getapplock 
		 @Resource=@Name 
		,@LockOwner='Session'
		,@LockMode = 'exclusive' 
		,@LockTimeOut=@Timeout
END			
RETURN @Result
END TRY
BEGIN CATCH
	RETURN -1000
END CATCH

GO

/****** Object:  StoredProcedure [zz].[PrintDebug]    Script Date: 05.02.2021 16:01:25 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


-- =============================================
-- Author:		<a9>
-- Create date: <2012.03.19>
-- Description:	<печать отладочных сообщений>
-- =============================================
CREATE PROCEDURE [zz].[PrintDebug]
	 @isPrintDebug BIT=1	--печатать или нет это сообщение
	,@Msg NVARCHAR(255)		--сообщение
	,@Level INT=1			--1-'info',2-'warning',3-'error'
	,@Format NVARCHAR(255)= NULL  --варианты:
								  --NULL - в начало сообщения ничего не добавляется
								  --{TITLE1} - в начало сообщения ничего не добавляется '============'
								  --{TITLE2} - в начало сообщения ничего не добавляется '======'
								  --{HEAD} - в начало сообщения ничего не добавляется '==='	
								  --{SHIFT1} - в начало сообщения добавляется три пробела
								  --{SHIFT2} - в начало сообщения добавляется шесть пробелов
								  --{SHIFT3} - в начало сообщения добавляется девять пробелов
AS
BEGIN TRY
SET NOCOUNT ON;
IF ISNULL(@isPrintDebug,0)=0 RETURN
IF ISNULL(@Msg,'')='' BEGIN
	RAISERROR('', 10, 1) WITH NOWAIT
	RETURN
END

DECLARE @DD DATETIME SET @DD=GETDATE()

DECLARE @DebugMsg NVARCHAR(255)
IF isNull(@Level,0)=1 SET @DebugMsg='info    ('+CONVERT(NVARCHAR,@DD,20)+'): '
IF isNull(@Level,0)=2 SET @DebugMsg='warning ('+CONVERT(NVARCHAR,@DD,20)+'): '
IF isNull(@Level,0)=3 SET @DebugMsg='error   ('+CONVERT(NVARCHAR,@DD,20)+'): '
IF @Format IS NOT NULL BEGIN
	IF @Format='{TITLE1}' SET @DebugMsg=@DebugMsg+'============'
	ELSE IF @Format='{TITLE2}' SET @DebugMsg=@DebugMsg+'======'
	ELSE IF @Format='{HEAD}' SET @DebugMsg=@DebugMsg+'==='
	ELSE IF @Format='{SHIFT1}' SET @DebugMsg=@DebugMsg+'   '
	ELSE IF @Format='{SHIFT2}' SET @DebugMsg=@DebugMsg+'      '
	ELSE IF @Format='{SHIFT3}' SET @DebugMsg=@DebugMsg+'         '
END 
				
SET @DebugMsg=LEFT(ISNULL(@DebugMsg,'')+@Msg,255)
RAISERROR(@DebugMsg, 10, 1) WITH NOWAIT

END TRY
BEGIN CATCH
	DECLARE @Error NVARCHAR(4000)
	SET @Error=rrUtil.dbo.ErrorMessage()	
	RAISERROR(@Error, 16, 1)
END CATCH


GO

/****** Object:  StoredProcedure [zz].[TblAddField]    Script Date: 05.02.2021 16:01:25 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


--#endregion 

--#region CREATE PROCEDURE [zz].[TblAddField]
CREATE PROCEDURE [zz].[TblAddField]
	 @To NVARCHAR(MAX)
	,@Fields NVARCHAR(MAX)
	,@IsIgnoreError BIT=1
AS
--Добавление столбцов в таблицу, столбцов может быть несколько через ';' (см. пример ниже)
--По умолчанию возникающие ошибки подавляются (@IsIgnoreError=1),
--например, такой скрипт:
--IF OBJECT_ID('tempdb..#t') IS NOT NULL DROP TABLE #t 
--CREATE TABLE #t(f1 INT)
--EXEC zz.TblAddField @To='#t',@Fields='f2 NVARCHAR(MAX); f3 XYZ; f4 MONEY'
--добавит два поля - f2 и f4
DECLARE @ProcRes INT=0
IF @To IS NULL OR LEN(@To)<=0 BEGIN
	RAISERROR('zz.TblAddField.@To IS NULL', 16, 1)
	RETURN 0	                  	
END 
IF @Fields IS NULL OR LEN(@Fields)<=0 BEGIN
	RAISERROR('zz.TblAddField.@Fields IS NULL', 16, 1)
	RETURN 0				
END
EXEC zz.TblCheck @Value=@To, @Name='zz.TblAddField.@To'   
	
DECLARE @SQL NVARCHAR(MAX), @cur_Field NVARCHAR(MAX)
	
DECLARE cur CURSOR LOCAL FAST_FORWARD READ_ONLY FOR
SELECT [VALUE] FROM zz.StrSplit1(@Fields,';',DEFAULT) ORDER BY IDD
OPEN cur
FETCH FROM cur INTO @cur_Field
WHILE @@FETCH_STATUS = 0 BEGIN
	IF @cur_Field IS NULL GOTO nxt 
	SET @cur_Field=LTRIM(RTRIM(@cur_Field))
	IF LEN(@cur_Field)<=0 GOTO nxt 
		
	SET @SQL='ALTER TABLE '+@To+' ADD '+@cur_Field
	BEGIN TRY
		EXEC sp_executesql @SQL
		SET @ProcRes=@ProcRes+1		
	END TRY 
	BEGIN CATCH
		IF ISNULL(@IsIgnoreError,0)<>1 BEGIN
			DECLARE @Error NVARCHAR(4000)
			SET @Error=LEFT('Ошибка добавления поля '+ISNULL(@cur_Field,'NULL')+' в таблицу '+ISNULL(@To,'NULL')+':'+CHAR(13)+ERROR_MESSAGE(),4000)
			RAISERROR(@Error, 16, 1)
		END 
	END CATCH
	nxt:
FETCH FROM cur INTO @cur_Field
END; CLOSE cur; DEALLOCATE cur
RETURN @ProcRes

GO

/****** Object:  StoredProcedure [zz].[TblAddIndex]    Script Date: 05.02.2021 16:01:25 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


--#endregion

--#region CREATE PROCEDURE [zz].[TblAddIndex]
CREATE PROCEDURE [zz].[TblAddIndex]
	 @To NVARCHAR(MAX)
	,@Fields NVARCHAR(MAX)
	,@Name NVARCHAR(255)=NULL
	,@Mode VARCHAR(8)=NULL
	,@IsIgnoreError BIT=1
--Добавление индекса (в т.ч. и уникального) или первичного ключа в таблицу по полю или нескольким полям.
--По умолчанию возникающие ошибки подавляются (@IsIgnoreError=1).
--Если полей несколько, то можно их указывать как через ',', так и через ';'.
--Пример добавления простого индекса по двум полям:
--IF OBJECT_ID('tempdb..#t') IS NOT NULL DROP TABLE #t 
--CREATE TABLE #t(f1 INT, f2 INT)
--EXEC zz.TblAddIndex @To='#t',@Fields='f1; f2'
--Пример добавления уникального индекса по двум полям:
--IF OBJECT_ID('tempdb..#t') IS NOT NULL DROP TABLE #t 
--CREATE TABLE #t(f1 INT, f2 INT)
--EXEC zz.TblAddIndex @To='#t',@Fields='f1, f2',@Mode='uniq'
--Пример добавления первичного ключа по двум полям:
--IF OBJECT_ID('tempdb..#t') IS NOT NULL DROP TABLE #t 
--CREATE TABLE #t(f1 INT NOT NULL, f2 INT NOT NULL)
--EXEC zz.TblAddIndex @To='#t',@Fields='f1; f2',@Mode='key'
--Внимание! Если, например, добавлять первичный ключ, в котором попадется NULLABLE столбец, и при этом ловить ошибки (@IsIgnoreError=0),
--то ошибка SQL-сервера выглядит непонятно - 'Could not create constraint. See previous errors.',  
--и ничего с этим не поделать ((
AS
--Добавление индекса в таблицу
IF @To IS NULL OR LEN(@To)<=0 BEGIN
	RAISERROR('zz.TblAddField.@To IS NULL', 16, 1)
	RETURN 0	                  	
END 
IF @Fields IS NULL OR LEN(@Fields)<=0 BEGIN
	RAISERROR('zz.TblAddField.@Fields IS NULL', 16, 1)
	RETURN 0				
END
EXEC zz.TblCheck @Value=@To, @Name='zz.TblAddField.@To'   

SET @Fields=REPLACE(@Fields,';',',')
	
IF @Name IS NULL OR LEN(@Name)<=0	BEGIN
	SET @Name='i'+REPLACE(CONVERT(NVARCHAR(36),NEWID()),'-','')
END
 
DECLARE @SQL NVARCHAR(MAX), @Msg NVARCHAR(256)
	
IF @Mode IS NULL OR LEN(@Mode)<=0 BEGIN
	SET @SQL='CREATE INDEX ' + @Name + ' ON '+ @To + '('+@Fields+')'
END ELSE IF @Mode='uniq' BEGIN
	SET @SQL='CREATE UNIQUE INDEX ' + @Name + ' ON '+ @To + '('+@Fields+')'
END ELSE IF @Mode='key' BEGIN
	SET @SQL='ALTER TABLE ' + @To + ' ADD CONSTRAINT '+ @Name + ' PRIMARY KEY('+@Fields+')'
END ELSE BEGIN
	SET @Msg='Неизвестный параметр zz.TblAddIndex.@Mode='+ISNULL(@Mode,'NULL')
	RAISERROR(@Msg, 16, 1)
	RETURN 0 
END

BEGIN TRY
	EXEC sp_executesql @SQL
	RETURN 1
END TRY
BEGIN CATCH
	IF ISNULL(@IsIgnoreError,0)<>1 BEGIN
		DECLARE @Error NVARCHAR(4000)
		SET @Error=LEFT('Ошибка добавления индекса '+ISNULL(@SQL,'NULL')
		+CASE WHEN @Mode='key' THEN ' (возможно не все столбцы добавляемого первичного ключа NOT NULL)' ELSE '' END
		+':'+CHAR(13)+ISNULL(ERROR_MESSAGE(),'Неопределенная ошибка добавления индекса/ключа'),4000)
		RAISERROR(@Error, 16, 1)			
	END
	RETURN 0 
END CATCH


--IF @Err IS NULL BEGIN
--	RETURN 1
--END ELSE BEGIN
--	IF ISNULL(@IsIgnoreError,0)<>1 BEGIN
--		SET @Err=LEFT('Ошибка добавления индекса '+ISNULL(@SQL,'NULL')+':'+CHAR(13)+@Err,4000)
--		RAISERROR(@Err, 16, 1)			
--	END
--	RETURN 0 
--END

GO

/****** Object:  StoredProcedure [zz].[TblCheck]    Script Date: 05.02.2021 16:01:25 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



--#region CREATE PROCEDURE [zz].[TblCheck]
CREATE PROCEDURE [zz].[TblCheck]
	 @Value NVARCHAR(MAX)
	,@Name NVARCHAR(MAX)=NULL
	,@Value0 NVARCHAR(MAX)=NULL
	,@Value1 NVARCHAR(MAX)=NULL
	,@Value2 NVARCHAR(MAX)=NULL
	,@Value3 NVARCHAR(MAX)=NULL
	,@Value4 NVARCHAR(MAX)=NULL
	,@Value5 NVARCHAR(MAX)=NULL
	,@Value6 NVARCHAR(MAX)=NULL
	,@Value7 NVARCHAR(MAX)=NULL
	,@Value8 NVARCHAR(MAX)=NULL
	,@Value9 NVARCHAR(MAX)=NULL	
AS
--Проверка наличия таблицы и поднятие ошибки, если ее нет.
--На вход подается название самой таблицы, ее имя и набора подстановок для имени (см. zz.StrToMsg).
--Эта одна из немногих хранимок, где допускается (и даже приветствкется) неуказание названий входных переменных хранимки.
--Примеры:
--EXEC [zz].[TblCheck] '#ttt','@ValueForTTT'
--EXEC [zz].[TblCheck] '#ttt','для подразделения {a}({a})','маг 101','47899623-4C1E-425C-ADEF-8BE53C198238'
BEGIN
	IF @Value IS NULL OR LEN(@Value)<=0 BEGIN
		DECLARE @Msg NVARCHAR(256)=CASE WHEN @Name IS NULL OR LEN(@Name)<=0 THEN 'zz.TblCheck.@Value IS NULL' ELSE 'Не указана таблица в переменной '+@Name END
		RAISERROR(@Msg, 16, 1)
		RETURN 0	                  	
	END  
	
	IF LEFT(@Value,1)='#' SET @Value='tempdb..'+@Value  
	IF OBJECT_ID(@Value) IS NULL BEGIN
		DECLARE @Error NVARCHAR(4000)
		IF @Name IS NOT NULL BEGIN
			SET @Name=zz.StrToMsg(@Name,@Value0,@Value1,@Value2,@Value3,@Value4,@Value5,@Value6,@Value7,@Value8,@Value9)
			SET @Error=LEFT('Не найдена таблица '+@Name+'='+@Value,4000)
		END ELSE BEGIN
		    SET @Error=LEFT('Не найдена таблица '+@Value,4000)     	
		END
		RAISERROR(@Error, 16, 1)
		RETURN 0
	END
	
	RETURN 1
END

GO

/****** Object:  StoredProcedure [zz].[TblCopyData]    Script Date: 05.02.2021 16:01:26 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


--#endregion

--#region CREATE PROCEDURE [zz].[TblCopyData]
CREATE PROCEDURE [zz].[TblCopyData]
	 @From NVARCHAR(MAX)      
	,@To NVARCHAR(MAX)
	,@SQL NVARCHAR(MAX)=NULL
	,@Fields NVARCHAR(MAX)=NULL       
	,@MappingFields NVARCHAR(MAX)=NULL
AS
--Копирование данных из таблицы-источника в таблицу-получатель
--Обрабатываются все типы данных из sys.systypes.
--Перетаскиваются данные из полей в поля, которые
--1. Имеют одинаковое имя в таблице-отправителе и таблице получателе + переопределение этих имен в параметре @MappingFields - о нем смотри ниже
--2. Имеют одинаковые типы данных или предумотрено преобразование данных. Какие преобразования предусмотрены, ищи раздел "Поддержка следующих преобразований типов данных" в теле хранимки

--@From, @To - таблица-источник и таблица-получатель.

--@SQL - расширение SQL запроса, в котором таблица-источник фигурирует под псевдонимом [F]
--	Внимание! Тут указывать надо поля равно так, как они в таблице-источнике (не обращая внимание на параметр @MappingFields - о нем смотри ниже)

--@Fields - тут можно отсекать часть полей:
--	@Fields='none:f1;f2'  - переносить все поля, кроме f1, f2
--	@Fields='only:f1;f2'  - переносить только поля f1, f2
--	Внимание! Тут указывать надо поля равно так, как они в таблице-источнике (не обращая внимание на параметр @MappingFields - о нем смотри ниже)

--@MappingFields - тут можно указать, какие поля в таблице-источнике в какие другие поля переносить в таблице-получателе, например:
--@MappingFields='Title>AnotherTitle;DD>AnotherDD'

--Простой пример:
--IF OBJECT_ID('tempdb..#t1') IS NOT NULL DROP TABLE #t1
--IF OBJECT_ID('tempdb..#t2') IS NOT NULL DROP TABLE #t2 
--CREATE TABLE #t1(i INT)
--CREATE TABLE #t2(i INT)
--INSERT INTO #t1(i) SELECT 42
--EXEC zz.TblCopyData @From='#t1', @To='#t2'
--SELECT * FROM #t2

--Сложный пример:
--IF OBJECT_ID('tempdb..#t1') IS NOT NULL DROP TABLE #t1
--IF OBJECT_ID('tempdb..#t2') IS NOT NULL DROP TABLE #t2 
--CREATE TABLE #t1(Guid UNIQUEIDENTIFIER NOT NULL, Title NVARCHAR(255), DD DATE, IDD INT)
--CREATE TABLE #t2(AnotherGuid UNIQUEIDENTIFIER NOT NULL, AnotherTitle NVARCHAR(255), AnotherDD DATETIME, IDD INT)
--INSERT INTO #t1(Guid, DD, Title, IDD) SELECT NEWID(),'20150101','Первое число',1
--INSERT INTO #t1(Guid, DD, Title, IDD) SELECT NEWID(),'20150102','Второе число',2
--INSERT INTO #t1(Guid, DD, Title, IDD) SELECT NEWID(),'20150103','Третье число',3
--INSERT INTO #t1(Guid, DD, Title, IDD) SELECT NEWID(),'20150104','Четвертое число',4
--INSERT INTO #t1(Guid, DD, Title, IDD) SELECT NEWID(),'20150105','Пятое число',5
--EXEC zz.TblCopyData 
--	 @From = '#t1'
--	,@To = '#t2'
--	,@SQL='WHERE F.DD BETWEEN ''20150102'' AND ''20150104'''
--	,@Fields='only:Guid;Title;DD'       
--    ,@MappingFields='Guid>AnotherGuid;Title>AnotherTitle;DD>AnotherDD'
--SELECT * FROM #t2
BEGIN TRY
SET NOCOUNT ON;

DECLARE @Msg NVARCHAR(255)
DECLARE @SuffixSQL NVARCHAR(MAX); SET @SuffixSQL=@SQL; SET @SQL=NULL;
DECLARE @FromBase NVARCHAR(MAX), @FromSchema NVARCHAR(MAX), @FromTable NVARCHAR(MAX)
DECLARE @ToBase NVARCHAR(MAX), @ToSchema NVARCHAR(MAX), @ToTable NVARCHAR(MAX)
DECLARE @FieldsNone NVARCHAR(MAX), @FieldsOnly NVARCHAR(MAX)
DECLARE @MappingFieldsResult NVARCHAR(MAX), @MappingFieldsResult2 NVARCHAR(MAX)=''

--#region Предварительная проверка и обработка входных параметров
IF @To IS NULL OR LEN(@To)<=0 RAISERROR('zz.[TblCopyData].@To IS NULL', 16, 1)
IF @From IS NULL OR LEN(@From)<=0  RAISERROR('zz.[TblCopyData].@From IS NULL', 16, 1)

SET @From=REPLACE(@From,'[','')
SET @From=REPLACE(@From,']','')
SET @To=REPLACE(@To,'[','')
SET @To=REPLACE(@To,']','')

EXEC zz.TblCheck @Value=@From, @Name='zz.TblCopyData.@From'
EXEC zz.TblCheck @Value=@To, @Name='zz.TblCopyData.@To'

IF @From=@To BEGIN
	SET @Msg=LEFT('zz.TblCopySchema.@From=zz.TblCopyData.@To ('+@From+')',255)  
	RAISERROR(@Msg, 16, 1)
END	

IF @Fields IS NOT NULL BEGIN
	IF LEFT(@Fields,5)='none:'	BEGIN
		SET @Fields=RIGHT(@Fields,LEN(@Fields)-5)
		SET @FieldsNone=''
		SELECT @FieldsNone=@FieldsNone+LTRIM(RTRIM([Value]))+''',''' FROM zz.StrSplit1(@Fields,';',DEFAULT)
		SET @FieldsNone=''''+LEFT(@FieldsNone,LEN(@FieldsNone)-2)
		SET @FieldsNone=REPLACE(@FieldsNone,'[','')
		SET @FieldsNone=REPLACE(@FieldsNone,']','')
	END	ELSE IF LEFT(@Fields,5)='only:' BEGIN
		SET @Fields=RIGHT(@Fields,LEN(@Fields)-5)
		SET @FieldsOnly=''
		SELECT @FieldsOnly=@FieldsOnly+LTRIM(RTRIM([Value]))+''',''' FROM zz.StrSplit1(@Fields,';',DEFAULT)
		SET @FieldsOnly=''''+LEFT(@FieldsOnly,LEN(@FieldsOnly)-2)
		SET @FieldsOnly=REPLACE(@FieldsOnly,'[','')
		SET @FieldsOnly=REPLACE(@FieldsOnly,']','')	
	END	ELSE BEGIN
		SET @Msg='Неизвестный параметр zz.TblCopySchema.@Fields='+ISNULL(@Fields,'NULL')
		RAISERROR(@Msg, 16, 1)	
	END
END 

SET @MappingFieldsResult='[F].name=[T].name'
IF @MappingFields IS NOT NULL AND LEN(@MappingFields)>0 BEGIN
	DECLARE @cur_MappingFields NVARCHAR(MAX), @cur_Field1 NVARCHAR(MAX), @cur_Field2 NVARCHAR(MAX) 
	DECLARE @MappingPair TABLE (Field NVARCHAR(MAX), OrderBy INT IDENTITY(1,1))
	DECLARE @ExistsFields TABLE (Field NVARCHAR(MAX))
	
	DECLARE cur CURSOR LOCAL FAST_FORWARD READ_ONLY FOR
	SELECT RTRIM(LTRIM([Value])) FROM zz.StrSplit1(@MappingFields,';',DEFAULT) ORDER BY IDD 
	OPEN cur
	FETCH FROM cur INTO @cur_MappingFields
	WHILE @@FETCH_STATUS = 0 BEGIN
		IF @cur_MappingFields IS NULL OR LEN(@cur_MappingFields)<=0 GOTO nextMappingFields 
		
		DELETE FROM @MappingPair		
		SET @cur_Field1=NULL
		SET @cur_Field2=NULL
		
		INSERT INTO @MappingPair(Field) SELECT RTRIM(LTRIM([Value])) FROM zz.StrSplit1(@cur_MappingFields,'>',DEFAULT) ORDER BY IDD
		IF @@ROWCOUNT<>2 BEGIN
			SET @Msg='Неизвестная часть параметра zz.TblCopySchema.@MappingFields='+ISNULL(@cur_MappingFields,NULL)
			RAISERROR(@Msg, 16, 1)
		END

		SELECT TOP 1 @cur_Field1=Field FROM @MappingPair ORDER BY OrderBy
		SELECT TOP 1 @cur_Field2=Field FROM @MappingPair ORDER BY OrderBy DESC
		
		IF @cur_Field1 IS NULL OR LEN(@cur_Field1)<=0 BEGIN
			SET @Msg='Неизвестное первое поле в одной из части параметра zz.TblCopySchema.@MappingFields='+ISNULL(@cur_MappingFields,NULL)
			RAISERROR(@Msg, 16, 1)
		END
		IF @cur_Field2 IS NULL OR LEN(@cur_Field2)<=0 BEGIN
			SET @Msg='Неизвестное второе поле в одной из части параметра zz.TblCopySchema.@MappingFields='+ISNULL(@cur_MappingFields,NULL)
			RAISERROR(@Msg, 16, 1)
		END
		IF @cur_Field1=@cur_Field2 GOTO nextMappingFields 
		
		IF EXISTS(SELECT TOP 1 * FROM @ExistsFields WHERE Field=@cur_Field2) BEGIN
			SET @Msg='Поле-получатель '+@cur_Field2+' встречается более одного раза в zz.TblCopySchema.@MappingFields='+ISNULL(@MappingFields,NULL)
			RAISERROR(@Msg, 16, 1)
		END ELSE BEGIN
			INSERT INTO @ExistsFields SELECT @cur_Field2         	
		END
		
		SET @MappingFieldsResult2=@MappingFieldsResult2+CASE WHEN LEN(@MappingFieldsResult2)>0 THEN ' OR ' ELSE '' END+'([F].[name]='''+@cur_Field1+''' AND [T].[name]='''+@cur_Field2+''')'
		SET @MappingFieldsResult=@MappingFieldsResult+' AND NOT ([F].[name]='''+@cur_Field1+''' AND [T].[name]='''+@cur_Field1+''')'
		SET @MappingFieldsResult=@MappingFieldsResult+' AND NOT ([F].[name]='''+@cur_Field2+''' AND [T].[name]='''+@cur_Field2+''')'
		
		nextMappingFields:
		
	FETCH FROM cur INTO @cur_MappingFields
	END; CLOSE cur; DEALLOCATE cur
END
IF LEN(@MappingFieldsResult2)<=0 BEGIN
	SET @MappingFieldsResult=' AND '+@MappingFieldsResult	
END ELSE BEGIN
    SET @MappingFieldsResult=' AND (('+@MappingFieldsResult+') OR ('+@MappingFieldsResult2+'))'    	
END
--#endregion

IF LEFT(@From,1)='#' BEGIN
	SELECT @FromBase='tempdb', @FromSchema='', @FromTable=@From
END	ELSE BEGIN
	SELECT @FromBase=ISNULL(DB_NAME(),PARSENAME(@From, 3)), @FromSchema=ISNULL(PARSENAME(@From, 2),'dbo'), @FromTable=PARSENAME(@From, 1)
END

IF LEFT(@To,1)='#' BEGIN
	SELECT @ToBase='tempdb', @ToSchema='', @ToTable=@To
END	ELSE BEGIN
	SELECT @ToBase=ISNULL(PARSENAME(@To, 3),DB_NAME()), @ToSchema=ISNULL(PARSENAME(@To, 2),'dbo'), @ToTable=PARSENAME(@To, 1)
END	
 
DECLARE @ParmDefinition NVARCHAR(MAX)
DECLARE @SQLfrom NVARCHAR(MAX), @SQLto NVARCHAR(MAX), @cntFields INT, @cntRows INT

SET @ParmDefinition='@FromBase NVARCHAR(MAX), @FromSchema NVARCHAR(MAX), @FromTable NVARCHAR(MAX), @ToBase NVARCHAR(MAX), @ToSchema NVARCHAR(MAX), @ToTable NVARCHAR(MAX), @SQLfrom NVARCHAR(MAX) OUT, @SQLto NVARCHAR(MAX) OUT, @cntFields INT OUT' 
SET @SQL='
	SET @SQLfrom=''''
	SET @SQLto=''''
	SELECT 
		 @SQLto=@SQLto+''[''+[T].[name]+'']''+'','' ,@SQLfrom=@SQLfrom
		+CASE 
			WHEN [T].user_type_id=231 AND [F].user_type_id=241
				THEN ''CONVERT(NVARCHAR(MAX),[F].[''+[F].[name]+''])''
			WHEN [T].user_type_id=167 AND [F].user_type_id=241
				THEN ''CONVERT(VARCHAR(MAX),[F].[''+[F].[name]+''])''
			WHEN [T].user_type_id=239 AND [F].user_type_id=241
				THEN ''CONVERT(NCHAR(4000),[F].[''+[F].[name]+''])''
			WHEN [T].user_type_id=175 AND [F].user_type_id=241
				THEN ''CONVERT(CHAR(8000),[F].[''+[F].[name]+''])''
			ELSE ''[F].[''+[F].[name]+'']''
		END +'',''
	FROM '+CONVERT(NVARCHAR(MAX),@FromBase)+'.sys.[columns] [F] WITH (NOLOCK)
	JOIN '+CONVERT(NVARCHAR(MAX),@ToBase)+'.sys.[columns] [T] WITH (NOLOCK) ON [T].[object_id]=OBJECT_ID(@ToBase+''.''+@ToSchema+''.''+@ToTable)'+@MappingFieldsResult+'
		AND 
		(
			([T].user_type_id=[F].user_type_id)
			OR ([T].user_type_id=127 AND
				(
					[F].user_type_id IN (104,56,52,48) OR ([F].user_type_id IN (106,108) AND [F].[scale]=0 AND [F].[precision]<=18)
				)
			) OR ([T].user_type_id=56 AND
				(
					[F].user_type_id IN (104,52,48) OR ([F].user_type_id IN (106,108) AND [F].[scale]=0 AND [F].[precision]<=9)
				)	
			) OR ([T].user_type_id=52 AND
				(
					[F].user_type_id IN (104,48) OR ([F].user_type_id IN (106,108) AND [F].[scale]=0 AND [F].[precision]<=4)
				)	
			) OR ([T].user_type_id=48 AND
				(
					[F].user_type_id=104 OR ([F].user_type_id IN (106,108) AND [F].[scale]=0 AND [F].[precision]<=2)
				)	
			) OR ([T].user_type_id=60 AND
				(
					[F].user_type_id IN (104,48,52,56,122) OR ([F].user_type_id IN (106,108) AND [F].[scale]<=4 AND [F].[precision]<=14)
				)	
			) OR ([T].user_type_id=122 AND
				(
					[F].user_type_id IN (104,48,52,56) OR ([F].user_type_id IN (106,108) AND [F].[scale]<=4 AND [F].[precision]<=5) 
				)	
			) OR ([T].user_type_id=231 AND [F].user_type_id IN (175,239,167)	
			) OR ([T].user_type_id=167 AND [F].user_type_id IN (175,239,231)
			) OR ([T].user_type_id=239 AND [F].user_type_id IN (175,167,231)
			) OR ([T].user_type_id=175 AND [F].user_type_id IN (239,167,231)
			) OR ([F].user_type_id=241 AND [T].user_type_id IN (231,167,239,175) AND [T].max_length IN (-1,8000)  
			) OR ([T].user_type_id=40 AND [F].user_type_id IN (42,43,61,58)
			) OR ([T].user_type_id=42 AND [F].user_type_id IN (40,43,61,58)
			) OR ([T].user_type_id=43 AND [F].user_type_id IN (40,42,61,58)
			) OR ([T].user_type_id=61 AND [F].user_type_id IN (40,42,43,58)
			) OR ([T].user_type_id=58 AND [F].user_type_id IN (40,42,43,61)
			) OR ([T].user_type_id=165 AND [F].user_type_id=173 
			) OR ([T].user_type_id=173 AND [F].user_type_id=165
			) OR ([T].user_type_id=98 AND
				(
					[F].user_type_id IN (127,173,104,175,40,61,42,106,62,56,60,99,108,59,58,52,122,98,256,41,189,48,36)
					OR ([F].user_type_id IN (231,165,167) AND [F].max_length>0)
				)			
			) OR ([F].user_type_id=189 AND [T].user_type_id IN (173,165) AND ([T].max_length=-1 OR [T].max_length>=8)
			)
		)
	/* Это штука нужна, не удалять ее!!!
	0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
	0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
	0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
	01
	*/
	WHERE [T].user_type_id<>189 AND [F].[object_id]=OBJECT_ID(@FromBase+''.''+@FromSchema+''.''+@FromTable)'
	+CASE WHEN @FieldsNone IS NULL THEN '' ELSE ' AND [F].[Name] NOT IN ('+CONVERT(NVARCHAR(MAX),@FieldsNone)+')' END
	+CASE WHEN @FieldsOnly IS NULL THEN '' ELSE ' AND [F].[Name] IN ('+CONVERT(NVARCHAR(MAX),@FieldsOnly)+')' END
	+'ORDER BY [F].[column_id] SET @cntFields=@@ROWCOUNT'
/*
Поддержка следующих преобразований типов данных:
	==Из====================		==В================
	104	BIT							127	BIGINT
	56	INT							127	BIGINT
	52	SMALLINT					127	BIGINT
	48	TINYINT						127	BIGINT
	106 DECIMAL(<=18,0)				127	BIGINT
	108	NUMERIC(<=18,0)				127	BIGINT
	104	BIT							56	INT
	52	SMALLINT					56	INT
	48	TINYINT						56	INT
	106 DECIMAL(<=9,0)				56	INT
	108	NUMERIC(<=9,0)				56	INT
	104	BIT							52	SMALLINT
	48	TINYINT						52	SMALLINT
	106 DECIMAL(<=4,0)				52	SMALLINT
	108	NUMERIC(<=4,0)				52	SMALLINT
	104	BIT							48	TINYINT
	106 DECIMAL(<=2,0)				48	TINYINT
	108	NUMERIC(<=2,0)				48	TINYINT
	104	BIT							60	MONEY
	48	TINYINT						60	MONEY
	52	SMALLINT					60	MONEY
	56	INT							60	MONEY
	122 SMALLMONEY					60	MONEY
	106 DECIMAL(<=14,<=4)			60	MONEY
	108 NUMERIC(<=14,<=4)			60	MONEY	
	104	BIT							122 SMALLMONEY
	48	TINYINT						122 SMALLMONEY
	52	SMALLINT					122 SMALLMONEY
	56	INT							122 SMALLMONEY
	106 DECIMAL(<=5,<=4)			122 SMALLMONEY
	108 NUMERIC(<=5,<=4)			122 SMALLMONEY	
	127	BIGINT						98	SQL_VARIANT 	
	173	BINARY						98	SQL_VARIANT
	104	BIT							98	SQL_VARIANT
	175	CHAR						98	SQL_VARIANT
	40	DATE						98	SQL_VARIANT
	61	DATETIME					98	SQL_VARIANT
	42	DATETIME2					98	SQL_VARIANT
	106	DECIMAL						98	SQL_VARIANT
	62	FLOAT						98	SQL_VARIANT
	56	INT							98	SQL_VARIANT
	60	MONEY						98	SQL_VARIANT
	99	NTEXT						98	SQL_VARIANT
	108	NUMERIC						98	SQL_VARIANT
	59	REAL						98	SQL_VARIANT
	58	SMALLDATETIME				98	SQL_VARIANT
	52	SMALLINT					98	SQL_VARIANT
	122	SMALLMONEY					98	SQL_VARIANT
	98	SQL_VARIANT					98	SQL_VARIANT
	256	SYSNAME						98	SQL_VARIANT
	41	TIME						98	SQL_VARIANT
	189	TIMESTAMP					98	SQL_VARIANT
	48	TINYINT						98	SQL_VARIANT
	36	UNIQUEIDENTIFIER			98	SQL_VARIANT
	231	NVARCHAR(<MAX)				98	SQL_VARIANT
	165	VARBINARY(<MAX)				98	SQL_VARIANT
	167	VARCHAR(<MAX)				98	SQL_VARIANT
	241 XML							231	NVARCHAR(MAX),NVARCHAR(4000)
	241 XML							167	VARCHAR(MAX),VARCHAR(8000)
	241 XML							239	NCHAR(4000)
	241 XML							175	CHAR(8000)
	189	TIMESTAMP					173	BINARY(>=8)
	189	TIMESTAMP					165	VARBINARY(>=8)
	--все что ниже - с возможными потерями части информации 
	175	CHAR						231	NVARCHAR		
	239	NCHAR						231	NVARCHAR		
	167 VARCHAR						231	NVARCHAR		
	175 CHAR						167	VARCHAR			
	239	NCHAR						167 VARCHAR			
	231	NVARCHAR					167	VARCHAR
	175	CHAR						239 NCHAR			
	167	VARCHAR						239	NCHAR			
	231	NVARCHAR					239	NCHAR			
	239	NCHAR						175 CHAR			
	167	VARCHAR						175	CHAR			
	231	NVARCHAR					175 CHAR
	42	DATETIME2					40	DATE			
	43	DATETIMEOFFSET				40	DATE			
	61	DATETIME					40	DATE			
	58	SMALLDATETIME				40	DATE
	40	DATE						42	DATETIME2	 
	43	DATETIMEOFFSET				42	DATETIME2	
	61	DATETIME					42	DATETIME2
	58	SMALLDATETIME				42	DATETIME2
	40	DATE						43	DATETIMEOFFSET	
	42	DATETIME2					43	DATETIMEOFFSET	
	61	DATETIME					43	DATETIMEOFFSET
	58	SMALLDATETIME				43	DATETIMEOFFSET
	40	DATE						61	DATETIME	
	42	DATETIME2					61	DATETIME	
	43	DATETIMEOFFSET				61	DATETIME
	58	SMALLDATETIME				61	DATETIME
	40	DATE						58	SMALLDATETIME
	42	DATETIME2					58	SMALLDATETIME
	43	DATETIMEOFFSET				58	SMALLDATETIME
	61	DATETIME					58	SMALLDATETIME
	165	VARBINARY					173	BINARY	
	173	BINARY						165	VARBINARY
*/	

IF RIGHT(@SQL,51)<>')ORDER BY [F].[column_id] SET @cntFields=@@ROWCOUNT' BEGIN
	SET @Msg='[zz].[TblCopyData]: При формировании запроса его длина превысила максимальную допустимую длину'
	RAISERROR(@Msg, 16, 1)
	RETURN 0		
END
 
--PRINT LEN(@SQL) --3500 (база rr2025, таблицы #t1>#t2)
--PRINT @SQL
	
EXEC sp_executesql @SQL, @ParmDefinition, 
	 @FromBase=@FromBase
	,@FromSchema=@FromSchema
	,@FromTable=@FromTable
	,@ToBase=@ToBase
	,@ToSchema=@ToSchema
	,@ToTable=@ToTable
	,@SQLfrom=@SQLfrom OUT
	,@SQLto=@SQLto OUT
	,@cntFields=@cntFields OUT

IF @cntFields<=0 RETURN 0 
 
SET @SQLto=LEFT(@SQLto,LEN(@SQLto)-1)
SET @SQLfrom=LEFT(@SQLfrom,LEN(@SQLfrom)-1) 

SET @ParmDefinition='@cntRows INT OUT' 
SET @SQL='
	SET ANSI_WARNINGS OFF;
	SET @cntRows=0
	INSERT INTO '
	+CASE WHEN @ToBase='tempdb' THEN @ToTable ELSE 
	+'['+ @ToBase+'].['+@ToSchema+'].['+@ToTable+']' END
	+'('+@SQLto+')'
	+'SELECT '+@SQLfrom+' FROM '     
	+CASE WHEN @FromBase='tempdb' THEN @FromTable ELSE 
	+'['+ @FromBase+'].['+@FromSchema+'].['+@FromTable+']' END
	+' [F]
	'+CASE WHEN @SuffixSQL IS NOT NULL AND LEN(@SuffixSQL)>0 
		THEN @SuffixSQL 
		ELSE ''
	  END+'
	SET @cntRows=@@ROWCOUNT
	'
	
EXEC sp_executesql @SQL, @ParmDefinition, @cntRows=@cntRows OUT	
RETURN @cntRows
END TRY
BEGIN CATCH
	PRINT 'Ошибка в скрипте:'
	PRINT @SQL
	DECLARE @ErrorMessage NVARCHAR(4000), @ErrorSeverity INT, @ErrorState INT
    SELECT @ErrorMessage = ERROR_MESSAGE(), @ErrorSeverity = ERROR_SEVERITY(), @ErrorState = ERROR_STATE();
    RAISERROR (@ErrorMessage, @ErrorSeverity,  @ErrorState)
    RETURN 0
END CATCH

GO

/****** Object:  StoredProcedure [zz].[TblCopySchema]    Script Date: 05.02.2021 16:01:26 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


--#endregion

--#region CREATE PROCEDURE [zz].[TblCopySchema]
CREATE PROCEDURE [zz].[TblCopySchema]
	 @From NVARCHAR(MAX)      
	,@To NVARCHAR(MAX)     
	,@Fields NVARCHAR(MAX)=NULL       
	,@isNullable BIT=0 
	,@isIdentity BIT=0
	,@isDefault BIT=0 
	,@TimeStamp VARCHAR(8)='no'
	,@isFirstLetterInLowCase BIT = 0
	,@Script NVARCHAR(MAX)=NULL OUT
AS
--Создание недостающих полей (относительно таблицы-источника) в таблице-получателе.
--Обрабатываются все типы данных из sys.systypes.
--@From, @To - таблица-источник и таблица-получатель.
--@Fields - тут можно отсекать часть полей:
--	@Fields='none:f1;f2'  - переносить все поля, кроме f1, f2
--	@Fields='only:f1;f2'  - переносить только поля f1, f2
--  @isNullable, @isIdentity, @isDefault - переносить ли NOT NULL, IDENTITY, DEFAULT
-- @TimeStamp - как переносить поле типа TimeStamp:
--  @TimeStamp='no'  -		такое поле не переносить
--  @TimeStamp='yes'  -		такое поле переносить
--  @TimeStamp='binary'  -  такое поле переносить, создавая в таблице-получателе поле binary(8)
-- @Script   -   если тут будет переменная, до запуска скрипта имеющая значение 'schema',
-- то скрипт не будет менять таблицу-получателя, а в этой переменной наружу вернет описание недостающих полей 
-- (т.е. того, что бы скрипт добави бы в таблицу - получатель) в хитром формате {[Поле1]=Тип1}..{[ПолеN]=ТипN}
-- (что-то типа такого {[f1]=bit}{[f2]=int}

--Если в таблице-отправителе будет поле z_z - оно будет проигнорировано.
--Если в таблице-получателе есть поле z_z - оно будет удалено.
BEGIN TRY
SET NOCOUNT ON;

DECLARE @Msg NVARCHAR(255)
DECLARE @FromBase SYSNAME, @FromSchema SYSNAME, @FromTable SYSNAME
DECLARE @ToBase SYSNAME, @ToSchema SYSNAME, @ToTable SYSNAME
DECLARE @FieldsNone NVARCHAR(MAX), @FieldsOnly NVARCHAR(MAX)

--#region Предварительная проверка и обработка входных параметров
IF @To IS NULL OR LEN(@To)<=0  RAISERROR('zz.[TblCopySchema].@To IS NULL', 16, 1)
IF @From IS NULL OR LEN(@From)<=0 RAISERROR('zz.[TblCopySchema].@From IS NULL', 16, 1)

SET @From=REPLACE(@From,'[','')
SET @From=REPLACE(@From,']','')
SET @To=REPLACE(@To,'[','')
SET @To=REPLACE(@To,']','')
EXEC zz.TblCheck @Value=@From, @Name='zz.TblCopySchema.@From'
EXEC zz.TblCheck @Value=@To, @Name='zz.TblCopySchema.@To'
IF @From=@To BEGIN
	SET @Msg=LEFT('zz.TblCopySchema.@From=zz.TblCopySchema.@To ('+@From+')',255)  
	RAISERROR(@Msg, 16, 1)
END	

IF @Fields IS NOT NULL BEGIN
	IF LEFT(@Fields,5)='none:'	BEGIN
		SET @Fields=RIGHT(@Fields,LEN(@Fields)-5)
		SET @FieldsNone=''
		SELECT @FieldsNone=@FieldsNone+LTRIM(RTRIM([Value]))+''',''' FROM zz.StrSplit1(@Fields,';',DEFAULT)
		SET @FieldsNone=''''+LEFT(@FieldsNone,LEN(@FieldsNone)-2)
		SET @FieldsNone=REPLACE(@FieldsNone,'[','')
		SET @FieldsNone=REPLACE(@FieldsNone,']','')
	END	ELSE IF LEFT(@Fields,5)='only:' BEGIN
		SET @Fields=RIGHT(@Fields,LEN(@Fields)-5)
		SET @FieldsOnly=''
		SELECT @FieldsOnly=@FieldsOnly+LTRIM(RTRIM([Value]))+''',''' FROM zz.StrSplit1(@Fields,';',DEFAULT)
		SET @FieldsOnly=''''+LEFT(@FieldsOnly,LEN(@FieldsOnly)-2)
		SET @FieldsOnly=REPLACE(@FieldsOnly,'[','')
		SET @FieldsOnly=REPLACE(@FieldsOnly,']','')	
	END	ELSE BEGIN
		SET @Msg='Неизвестный параметр zz.TblCopySchema.@Fields='+ISNULL(@Fields,'NULL')
		RAISERROR(@Msg, 16, 1)	
	END
END 

IF @TimeStamp IS NULL SET @TimeStamp='no'  
IF @TimeStamp NOT IN ('no','yes','binary') BEGIN
	SET @Msg='Неизвестный параметр zz.TblCopySchema.@TimeStamp='+ISNULL(@TimeStamp,'NULL')
	RAISERROR(@Msg, 16, 1)
END 
--#endregion

IF LEFT(@From,1)='#' BEGIN
	SELECT @FromBase='tempdb', @FromSchema='', @FromTable=@From
END	ELSE BEGIN
	SELECT @FromBase=ISNULL(DB_NAME(),PARSENAME(@From, 3)), @FromSchema=ISNULL(PARSENAME(@From, 2),''), @FromTable=PARSENAME(@From, 1)
END

IF LEFT(@To,1)='#' BEGIN
	SELECT @ToBase='tempdb', @ToSchema='', @ToTable=@To
END	ELSE BEGIN
	SELECT @ToBase=ISNULL(PARSENAME(@To, 3),DB_NAME()), @ToSchema=ISNULL(PARSENAME(@To, 2),''), @ToTable=PARSENAME(@To, 1)
END	
 
DECLARE @ParmDefinition NVARCHAR(MAX)
DECLARE @SQL NVARCHAR(MAX), @tmpSQL NVARCHAR(MAX), @cntFields INT
DECLARE @SeparatorL VARCHAR(1), @SeparatorR VARCHAR(1), @Separator VARCHAR(1)
IF @Script='schema' BEGIN
	SET @Separator='='
	SET @SeparatorL='{'
	SET @SeparatorR='}'
END ELSE BEGIN
	SET @Separator=' '
	SET @SeparatorL=''
	SET @SeparatorR=','
END
 
SET @ParmDefinition='@FromBase SYSNAME, @FromSchema SYSNAME, @FromTable SYSNAME, @ToBase SYSNAME, @ToSchema SYSNAME, @ToTable SYSNAME, @TimeStamp VARCHAR(8), @Separator VARCHAR(1), @SeparatorL VARCHAR(1), @SeparatorR VARCHAR(1), @isFirstLetterInLowCase BIT, @tmpSQL NVARCHAR(MAX) OUT, @cntFields INT OUT' 
SET @SQL=CASE WHEN ISNULL(@isIdentity,0)=0 THEN ''
	ELSE
	'			
	DECLARE @IDENT_SEED NUMERIC, @IDENT_INCR NUMERIC 
	SET @IDENT_SEED=IDENT_SEED(@FromBase+''.''+@FromSchema+''.''+@FromTable)
	SET @IDENT_INCR=IDENT_INCR(@FromBase+''.''+@FromSchema+''.''+@FromTable) 
	'			
	END+
	'
	SET @tmpSQL=''''
	SELECT @tmpSQL=@tmpSQL+@SeparatorL+''[''+
		CASE WHEN ISNULL(@isFirstLetterInLowCase,0) = 0 THEN [F].[name] ELSE LOWER(LEFT([F].[name],1)) + RIGHT([F].[name],LEN([F].[name])-1) END
	+'']''+@Separator'+
	CASE WHEN @TimeStamp<>'binary' THEN '+[TT].[name]'
	ELSE '+REPLACE([TT].[name],''timestamp'',''binary(8)'')' END+
	'+ CASE [TT].[name]
	    WHEN ''nvarchar'' THEN CASE WHEN [F].max_length=-1 THEN ''(MAX)'' ELSE ''(''+CONVERT(NVARCHAR(MAX),[F].max_length/2)+'')'' END
	    WHEN ''varchar'' THEN CASE WHEN [F].max_length=-1 THEN ''(MAX)'' ELSE ''(''+CONVERT(NVARCHAR(MAX),[F].max_length)+'')'' END
	    WHEN ''varbinary'' THEN CASE WHEN [F].max_length=-1 THEN ''(MAX)'' ELSE ''(''+CONVERT(NVARCHAR(MAX),[F].max_length)+'')'' END
	    WHEN ''binary'' THEN CASE WHEN [F].max_length=-1 THEN ''(MAX)'' ELSE ''(''+CONVERT(NVARCHAR(MAX),[F].max_length)+'')'' END
	    WHEN ''char'' THEN CASE WHEN [F].max_length=-1 THEN ''(MAX)'' ELSE ''(''+CONVERT(NVARCHAR(MAX),[F].max_length)+'')'' END
	    WHEN ''datetimeoffset'' THEN CASE WHEN [F].scale IS NULL THEN '''' ELSE ''(''+CONVERT(NVARCHAR(MAX),[F].scale)+'')'' END
	    WHEN ''datetime2'' THEN CASE WHEN [F].scale IS NULL THEN '''' ELSE ''(''+CONVERT(NVARCHAR(MAX),[F].scale)+'')'' END
	    WHEN ''time'' THEN CASE WHEN [F].scale IS NULL THEN '''' ELSE ''(''+CONVERT(NVARCHAR(MAX),[F].scale)+'')'' END
	    WHEN ''nchar'' THEN CASE WHEN [F].max_length=-1 THEN ''(MAX)'' ELSE ''(''+CONVERT(NVARCHAR(MAX),[F].max_length/2)+'')'' END
	    WHEN ''decimal'' THEN ''(''+CONVERT(NVARCHAR(MAX),[F].[precision])+'',''+CONVERT(NVARCHAR(MAX),[F].[scale])+'')''
	    WHEN ''numeric'' THEN ''(''+CONVERT(NVARCHAR(MAX),[F].[precision])+'',''+CONVERT(NVARCHAR(MAX),[F].[scale])+'')''
	    ELSE ''''
	END '
	+CASE WHEN ISNULL(@isNullable,0)=0 THEN '' ELSE ' + CASE WHEN [F].[is_nullable]=1 OR [TT].[Name]=''timestamp'' THEN '''' ELSE '' NOT NULL'' END' END
	+CASE WHEN ISNULL(@isDefault,0)=0  THEN '' ELSE ' + CASE WHEN [SM].[text] IS NULL THEN '''' ELSE '' DEFAULT''+[SM].[text] END' END
	+CASE WHEN ISNULL(@isIdentity,0)=0 THEN '' ELSE ' + CASE WHEN [F].[is_identity]=1 AND ISNULL(@IDENT_SEED,0)>0 AND ISNULL(@IDENT_INCR,0)>0 THEN '' IDENTITY(''+CONVERT(NVARCHAR(MAX),@IDENT_SEED)+'',''+CONVERT(NVARCHAR(MAX),@IDENT_INCR)+'') '' ELSE '''' END' END
	+'+@SeparatorR
	FROM '+CONVERT(NVARCHAR(MAX),@FromBase)+'.sys.[columns] [F] WITH (NOLOCK)
	JOIN '+CONVERT(NVARCHAR(MAX),@FromBase)+'.sys.types [TT] WITH (NOLOCK) ON [TT].system_type_id=[F].system_type_id AND [TT].user_type_id=[F].user_type_id
	'+CASE ISNULL(@isDefault,0)
	      WHEN 0 THEN '' 
	      WHEN 1 THEN 'LEFT JOIN '+CONVERT(NVARCHAR(MAX),@FromBase)+'.sys.syscomments [SM] ON [SM].id = [F].default_object_id' 
	      ELSE ''
	  END+
	' 
	WHERE [F].[object_id]=OBJECT_ID(@FromBase+''.''+@FromSchema+''.''+@FromTable)
	AND [F].[name]<>''z_z''
	AND [F].[name] NOT IN ( 
		SELECT [T].[name] 
		FROM '+CONVERT(NVARCHAR(MAX),@ToBase)+'.sys.[columns] [T] WITH (NOLOCK) WHERE [T].[object_id]=OBJECT_ID(@ToBase+''.''+@ToSchema+''.''+@ToTable)
	)
	AND [TT].[name] IN (
		 ''bigint''
		,''binary''
		,''bit''
		,''char''
		,''date''
		,''datetime''
		,''datetime2''
		,''datetimeoffset''
		,''decimal''
		,''float''
		,''geography''
		,''geometry''
		,''hierarchyid''
		,''image''
		,''int''
		,''money''
		,''nchar''
		,''ntext''
		,''numeric''
		,''nvarchar''
		,''real''
		,''smalldatetime''
		,''smallint''
		,''smallmoney''
		,''sql_variant''
		,''sysname''
		,''text''
		,''time''
		,''tinyint''
		,''uniqueidentifier''
		,''varbinary''
		,''varchar''
		,''xml''
		'+CASE WHEN @TimeStamp='no' THEN '' ELSE ',''timestamp''' END+
		'
	)'
	+CASE WHEN @FieldsNone IS NULL THEN '' ELSE ' AND [F].[Name] NOT IN ('+CONVERT(NVARCHAR(MAX),@FieldsNone)+')' END
	+CASE WHEN @FieldsOnly IS NULL THEN '' ELSE ' AND [F].[Name] IN ('+CONVERT(NVARCHAR(MAX),@FieldsOnly)+')' END
	+'
	ORDER BY [F].[column_id]
	SET @cntFields=@@ROWCOUNT'

IF RIGHT(@SQL,35) NOT LIKE '%SET @cntFields=@@ROWCOUNT' BEGIN
	RAISERROR('Превышена максимальная длина "красноты", для данной ситуации этот механизм не подходит', 16, 1)
	RETURN	0
END
	
EXEC sp_executesql @SQL, @ParmDefinition, 
	 @FromBase=@FromBase
	,@FromSchema=@FromSchema
	,@FromTable=@FromTable
	,@ToBase=@ToBase
	,@ToSchema=@ToSchema
	,@ToTable=@ToTable
	,@TimeStamp=@TimeStamp 	 
	,@Separator=@Separator
	,@SeparatorL=@SeparatorL
	,@SeparatorR=@SeparatorR
	,@isFirstLetterInLowCase=@isFirstLetterInLowCase
	,@tmpSQL=@tmpSQL OUT
	,@cntFields=@cntFields OUT

IF @cntFields<=0 BEGIN
	IF @Script='schema' SET @Script=NULL  
	RETURN 0
END	 

IF @Script='schema' BEGIN
	SET @Script=@tmpSQL
END ELSE BEGIN
	SET @SQL='ALTER TABLE '
		+CASE WHEN @ToBase='tempdb' THEN '' ELSE '['+ @ToBase+'].' END
		+CASE WHEN LEN(@ToSchema)>0 THEN '['+@ToSchema+'].' ELSE 
			CASE WHEN @ToBase='tempdb' THEN '' ELSE '..' END
		 END
		+'['+@ToTable+'] ADD '+LEFT(@tmpSQL,LEN(@tmpSQL)-1) 

	EXEC sp_executesql @SQL
	
	SET @SQL='['+ @ToBase+']'+CASE WHEN LEN(@ToSchema)>0 THEN '.['+@ToSchema+'].' ELSE '..' END +'['+@ToTable+']'  
	IF COL_LENGTH(@SQL,'z_z') IS NOT NULL BEGIN
		SET @SQL='ALTER TABLE '
			+CASE WHEN @ToBase='tempdb' THEN '' ELSE '['+ @ToBase+'].' END
			+CASE WHEN LEN(@ToSchema)>0 THEN '['+@ToSchema+'].' ELSE 
				CASE WHEN @ToBase='tempdb' THEN '' ELSE '..' END
			 END
			+'['+@ToTable+'] DROP COLUMN z_z'
		EXEC sp_executesql @SQL
	END
END		

RETURN @cntFields
END TRY
BEGIN CATCH
	DECLARE @ErrorMessage NVARCHAR(4000), @ErrorSeverity INT, @ErrorState INT
    SELECT @ErrorMessage = ERROR_MESSAGE(), @ErrorSeverity = ERROR_SEVERITY(), @ErrorState = ERROR_STATE();
    RAISERROR (@ErrorMessage, @ErrorSeverity,  @ErrorState)
    RETURN 0
END CATCH

GO

/****** Object:  StoredProcedure [zz].[TblFromXml]    Script Date: 05.02.2021 16:01:26 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


--#endregion

--#region CREATE PROCEDURE [zz].[TblFromXml]
CREATE PROCEDURE [zz].[TblFromXml]
	 @From XML      
	,@To NVARCHAR(MAX)
	,@Fields NVARCHAR(MAX)=NULL       
	,@MappingFields NVARCHAR(MAX)=NULL
	,@TimeStamp NVARCHAR(MAX)=NULL
AS	
--Запись данных из XML в таблицы-получатели.
--Суть параметров @Fields,@MappingFields,@TimeStamp точно такая же, как и в хранимке zz.TblToXml.
--Есть нюанс с передачей типа данных SQL_VARIANT, т.к. при чтении его XML такое поле пройдет через преобразование через NVARCHAR(4000).
BEGIN TRY
SET NOCOUNT ON;
DECLARE @S NVARCHAR(MAX), @S1 NVARCHAR(MAX), @S2 NVARCHAR(MAX), @Msg NVARCHAR(MAX) 

--#region Предварительная проверка и обработка входных параметров
IF @From IS NULL RAISERROR('zz.[TblFromXml].@From IS NULL', 16, 1)
IF @To IS NULL OR LEN(@To)<=0  RAISERROR('zz.[TblFromXml].@To IS NULL', 16, 1)
--#endregion

DECLARE @Schema TABLE(OrderBy INT NOT NULL IDENTITY (1,1), [Table] NVARCHAR(MAX) NOT NULL, FromTable NVARCHAR(MAX), SchemaFields NVARCHAR(MAX) NOT NULL, ToTable NVARCHAR(MAX), isExistsFromTable BIT NOT NULL DEFAULT (0)) 
DECLARE @cntFrom INT

IF OBJECT_ID('tempdb..#ExistsColumn') IS NOT NULL DROP TABLE #ExistsColumn 
CREATE TABLE #ExistsColumn([Column] NVARCHAR(MAX) NOT NULL, user_type_id INT NOT NULL, max_length SMALLINT NOT NULL)

INSERT INTO @Schema(
	 [Table]
	,FromTable
	,SchemaFields
) SELECT  
	 NULLIF(doc.value('./@table','nvarchar(MAX)'),'') AS [table]
	,NULLIF(doc.value('./@fromtable','nvarchar(MAX)'),'') AS [fromtable]
	,NULLIF(doc.value('./@fields','nvarchar(MAX)'),'') AS [schemafields]
FROM @From.nodes('root/schema/row') AS T(doc)
SET @cntFrom=@@ROWCOUNT
IF @cntFrom<=0 RETURN 

UPDATE s SET s.ToTable=t.[VALUE]
FROM @Schema s
JOIN zz.StrSplit1(@To,';',DEFAULT) t ON t.IDD=s.OrderBy

IF EXISTS(SELECT TOP 1 * FROM @Schema WHERE ToTable IS NULL) RAISERROR('Несовпадение количества присланных таблиц и таблиц-получателей', 16, 1)

;WITH fndTable AS (
	SELECT
		 CONVERT(NVARCHAR(MAX),col.query('local-name(.)')) [Node]
	FROM @From.nodes('/root/node()') doc(col)
) UPDATE s SET s.isExistsFromTable=1
FROM @Schema s
JOIN fndTable ft ON ft.[Node]=s.[Table]

DECLARE @SplitFields TABLE (OrderBy INT, Fields NVARCHAR(MAX))
DECLARE @SplitMappingFields TABLE (OrderBy INT, MappingFields NVARCHAR(MAX))
DECLARE @SplitTimeStamp TABLE (OrderBy INT, [TimeStamp] NVARCHAR(MAX))

IF @Fields IS NOT NULL AND LEN(@Fields)>0 BEGIN
	IF LEFT(@Fields,1)='{' BEGIN
		INSERT INTO @SplitFields(OrderBy,[Fields]) SELECT IDD,[VALUE] FROM zz.StrSplit1(@Fields,'{}',DEFAULT)
		IF @cntFrom<>@@ROWCOUNT BEGIN
			RAISERROR('Несоответствие числа параметров [zz].[TbltoXml].@From и [zz].[TbltoXml].@Fields', 16, 1)
			RETURN	
		END
	END ELSE BEGIN
		IF @cntFrom>1 BEGIN
			RAISERROR('Неправильный параметр [zz].[TbltoXml].@Fields', 16, 1)
			RETURN	
		END       
		INSERT INTO @SplitFields(OrderBy,[Fields]) SELECT 1,@Fields  	
	END
END

IF @MappingFields IS NOT NULL AND LEN(@MappingFields)>0 BEGIN
	IF LEFT(@MappingFields,1)='{' BEGIN
		INSERT INTO @SplitMappingFields(OrderBy,[MappingFields]) SELECT IDD,[VALUE] FROM zz.StrSplit1(@MappingFields,'{}',DEFAULT)		
		IF @cntFrom<>@@ROWCOUNT BEGIN
			RAISERROR('Несоответствие числа параметров [zz].[TbltoXml].@From и [zz].[TbltoXml].@MappingFields', 16, 1)
			RETURN	
		END
	END ELSE BEGIN
		IF @cntFrom>1 BEGIN
			RAISERROR('Неправильный параметр [zz].[TbltoXml].@MappingFields', 16, 1)
			RETURN	
		END
		INSERT INTO @SplitMappingFields(OrderBy,[MappingFields]) SELECT 1,@MappingFields         	
	END
END

IF @TimeStamp IS NOT NULL AND LEN(@TimeStamp)>0 BEGIN
	IF LEFT(@TimeStamp,1)='{' BEGIN
		INSERT INTO @SplitTimeStamp(OrderBy,[TimeStamp]) SELECT IDD,[VALUE] FROM zz.StrSplit1(@TimeStamp,'{}',DEFAULT)
		IF @cntFrom<>@@ROWCOUNT BEGIN
			RAISERROR('Несоответствие числа параметров [zz].[TbltoXml].@From и [zz].[TbltoXml].@TimeStamp', 16, 1)
			RETURN	
		END				
	END ELSE BEGIN
		IF @cntFrom>1 BEGIN
			RAISERROR('Неправильный параметр [zz].[TbltoXml].@TimeStamp', 16, 1)
			RETURN	
		END         	
		INSERT INTO @SplitTimeStamp(OrderBy,[TimeStamp]) SELECT 1,@TimeStamp
	END
END

DECLARE @cur_OrderBy INT, @cur_Table NVARCHAR(MAX), @cur_FromTable NVARCHAR(MAX), @cur_SchemaFields NVARCHAR(MAX), @cur_ToTable NVARCHAR(MAX), @cur_isExistsFromTable BIT
DECLARE @cur_Fields NVARCHAR(MAX), @cur_MappingFields NVARCHAR(MAX), @cur_TimeStamp NVARCHAR(MAX)

DECLARE cur CURSOR LOCAL FAST_FORWARD READ_ONLY FOR
SELECT OrderBy, [Table], FromTable, SchemaFields, ToTable, isExistsFromTable FROM @Schema ORDER BY OrderBy 
OPEN cur
FETCH FROM cur INTO @cur_OrderBy, @cur_Table, @cur_FromTable, @cur_SchemaFields, @cur_ToTable, @cur_isExistsFromTable
WHILE @@FETCH_STATUS = 0 BEGIN
	
	EXEC zz.TblCheck @cur_ToTable, '@To[{n}]', @cur_OrderBy
	
	TRUNCATE TABLE #ExistsColumn

	SET @cur_Fields=NULL;			SELECT @cur_Fields=NULLIF(LTRIM(RTRIM([Fields])),'') FROM @SplitFields WHERE OrderBy=@cur_OrderBy
	SET @cur_MappingFields=NULL;	SELECT @cur_MappingFields=NULLIF(LTRIM(RTRIM([MappingFields])),'') FROM @SplitMappingFields WHERE OrderBy=@cur_OrderBy
	SET @cur_TimeStamp=NULL;		SELECT @cur_TimeStamp=NULLIF(LTRIM(RTRIM([TimeStamp])),'') FROM @SplitTimeStamp WHERE OrderBy=@cur_OrderBy
	
	IF @cur_Fields IS NOT NULL BEGIN
		SET @cur_Fields=REPLACE(REPLACE(@cur_Fields,']',''),'[','')
		IF LEFT(@cur_Fields,5)='none:' OR  LEFT(@cur_Fields,5)='only:' BEGIN
			SET @S=''
			SELECT @S=@S+'{'+s1.VARIABLE+'='+s1.[VALUE]+'}'
			FROM zz.StrSplit2(@cur_SchemaFields,'{}','=',DEFAULT) s1
			LEFT JOIN zz.StrSplit1(RIGHT(@cur_Fields,LEN(@cur_Fields)-5),';',DEFAULT) s2 ON '['+s2.[VALUE]+']'=s1.VARIABLE
			WHERE (LEFT(@cur_Fields,5)='none:' AND s2.[VALUE] IS NULL) OR (LEFT(@cur_Fields,5)='only:' AND s2.[VALUE] IS NOT NULL) 
			SET @cur_SchemaFields=@S
		END	ELSE BEGIN
			SET @Msg='Неправильная часть параметра @Fields='+ISNULL(@cur_Fields,'NULL')
			RAISERROR(@Msg, 16, 1)
			RETURN	   	     	
		END
	END

	IF @cur_TimeStamp IS NOT NULL BEGIN
		IF @cur_TimeStamp NOT IN ('binary','no') BEGIN
			SET @Msg='Неправильная часть параметра @TimeStamp='+ISNULL(@cur_TimeStamp,'NULL')
			RAISERROR(@Msg, 16, 1)
			RETURN	   	     	
		END
	END ELSE BEGIN
		SET @cur_TimeStamp='no'         	
	END

	IF LEFT(@cur_ToTable,1)='#'	BEGIN
		INSERT INTO #ExistsColumn([Column],user_type_id, max_length)
		SELECT c.name, c.user_type_id, c.max_length FROM tempdb.sys.[columns] c WITH (NOLOCK) WHERE c.[object_id]=OBJECT_ID('tempdb..'+@cur_ToTable)
	END ELSE BEGIN
		INSERT INTO #ExistsColumn([Column],user_type_id, max_length)
		SELECT c.name, c.user_type_id, c.max_length  FROM sys.[columns] c WITH (NOLOCK) WHERE c.[object_id]=OBJECT_ID(@cur_ToTable)
	END
	
	SET @S=''
	SELECT @S=@S+ISNULL('['+ss.[VALUE]+']',s.VARIABLE)+' '
	+CASE WHEN s.[Value]='timestamp' THEN 'binary(8)' ELSE s.[Value] END 
	+',' 
	FROM zz.StrSplit2(@cur_SchemaFields,'{}','=',DEFAULT) s
	LEFT JOIN zz.StrSplit2(@cur_MappingFields,';','>',DEFAULT) ss ON '['+ss.VARIABLE+']'=s.VARIABLE
	WHERE ISNULL('['+ss.[VALUE]+']',s.VARIABLE) NOT IN (SELECT '['+[Column]+']' FROM #ExistsColumn)
	AND (@cur_TimeStamp='binary' OR s.[Value]<>'timestamp')
	
	IF @S IS NOT NULL AND LEN(@S)>0	BEGIN
		SET @S='ALTER TABLE '+@cur_ToTable+' ADD '+LEFT(@S,LEN(@S)-1)
		EXEC sp_executesql @S
	END

	IF @cur_isExistsFromTable<>1 GOTO nextTable

	SET @S1=''
	SET @S2=''
	
	SELECT @S1=@S1+ISNULL('['+ss.[VALUE]+']',s.VARIABLE)+',' 
	FROM zz.StrSplit2(@cur_SchemaFields,'{}','=',DEFAULT) s
	LEFT JOIN zz.StrSplit2(@cur_MappingFields,';','>',DEFAULT) ss ON '['+ss.VARIABLE+']'=s.VARIABLE
	LEFT JOIN #ExistsColumn e ON '['+e.[Column]+']'=s.VARIABLE
	WHERE (e.[Column] IS NULL OR e.user_type_id<>189)
	AND (@cur_TimeStamp='binary' OR s.[Value]<>'timestamp')
	
	SELECT @S2=CONVERT(NVARCHAR(MAX),@S2)
	+'d.value(''./@'
	+CONVERT(NVARCHAR(MAX),REPLACE(REPLACE(s.[Variable],']',''),'[',''))
	+''','''
	+CASE 
		WHEN e.user_type_id=231 AND e.max_length<>-1 AND s.[Value] LIKE 'nvarchar%' 
			THEN 'nvarchar('+CONVERT(NVARCHAR(MAX),e.max_length/2)+')'
		WHEN (s.[Value] LIKE 'geography%' OR s.[Value] LIKE 'geometry%' OR s.[Value] LIKE 'hierarchyid%' OR s.[Value] LIKE 'ntext%' OR s.[Value] LIKE 'text%' OR s.[Value] LIKE 'xml%') 
			THEN 'nvarchar(max)'	
		WHEN (s.[Value] LIKE 'sql_variant%') 
			THEN 'nvarchar(4000)'	
		WHEN (s.[Value] LIKE 'image%')
			THEN 'varbinary(max)'			
		ELSE	   
			CONVERT(NVARCHAR(MAX),s.[Value])
	END	
	+''')'
	+','
	FROM zz.StrSplit2(@cur_SchemaFields,'{}','=',DEFAULT) s
	LEFT JOIN #ExistsColumn e ON '['+e.[Column]+']'=s.VARIABLE
	WHERE (e.[Column] IS NULL OR e.user_type_id<>189)
	AND (@cur_TimeStamp='binary' OR s.[Value]<>'timestamp')
	
	IF @S1 IS NOT NULL AND LEN(@S1)>0 AND @S2 IS NOT NULL AND LEN(@S2)>0 BEGIN	
		SET @S='INSERT INTO '+@cur_ToTable+'('+LEFT(@S1,LEN(@S1)-1)+') SELECT '+LEFT(@S2,LEN(@S2)-1)+' FROM @From.nodes(''root/'+@cur_Table+'/row'') AS T(d)'
		EXEC sp_executesql @S, N'@From XML', @From=@From
	END

	nextTable:
			
FETCH FROM cur INTO @cur_OrderBy, @cur_Table, @cur_FromTable, @cur_SchemaFields, @cur_ToTable, @cur_isExistsFromTable
END; CLOSE cur; DEALLOCATE cur

END TRY
BEGIN CATCH
	DECLARE @ErrorMessage NVARCHAR(4000), @ErrorSeverity INT, @ErrorState INT
    SELECT @ErrorMessage = ERROR_MESSAGE(), @ErrorSeverity = ERROR_SEVERITY(), @ErrorState = ERROR_STATE();
    RAISERROR (@ErrorMessage, @ErrorSeverity,  @ErrorState)
    RETURN 0
END CATCH

GO

/****** Object:  StoredProcedure [zz].[TblSource]    Script Date: 05.02.2021 16:01:27 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

-- =============================================
-- Author: a9
-- Create date: 05.02.2021
-- Description: 
--
-- Вариант 1 "значение во времянку"
--
-- IF OBJECT_ID('tempdb..#example1_result1') IS NOT NULL DROP TABLE #example1_result1 
-- CREATE TABLE #example1_result1(ggg UNIQUEIDENTIFIER)
-- EXEC [zz].[TblSource] @source = '7FAA4517-59A4-41A8-9173-BE91254AB34F', @result = '#example1_result1;ggg'
-- SELECT * FROM #example1_result1
-- 
-- IF OBJECT_ID('tempdb..#example1_result2') IS NOT NULL DROP TABLE #example1_result2 
-- CREATE TABLE #example1_result2(iii INT)
-- EXEC [zz].[TblSource] @source = '42', @result = '#example1_result2;iii'
-- SELECT * FROM #example1_result2
-- 
-- Вариант 2 "времянка во времянку"
-- 
-- IF OBJECT_ID('tempdb..#example2_sourse') IS NOT NULL DROP TABLE #example2_sourse
-- CREATE TABLE #example2_sourse(a INT)
-- INSERT INTO #example2_sourse(a) SELECT 1
-- IF OBJECT_ID('tempdb..#example2_result') IS NOT NULL DROP TABLE #example2_result
-- CREATE TABLE #example2_result(b INT)
-- EXEC [zz].[TblSource] @source = '#example2_sourse;a', @result = '#example2_result;b'
-- SELECT * FROM #example2_result
-- 
-- Вариант 3 "запрос во времянку"
-- 
-- IF OBJECT_ID('tempdb..#example3_result') IS NOT NULL DROP TABLE #example3_result
-- CREATE TABLE #example3_result(z INT)
-- EXEC [zz].[TblSource] @source = 'SELECT TOP 10 object_id FROM sys.objects',	@result = '#example3_result; z'
-- SELECT * FROM #example3_result
-- =============================================
CREATE PROCEDURE [zz].[TblSource]
	@source VARCHAR(MAX),	--примеры:
							--			'#a'
							--			'#a;assortmentRid'
							--			'2D3B1F58-F54D-40AE-B6FA-18DD16656DC9'
							--			'SELECT DISTINCT aaa FROM #ttt WHERE aaa IS NOT NULL'
	@result VARCHAR(MAX)	--примеры:
							--			'#b'
							--			'#b;Rid'
AS
BEGIN TRY
SET NOCOUNT ON;
DECLARE @spName NVARCHAR(50) = OBJECT_NAME(@@procID)
IF @source IS NULL OR @result IS NULL RETURN

DECLARE @sql NVARCHAR(MAX)

IF @source LIKE '#%' BEGIN

	DECLARE @split_sourse TABLE (i INT, v VARCHAR(MAX))
	DECLARE @sourse_table VARCHAR(MAX)
	DECLARE @sourse_field VARCHAR(MAX)

	INSERT INTO @split_sourse(i, v) SELECT IDD, [VALUE] FROM zz.StrSplit1(@source, ';', DEFAULT)
	SELECT @sourse_table = v FROM @split_sourse WHERE i = 1
	SELECT @sourse_field = v FROM @split_sourse WHERE i = 2
	IF @sourse_field IS NULL BEGIN
		SET @sourse_field = 'rid'
	END
	SET @sql = 'SELECT DISTINCT ' + @sourse_field + ' FROM ' + @sourse_table + ' WHERE ' + @sourse_field + ' IS NOT NULL'

END ELSE IF @source LIKE 'select %' BEGIN

	SET @sql = @source

END ELSE BEGIN
	
	SET @sql = 'SELECT ''' + @source + ''''
	         	
END

DECLARE @split_result TABLE (i INT, v VARCHAR(MAX))
DECLARE @result_table VARCHAR(MAX)
DECLARE @result_field VARCHAR(MAX)

INSERT INTO @split_result(i, v) SELECT IDD, [VALUE] FROM zz.StrSplit1(@result, ';', DEFAULT)
SELECT @result_table = v FROM @split_result WHERE i = 1
SELECT @result_field = v FROM @split_result WHERE i = 2
IF @result_field IS NULL BEGIN
	SET @result_field = 'rid'
END

SET @sql = 'INSERT INTO ' + @result_table + ' (' + @result_field + ') ' + @sql
EXEC (@sql)
 
END TRY
BEGIN CATCH
	EXEC zz.[Error] @spName = @spName
END CATCH

GO

/****** Object:  StoredProcedure [zz].[TbltoXml]    Script Date: 05.02.2021 16:01:27 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


--#endregion

--#region CREATE PROCEDURE [zz].[TbltoXml]
CREATE PROCEDURE [zz].[TbltoXml]
	 @From NVARCHAR(MAX)      
	,@To XML=NULL OUT
	,@SQL NVARCHAR(MAX)=NULL
	,@Fields NVARCHAR(MAX)=NULL       
	,@MappingFields NVARCHAR(MAX)=NULL
	,@TimeStamp NVARCHAR(MAX)=NULL
AS	
--Копирование одной или нескольких таблиц в одну переменную типа XML
--@From - одна (например, @From='#t1') или несколько (например, @From='#t1;#t2') таблиц-источников
--@To - получившийся результат
--суть параметров @SQL,@Fields,@MappingFields,@TimeStamp точно такая же, как и в хранимке zz.TblCopyData,
--с поправкой на то, что в этой хранимке мы можем читать данные из более чем одной-таблицы-источника.
--Например, если в @From='#t1;#t2', то в будет что-то подобное @Fields='{none:f1;f2}{none:a1;a2}',
--а если какой-то модификатор нужно только для одной таблицы, то для всех остальных этот мадификатор должен быть,
--но пустым, что-то типа такого @Fields='{}{none:a1;a2}'
BEGIN TRY
SET NOCOUNT ON;
SET @To=NULL
DECLARE @S NVARCHAR(MAX)

--#region Предварительная проверка и обработка входных параметров
IF @From IS NULL OR LEN(@From)<=0  RAISERROR('zz.[TblToXml].@From IS NULL', 16, 1)
--#endregion

DECLARE @SplitFrom TABLE (OrderBy INT, [From] NVARCHAR(MAX))
DECLARE @SplitSql TABLE (OrderBy INT, [SQL] NVARCHAR(MAX))
DECLARE @SplitFields TABLE (OrderBy INT, Fields NVARCHAR(MAX))
DECLARE @SplitMappingFields TABLE (OrderBy INT, MappingFields NVARCHAR(MAX))
DECLARE @SplitTimeStamp TABLE (OrderBy INT, [TimeStamp] NVARCHAR(MAX))

IF OBJECT_ID('tempdb..#Schema') IS NOT NULL DROP TABLE #Schema 
CREATE TABLE #Schema(OrderBy INT NOT NULL, [Table] NVARCHAR(MAX) NOT NULL, Fields NVARCHAR(MAX) NOT NULL, [SQL] NVARCHAR(MAX) NOT NULL)

DECLARE @cntFrom INT 

INSERT INTO @SplitFrom(OrderBy, [From]) SELECT IDD,[VALUE] FROM zz.StrSplit1(@From,';',DEFAULT)
SET @cntFrom=@@ROWCOUNT
IF @cntFrom<=0 RETURN 

IF @SQL IS NOT NULL AND LEN(@SQL)>0 BEGIN
	IF LEFT(@SQL,1)='{' BEGIN
		INSERT INTO @SplitSql(OrderBy,[SQL]) SELECT IDD,[VALUE] FROM zz.StrSplit1(@SQL,'{}',DEFAULT)		
		IF @cntFrom<>@@ROWCOUNT BEGIN
			RAISERROR('Несоответствие числа параметров [zz].[TbltoXml].@From и [zz].[TbltoXml].@SQL', 16, 1)
			RETURN	
		END
	END ELSE BEGIN
		IF @cntFrom>1 BEGIN
			RAISERROR('Неправильный параметр [zz].[TbltoXml].@SQL', 16, 1)
			RETURN	
		END
		INSERT INTO @SplitSql(OrderBy,[SQL]) SELECT 1,@SQL
	END
END

IF @Fields IS NOT NULL AND LEN(@Fields)>0 BEGIN
	IF LEFT(@Fields,1)='{' BEGIN
		INSERT INTO @SplitFields(OrderBy,[Fields]) SELECT IDD,[VALUE] FROM zz.StrSplit1(@Fields,'{}',DEFAULT)
		IF @cntFrom<>@@ROWCOUNT BEGIN
			RAISERROR('Несоответствие числа параметров [zz].[TbltoXml].@From и [zz].[TbltoXml].@Fields', 16, 1)
			RETURN	
		END
	END ELSE BEGIN
		IF @cntFrom>1 BEGIN
			RAISERROR('Неправильный параметр [zz].[TbltoXml].@Fields', 16, 1)
			RETURN	
		END       
		INSERT INTO @SplitFields(OrderBy,[Fields]) SELECT 1,@Fields  	
	END
END

IF @MappingFields IS NOT NULL AND LEN(@MappingFields)>0 BEGIN
	IF LEFT(@MappingFields,1)='{' BEGIN
		INSERT INTO @SplitMappingFields(OrderBy,[MappingFields]) SELECT IDD,[VALUE] FROM zz.StrSplit1(@MappingFields,'{}',DEFAULT)		
		IF @cntFrom<>@@ROWCOUNT BEGIN
			RAISERROR('Несоответствие числа параметров [zz].[TbltoXml].@From и [zz].[TbltoXml].@MappingFields', 16, 1)
			RETURN	
		END
	END ELSE BEGIN
		IF @cntFrom>1 BEGIN
			RAISERROR('Неправильный параметр [zz].[TbltoXml].@MappingFields', 16, 1)
			RETURN	
		END
		INSERT INTO @SplitMappingFields(OrderBy,[MappingFields]) SELECT 1,@MappingFields         	
	END
END

IF @TimeStamp IS NOT NULL AND LEN(@TimeStamp)>0 BEGIN
	IF LEFT(@TimeStamp,1)='{' BEGIN
		INSERT INTO @SplitTimeStamp(OrderBy,[TimeStamp]) SELECT IDD,[VALUE] FROM zz.StrSplit1(@TimeStamp,'{}',DEFAULT)
		IF @cntFrom<>@@ROWCOUNT BEGIN
			RAISERROR('Несоответствие числа параметров [zz].[TbltoXml].@From и [zz].[TbltoXml].@TimeStamp', 16, 1)
			RETURN	
		END				
	END ELSE BEGIN
		IF @cntFrom>1 BEGIN
			RAISERROR('Неправильный параметр [zz].[TbltoXml].@TimeStamp', 16, 1)
			RETURN	
		END         	
		INSERT INTO @SplitTimeStamp(OrderBy,[TimeStamp]) SELECT 1,@TimeStamp
	END
END

DECLARE @cur_Schema NVARCHAR(MAX), @cur_OrderBy INT
DECLARE @cur_From NVARCHAR(MAX), @cur_Sql NVARCHAR(MAX), @cur_Fields NVARCHAR(MAX), @cur_MappingFields NVARCHAR(MAX), @cur_TimeStamp NVARCHAR(MAX) 
DECLARE @cur_MappingFields_Variable NVARCHAR(MAX), @cur_MappingFields_Value  NVARCHAR(MAX)

DECLARE cur CURSOR LOCAL FAST_FORWARD READ_ONLY FOR
SELECT OrderBy, [From] FROM @SplitFrom ORDER BY OrderBy
OPEN cur
FETCH FROM cur INTO @cur_OrderBy, @cur_From
WHILE @@FETCH_STATUS = 0 BEGIN

	IF OBJECT_ID('tempdb..#TbltoXml_EmptyTable') IS NOT NULL DROP TABLE #TbltoXml_EmptyTable 
	CREATE TABLE #TbltoXml_EmptyTable(_zz_ BIT)
	
	SET @S=''
	SET @cur_Schema='schema'
	SET @cur_Sql=NULL;				SELECT @cur_Sql=NULLIF(LTRIM(RTRIM([SQL])),'') FROM @SplitSql WHERE OrderBy=@cur_OrderBy 
	SET @cur_Fields=NULL;			SELECT @cur_Fields=NULLIF(LTRIM(RTRIM([Fields])),'') FROM @SplitFields WHERE OrderBy=@cur_OrderBy
	SET @cur_MappingFields=NULL;	SELECT @cur_MappingFields=NULLIF(LTRIM(RTRIM([MappingFields])),'') FROM @SplitMappingFields WHERE OrderBy=@cur_OrderBy
	SET @cur_TimeStamp=NULL;		SELECT @cur_TimeStamp=NULLIF(LTRIM(RTRIM([TimeStamp])),'') FROM @SplitTimeStamp WHERE OrderBy=@cur_OrderBy
	IF @cur_MappingFields IS NOT NULL BEGIN
		SET @cur_MappingFields=REPLACE(@cur_MappingFields,'[','')
		SET @cur_MappingFields=REPLACE(@cur_MappingFields,']','')
		SET @cur_MappingFields=LOWER(@cur_MappingFields)
	END

	EXEC zz.TblCopySchema
		 @From = @cur_From
		,@To = '#TbltoXml_EmptyTable'
		,@Fields = @cur_Fields
		,@TimeStamp = @cur_TimeStamp
		,@Script = @cur_Schema OUT
	
	IF @cur_Schema IS NULL GOTO nextTable
	SET @cur_Schema=LOWER(@cur_Schema)
	
	SELECT @S=@S
	+CASE WHEN s2.[VALUE] LIKE '%geography%' OR s2.[VALUE] LIKE '%geometry%' OR s2.[VALUE] LIKE '%xml%' THEN 'CONVERT(NVARCHAR(MAX),' ELSE '' END
	+'[F].'+LOWER(s2.VARIABLE)
	+CASE WHEN s2.[VALUE] LIKE '%geography%' OR s2.[VALUE] LIKE '%geometry%' OR s2.[VALUE] LIKE '%xml%' THEN ') '+s2.VARIABLE ELSE '' END
	+CASE WHEN s3.[VALUE] IS NULL THEN '' ELSE ' ['+s3.[VALUE]+']' END 
	+','
	FROM zz.StrSplit1(@cur_Schema,'{}',DEFAULT) s1
	CROSS APPLY zz.StrSplit2(s1.[VALUE],';','=',DEFAULT) s2
	LEFT JOIN zz.StrSplit2(@cur_MappingFields,';','>',DEFAULT) s3 ON '['+LOWER(s3.VARIABLE)+']'=LOWER(s2.VARIABLE)
	ORDER BY s1.IDD, s2.IDD
	SET @S=NULLIF(RTRIM(LTRIM(@S)),'')
	
	IF LEN(@S)>0 BEGIN
		IF RIGHT(@S,1)=',' BEGIN
			SET @S=LEFT(@S,LEN(@S)-1)
		END
		SET @S='SELECT '+@S+' FROM '+@cur_From+' [F] WITH (NOLOCK) '+ISNULL(@cur_Sql,'')+' FOR XML RAW(''row''), TYPE, BINARY BASE64'
	END

	IF @cur_MappingFields IS NOT NULL BEGIN
		DECLARE cur_mf CURSOR LOCAL FAST_FORWARD READ_ONLY FOR
		SELECT [VARIABLE],[VALUE] FROM zz.StrSplit2(@cur_MappingFields,';','>',DEFAULT) ORDER BY IDD
		OPEN cur_mf
		FETCH FROM cur_mf INTO @cur_MappingFields_Variable, @cur_MappingFields_Value 
		WHILE @@FETCH_STATUS = 0 BEGIN
			SET @cur_Schema=REPLACE(@cur_Schema,'{['+@cur_MappingFields_Variable+']','{['+@cur_MappingFields_Value+']')	
		FETCH FROM cur_mf INTO @cur_MappingFields_Variable, @cur_MappingFields_Value
		END; CLOSE cur_mf; DEALLOCATE cur_mf
	END
		
	INSERT INTO #Schema(OrderBy, [Table], Fields, [SQL]) SELECT @cur_OrderBy, @cur_From, @cur_Schema, @S	
	
	nextTable:
FETCH FROM cur INTO @cur_OrderBy, @cur_From
END; CLOSE cur; DEALLOCATE cur

IF NOT EXISTS (SELECT TOP 1 * FROM #Schema) RETURN

SET @S='    (SELECT ''table_''+CONVERT(NVARCHAR(MAX),OrderBy) [table], [Table] [fromtable], fields FROM #Schema FOR XML RAW(''row''), TYPE, BINARY BASE64) AS [schema]'
SELECT @S=@S+'
   ,('+s.[SQL]+') AS [table_'+CONVERT(NVARCHAR(MAX),s.OrderBy)+']'  
FROM #Schema s

SET @S='SET @To=(
SELECT
'+@S+'
FOR XML PATH(''''), TYPE, ROOT(''root'')
)'

EXEC sp_executesql @S, N'@To XML OUTPUT',@To=@To OUT

END TRY
BEGIN CATCH
	DECLARE @ErrorMessage NVARCHAR(4000), @ErrorSeverity INT, @ErrorState INT
    SELECT @ErrorMessage = ERROR_MESSAGE(), @ErrorSeverity = ERROR_SEVERITY(), @ErrorState = ERROR_STATE();
    RAISERROR (@ErrorMessage, @ErrorSeverity,  @ErrorState)
    RETURN 0
END CATCH

GO

/****** Object:  StoredProcedure [zz].[Validate]    Script Date: 05.02.2021 16:01:27 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE PROCEDURE [zz].[Validate]
	@validation NVARCHAR(50)=NULL,				--название времянки, например #validation
	@Text NVARCHAR(255)=NULL,					--текст сообщения
	@Level VARCHAR(3) = 'err',					--уровень ошибки: 'inf' - информационное сообщение, 'wrn' - предупреждение, 'err' - ошибка
	@spName NVARCHAR(255) = NULL,				--название хранимки, которая сгенерировала сообщение							
	@Mode INT=1,								--0 - добить времянку полями
												--1 - вставить сообщение
	@Note NVARCHAR(MAX) = NULL,					--примечание по ошибке
	@isRaiserror BIT=0 							--если @isRaiserror=1 и в валидацию добавляется сообщение, то оно продублируется RAISERROR(..., 10, 1)
AS
BEGIN TRY
SET NOCOUNT ON;
IF @validation IS NULL RETURN 
DECLARE @Msg NVARCHAR(255), @ParmDefinition NVARCHAR(MAX), @SQL NVARCHAR(MAX)

EXEC zz.TblCheck @validation,'@validation' 
	
IF @Mode = 0 BEGIN
	EXEC zz.TblAddField @To=@validation,@Fields='[text] NVARCHAR(255); [note] NVARCHAR(MAX); [level] VARCHAR(3) NOT NULL CHECK ([Level] IN (''inf'',''wrn'',''err'')); [orderBy] INT IDENTITY(1,1) NOT NULL; [spName] NVARCHAR(255); fdm DATETIME DEFAULT(GETDATE())'
	SET @SQL = 
	'
		BEGIN TRY
			ALTER TABLE '+@validation+' DROP COLUMN i
		END TRY
		BEGIN CATCH
			DECLARE @stub BIT
		END CATCH
	'
	EXEC (@SQL)
END ELSE IF @Mode = 1 BEGIN
	IF @Text IS NULL BEGIN
		SET @Msg='Validate: @Text=NULL'
		RAISERROR(@Msg, 16, 1) 
		RETURN
	END
	SET @Level = LOWER(@Level)
	
	IF ISNULL(@isRaiserror,0)=1 BEGIN
		SET @Msg=LEFT(ISNULL(CONVERT(NVARCHAR(MAX),@Level),'') +' ('+CONVERT(NVARCHAR,GETDATE(),20)+'): '+ISNULL(@Text,''), 255)
		RAISERROR(@Msg, 10, 1) WITH NOWAIT
	END

	SET @ParmDefinition = N'@Text NVARCHAR(255), @Note NVARCHAR(4000), @Level VARCHAR(3), @spName NVARCHAR(255)'
	SET @SQL='INSERT INTO '+@validation+'([Text],[Note],[Level],[spName]) VALUES (@Text, @Note, @Level, @spName)'
	EXEC sp_executesql @SQL, @ParmDefinition
		,@Text=@Text
		,@Note=@Note
		,@Level=@Level
		,@spName=@spName
END
END TRY
BEGIN CATCH
	EXEC zz.[Error]
END CATCH

GO

/****** Object:  StoredProcedure [zz].[zzenerate_sp]    Script Date: 05.02.2021 16:01:27 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

-- =============================================
-- Author:		a9
-- Create date: 04.20.2021
-- Description:	формирование шаблона хранимой процедуры
-- =============================================
CREATE PROCEDURE [zz].[zzenerate_sp]
	@type VARCHAR(50)=NULL	-- варианты:
							--	NULL		- базовый шаблон
							--  'v'			- базовый шаблон + валидация
							--  't'			- базовый шаблон + транзакция
							--	'vt'		- базовый шаблон + валидация + транзакция
AS
BEGIN
	SET NOCOUNT ON;
	DECLARE @text TABLE (orderBy INT IDENTITY(1,1), t NVARCHAR(MAX))

	INSERT INTO @text(t) SELECT '-- ============================================='
	INSERT INTO @text(t) SELECT '-- Author: '
	INSERT INTO @text(t) SELECT '-- Create date: ' + CONVERT(NVARCHAR(MAX), GETDATE(), 104)
	INSERT INTO @text(t) SELECT '-- Description: '
	INSERT INTO @text(t) SELECT '-- ============================================='
	INSERT INTO @text(t) SELECT 'CREATE PROCEDURE [%schema%].[%sp%]'
	INSERT INTO @text(t) SELECT '	@lid UNIQUEIDENTIFIER,'
	INSERT INTO @text(t) SELECT '	@cid UNIQUEIDENTIFIER' + CASE WHEN @type IN ('v','vt') THEN ',' ELSE '' END
	IF @type IN ('v','vt') BEGIN
		INSERT INTO @text(t) SELECT '	@validation NVARCHAR(50)=''CREATE'''
	END
	INSERT INTO @text(t) SELECT 'AS'
	INSERT INTO @text(t) SELECT 'BEGIN TRY'
	INSERT INTO @text(t) SELECT 'SET NOCOUNT ON;'
	INSERT INTO @text(t) SELECT 'DECLARE @spName NVARCHAR(50) = OBJECT_NAME(@@procID)' + CASE WHEN @type IN ('t','vt') THEN '; DECLARE @tran BIT = 0' ELSE '' END
	IF @type IN ('v','vt') BEGIN
		INSERT INTO @text(t) SELECT ''
		INSERT INTO @text(t) SELECT '--#region валидация'
		INSERT INTO @text(t) SELECT 'DECLARE @ValidationRoot BIT = 0'
		INSERT INTO @text(t) SELECT 'IF @validation IS NOT NULL BEGIN'
		INSERT INTO @text(t) SELECT '	IF @validation = ''CREATE'' BEGIN'
		INSERT INTO @text(t) SELECT '		SET @validation = ''#validation'''
		INSERT INTO @text(t) SELECT '		SET @ValidationRoot = 1'
		INSERT INTO @text(t) SELECT '		IF OBJECT_ID(''tempdb..#validation'') IS NULL CREATE TABLE #validation(i INT)'
		INSERT INTO @text(t) SELECT '		EXEC [zz].[Validate] @validation = @validation, @Mode = 0'
		INSERT INTO @text(t) SELECT '	END ELSE BEGIN'
		INSERT INTO @text(t) SELECT '		EXEC zz.TblCheck @Value = @validation, @Name = ''@validation'''
		INSERT INTO @text(t) SELECT '	END'
		INSERT INTO @text(t) SELECT 'END'
		INSERT INTO @text(t) SELECT '--#endregion валидация'
		INSERT INTO @text(t) SELECT ''
		INSERT INTO @text(t) SELECT '--#region пример валидации, посмотрел - удали'
		INSERT INTO @text(t) SELECT 'IF @validation IS NOT NULL BEGIN'
		INSERT INTO @text(t) SELECT '	EXEC zz.Validate @validation = @validation, @Text = ''Это инфо'', @Note = ''Примечание'', @spName = @spName, @Level = ''inf'''
		INSERT INTO @text(t) SELECT 'END'
		INSERT INTO @text(t) SELECT 'IF @validation IS NOT NULL BEGIN'
		INSERT INTO @text(t) SELECT '	EXEC zz.Validate @validation = @validation, @Text = ''Это предупреждение'', @Note = ''Примечание'', @spName = @spName, @Level = ''wrn'''
		INSERT INTO @text(t) SELECT 'END'
		INSERT INTO @text(t) SELECT 'IF 1 <> 1 BEGIN'
		INSERT INTO @text(t) SELECT '	IF @validation IS NOT NULL BEGIN'
		INSERT INTO @text(t) SELECT '		EXEC zz.Validate @validation = @validation, @Text = ''Это ошибка'', @Note = ''Примечание'', @spName = @spName'
		INSERT INTO @text(t) SELECT '	END ELSE BEGIN'
		INSERT INTO @text(t) SELECT '		GOTO exx'
		INSERT INTO @text(t) SELECT '	END'
		INSERT INTO @text(t) SELECT 'END'
		INSERT INTO @text(t) SELECT '--#endregion пример валидации, посмотрел - удали'
	END
	IF @type IN ('t','vt')	BEGIN
		INSERT INTO @text(t) SELECT ''
		INSERT INTO @text(t) SELECT 'SET @tran=CASE WHEN @@TRANCOUNT>0 THEN 0 ELSE 1 END'
		INSERT INTO @text(t) SELECT 'IF @tran=1 BEGIN TRAN'
		INSERT INTO @text(t) SELECT ''
		INSERT INTO @text(t) SELECT '	--тут код в транзации, это примечание удали'
		INSERT INTO @text(t) SELECT ''
		INSERT INTO @text(t) SELECT 'IF @tran=1 AND @@TRANCOUNT>0 COMMIT'
	END

	INSERT INTO @text(t) SELECT ''
	IF @type IN ('v','vt','t') BEGIN
		INSERT INTO @text(t) SELECT 'exx:'
	END
	IF @type IN ('v','vt') BEGIN
		INSERT INTO @text(t) SELECT 'IF @ValidationRoot = 1 BEGIN'
		INSERT INTO @text(t) SELECT '	EXEC (''SELECT * FROM '' + @validation + '' ORDER BY [orderBy]'')'
		INSERT INTO @text(t) SELECT 'END'
	END
	IF @type IN ('v','vt') BEGIN
		INSERT INTO @text(t) SELECT 'IF @tran=1 AND @@TRANCOUNT>0 ROLLBACK'
	END	
	
	INSERT INTO @text(t) SELECT 'END TRY'
	INSERT INTO @text(t) SELECT 'BEGIN CATCH'
	INSERT INTO @text(t) SELECT '	EXEC zz.[Error] @spName = @spName'
	IF @type IN ('t','vt')	BEGIN
		INSERT INTO @text(t) SELECT '	IF @tran=1 AND @@TRANCOUNT>0 ROLLBACK'
	END
	INSERT INTO @text(t) SELECT 'END CATCH'
	INSERT INTO @text(t) SELECT 'GO'
	INSERT INTO @text(t) SELECT ''
	
	DECLARE @t NVARCHAR(MAX)
	DECLARE cur CURSOR LOCAL FOR
	SELECT t FROM @text ORDER BY orderBy
	OPEN cur
	FETCH FROM cur INTO @t
	WHILE @@FETCH_STATUS = 0 BEGIN
		PRINT  @t
	FETCH FROM cur INTO @t
	END; CLOSE cur; DEALLOCATE cur
	
END

GO

/****** Object:  StoredProcedure [zz].[zzenerate_tbl]    Script Date: 05.02.2021 16:01:27 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

-- =============================================
-- Author:		a9
-- Create date: 04.20.2021
-- Description:	формирование шаблона таблицы
-- =============================================
CREATE PROCEDURE [zz].[zzenerate_tbl]
	@schema VARCHAR(50) = 'dbo',
	@name VARCHAR(50)
AS
BEGIN
	SET NOCOUNT ON;
	DECLARE @text TABLE (orderBy INT IDENTITY(1,1), t NVARCHAR(MAX))

	INSERT INTO @text(t) SELECT 'SET ANSI_NULLS ON'
	INSERT INTO @text(t) SELECT 'GO'
	INSERT INTO @text(t) SELECT 'SET QUOTED_IDENTIFIER ON'
	INSERT INTO @text(t) SELECT 'GO'
	INSERT INTO @text(t) SELECT ''
	INSERT INTO @text(t) SELECT 'CREATE TABLE ['+@schema+'].['+@name+'] ('
	INSERT INTO @text(t) SELECT '	[Rid] [uniqueidentifier] NOT NULL,'
	INSERT INTO @text(t) SELECT '	[Cid] [uniqueidentifier] NOT NULL,'
	INSERT INTO @text(t) SELECT '	[FDM] [datetime] NOT NULL,'
	INSERT INTO @text(t) SELECT '	[LDM] [datetime] NOT NULL,'
	INSERT INTO @text(t) SELECT '	[DDM] [datetime] NULL,'
	INSERT INTO @text(t) SELECT '	[LAT] [timestamp] NOT NULL,'
	
	INSERT INTO @text(t) SELECT		'CONSTRAINT [PK_'+@schema+'_'+@name+'] PRIMARY KEY CLUSTERED ([Rid] ASC)'
	INSERT INTO @text(t) SELECT '	WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]'
	INSERT INTO @text(t) SELECT ') ON [PRIMARY]'
	INSERT INTO @text(t) SELECT 'GO'
	INSERT INTO @text(t) SELECT 'ALTER TABLE ['+@schema+'].['+@name+']  WITH CHECK ADD  CONSTRAINT [FK_'+@schema+'_'+@name+'_dbo_Contractor] FOREIGN KEY([Cid])'
	INSERT INTO @text(t) SELECT 'REFERENCES [dbo].[Contractor] ([Rid])'
	INSERT INTO @text(t) SELECT 'ON UPDATE CASCADE'
	INSERT INTO @text(t) SELECT 'ON DELETE CASCADE'
	INSERT INTO @text(t) SELECT 'GO'
	INSERT INTO @text(t) SELECT 'ALTER TABLE ['+@schema+'].['+@name+'] CHECK CONSTRAINT [FK_'+@schema+'_'+@name+'_dbo_Contractor]'
	INSERT INTO @text(t) SELECT 'GO'
	INSERT INTO @text(t) SELECT ''
	
	DECLARE @t NVARCHAR(MAX)
	DECLARE cur CURSOR LOCAL FOR
	SELECT t FROM @text ORDER BY orderBy
	OPEN cur
	FETCH FROM cur INTO @t
	WHILE @@FETCH_STATUS = 0 BEGIN
		PRINT  @t
	FETCH FROM cur INTO @t
	END; CLOSE cur; DEALLOCATE cur
	
END

GO



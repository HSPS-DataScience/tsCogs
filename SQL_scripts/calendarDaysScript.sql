
/****** Script to generate daily dates from 01/01/2000 -- GETDATE() ******/

CREATE TABLE [SupportReports].[dbo].[calendarDays]([CalendarDate] DATE);
DECLARE @StartDate DATE;
DECLARE @EndDate DATE;
SET @StartDate = '20000101';
SET @EndDate = GETDATE();
WHILE @StartDate <= @EndDate
    BEGIN
        INSERT INTO [SupportReports].[dbo].[calendarDays]([CalendarDate])
               SELECT @StartDate;
        SET @StartDate = DATEADD(dd, 1, @StartDate);
    END;
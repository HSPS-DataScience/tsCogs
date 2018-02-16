
/****** Script to generate hourly dates from 01/01/2000 -- GETDATE() ******/

CREATE TABLE [SupportReports].[dbo].[calendarDateTimeHourly]([CalendarDateTimeHourly] DATETIME);
DECLARE @StartDateTimeHourly DATETIME;
DECLARE @EndDateTimeHourly DATETIME;
SET @StartDateTimeHourly = '20180101';
SET @EndDateTimeHourly = GETDATE();
WHILE @StartDateTimeHourly <= @EndDateTimeHourly
    BEGIN
        INSERT INTO [SupportReports].[dbo].[calendarDateTimeHourly]([CalendarDateTimeHourly])
               SELECT @StartDateTimeHourly;
        SET @StartDateTimeHourly = DATEADD(HH, 1, @StartDateTimeHourly);
    END;
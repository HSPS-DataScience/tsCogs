/****** Script to generate hourly zeros from [SupportReports].[dbo].[calendarDateTimeHourly] ******/

DECLARE @startDateTimeHourly DATETIME= '20141001', @endDateTimeHourly DATETIME= GETDATE();
SELECT DATEADD(HOUR, nbr - 1, @startDateTimeHourly) AS ymd,
       0 AS Count
INTO [SupportReports].[dbo].[hourlyZero]
FROM
(
    SELECT ROW_NUMBER() OVER(ORDER BY c.CalendarDateTimeHourly) AS Nbr
    FROM [SupportReports].[dbo].[calendarDateTimeHourly] c
) AS nbrs
WHERE nbr - 1 <= DATEDIFF(HOUR, @startDateTimeHourly, @endDateTimeHourly);


/****** Script to generate SupportReports.dbo.eClaimDailyProfilesHourly ******/

SELECT AccountNumber,
       CONVERT(DATETIME, DATEPART(hour, [Create_Date])) AS ymd,
       COUNT(*) AS Count
INTO [SupportReports].[dbo].[eClaimDailyProfilesHourly]
FROM [SupportReports].[dbo].[vwTrentonV2]
WHERE Claim_State IN('ACCEPTED', 'SETTLED', 'SETTLED ')
AND (Create_Date <= '2018-01-01 00:00:00.000')
GROUP BY AccountNumber,
         CONVERT(DATETIME, DATEPART(hour, [Create_Date]));


/****** Script to generate SupportReports.dbo.eClaimDailyProfilesExpandedHourly ******/

DECLARE @startDateTimeHourly DATETIME= '20180101', @endDateTimeHourly DATETIME= GETDATE();
SELECT hourlyZeroAccount.AccountNumber,
       hourlyZeroAccount.ymd,
       COALESCE(eClaimDailyProfilesHourly.Count, 0) AS Count
INTO [SupportReports].[dbo].[eClaimDailyProfilesExpandedHourly]
FROM
(
    SELECT AccountNumber,
           ymd
    FROM
(
    SELECT AccountNumber,
           @startDateTimeHourly AS minYMD,
           @endDateTimeHourly AS maxYMD
    FROM [SupportReports].[dbo].[eClaimDailyProfilesHourly]
    GROUP BY AccountNumber
) AS tmp
CROSS JOIN [SupportReports].[dbo].[hourlyZero] AS hourlyZero
    WHERE hourlyZero.ymd BETWEEN tmp.minYMD AND tmp.maxYMD
) AS hourlyZeroAccount
LEFT JOIN [SupportReports].[dbo].[eClaimDailyProfiles] AS eClaimDailyProfiles ON hourlyZeroAccount.AccountNumber = eClaimDailyProfiles.AccountNumber
                                                                           AND hourlyZeroAccount.ymd = eClaimDailyProfiles.ymd
ORDER BY hourlyZeroAccount.AccountNumber,
         hourlyZeroAccount.ymd; 
/****** Script to generate daily zeros from [SupportReports].[dbo].[calendarDate] ******/

DECLARE @startDate DATE= '20141001', @endDate DATE= GETDATE();
SELECT DATEADD(DAY, nbr - 1, @startDate) AS ymd,
       0 AS Count
INTO [SupportReports].[dbo].[dailyZero]
FROM
(
    SELECT ROW_NUMBER() OVER(ORDER BY c.CalendarDate) AS Nbr
    FROM [SupportReports].[dbo].[calendarDays] c
) AS nbrs
WHERE nbr - 1 <= DATEDIFF(DAY, @startDate, @endDate);


/****** Script to generate SupportReports.dbo.eClaimDailyProfiles ******/

SELECT AccountNumber,
       CONVERT(DATE, [Create_Date]) AS ymd,
       COUNT(*) AS Count
INTO [SupportReports].[dbo].[eClaimDailyProfiles3]
FROM [SupportReports].[dbo].[vwTrentonV3]
WHERE Claim_State IN('ACCEPTED', 'SETTLED', 'SETTLED ')
GROUP BY AccountNumber,
         CONVERT(DATE, [Create_Date]);


/****** Script to generate SupportReports.dbo.eClaimDailyProfilesExpanded ******/
DECLARE @startDate DATE= '20141001', @endDate DATE= GETDATE();
SELECT dailyZeroAccount.AccountNumber,
       dailyZeroAccount.ymd,
       COALESCE(eClaimDailyProfiles.Count, 0) AS Count
INTO [SupportReports].[dbo].[eClaimDailyProfilesExpanded3]
FROM
(
    SELECT AccountNumber,
           ymd
    FROM
(
    SELECT AccountNumber,
           @startDate AS minYMD,
           @endDate AS maxYMD
    FROM [SupportReports].[dbo].[eClaimDailyProfiles3]
    GROUP BY AccountNumber
) AS tmp
CROSS JOIN [SupportReports].[dbo].[dailyZero] AS dailyZero
    WHERE dailyZero.ymd BETWEEN tmp.minYMD AND tmp.maxYMD
) AS dailyZeroAccount
LEFT JOIN [SupportReports].[dbo].[eClaimDailyProfiles3] AS eClaimDailyProfiles ON dailyZeroAccount.AccountNumber = eClaimDailyProfiles.AccountNumber
                                                                           AND dailyZeroAccount.ymd = eClaimDailyProfiles.ymd
ORDER BY dailyZeroAccount.AccountNumber,
         dailyZeroAccount.ymd; 
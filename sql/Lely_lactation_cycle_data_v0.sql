-- Clear previous tables
DROP TABLE IF EXISTS [LelyDB].[dbo].[LactationSummary];
DROP TABLE IF EXISTS [LelyDB].[dbo].[LactationSummary_WithLifeNumber];

SELECT COUNT(*) FROM [LelyDB].[dbo].[RemInsemination];

-- Step 1: Raw milking records with gaps
WITH Milkings AS (
  SELECT 
    [MdpId],
    [MdpAniId],
    [MdpProductionDate],
    [MdpDayProduction],
    [MdpDayProductionMAvg],
    [MdpMilkings],
    [MdpFatPercentage],
    [MdpProteinPercentage],
    LAG([MdpProductionDate]) OVER (PARTITION BY [MdpAniId] ORDER BY [MdpProductionDate]) AS [PrevProductionDate]
  FROM [LelyDB].[dbo].[PrmMilkDayProduction]
  WHERE [MdpDayProductionMAvg] IS NOT NULL
    AND [MdpProductionDate] > '2016-10-31'
),

-- Step 2: Detect new lactation cycles based on 7-day gap
LactationFlags AS (
  SELECT *,
    CASE 
      WHEN [PrevProductionDate] IS NULL THEN 1
      WHEN DATEDIFF(DAY, [PrevProductionDate], [MdpProductionDate]) > 7 THEN 1
      ELSE 0
    END AS [NewLactationFlag]
  FROM Milkings
),

-- Step 3: Number lactation cycles per cow
LactationNumbered AS (
  SELECT *,
    SUM([NewLactationFlag]) OVER (
      PARTITION BY [MdpAniId] ORDER BY [MdpProductionDate]
      ROWS UNBOUNDED PRECEDING
    ) AS [CalculatedLactationCycle]
  FROM LactationFlags
),

-- Step 4: Match each calving date to the first milking after calving
MatchedLactation AS (
  SELECT 
    L.*,
    R.[LacNumber] AS [RemLactation_LacNumber],
    R.[LacId],
    R.[LacCalvingDate],
    R.[LacRemarks],
    R.[LacColostrumDate],
    ROW_NUMBER() OVER (
      PARTITION BY R.[LacAniId], R.[LacCalvingDate]
      ORDER BY DATEDIFF(DAY, R.[LacCalvingDate], L.[MdpProductionDate])
    ) AS rn
  FROM LactationNumbered L
  INNER JOIN [LelyDB].[dbo].[RemLactation] R
    ON L.[MdpAniId] = R.[LacAniId]
    AND R.[LacCalvingDate] > '2016-10-31'
    AND L.[MdpProductionDate] >= R.[LacCalvingDate]
    AND DATEDIFF(DAY, R.[LacCalvingDate], L.[MdpProductionDate]) <= 30
),

-- Step 4b: Only keep the first milking match per calving event
BestMatches AS (
  SELECT * FROM MatchedLactation
  WHERE rn = 1
),

-- Step 5: Get start + next calving date per cow and lactation
LactationBoundaries AS (
  SELECT
    B.[MdpAniId],
    B.[CalculatedLactationCycle],
    B.[LacCalvingDate],
    B.[RemLactation_LacNumber],
    B.[LacId],
    B.[LacColostrumDate],
    B.[LacRemarks],
    MIN(L.[MdpProductionDate]) AS [milk_production_start_date],
    LEAD(B.[LacCalvingDate]) OVER (PARTITION BY B.[MdpAniId] ORDER BY B.[LacCalvingDate]) AS [NextCalvingDate]
  FROM BestMatches B
  LEFT JOIN LactationNumbered L
    ON B.[MdpAniId] = L.[MdpAniId]
    AND L.[MdpProductionDate] >= B.[LacCalvingDate]
    AND L.[CalculatedLactationCycle] = B.[CalculatedLactationCycle]
  GROUP BY B.[MdpAniId], B.[CalculatedLactationCycle], B.[LacCalvingDate], B.[RemLactation_LacNumber], B.[LacId], B.[LacColostrumDate], B.[LacRemarks]
),

-- Step 6: For each cycle, find the last milking before 7-day gap or next calving
LactationEnds AS (
  SELECT
    LB.*,
    MAX(LN.[MdpProductionDate]) AS [milk_production_end_date]
  FROM LactationBoundaries LB
  INNER JOIN LactationNumbered LN
    ON LN.[MdpAniId] = LB.[MdpAniId]
    AND LN.[MdpProductionDate] >= LB.[milk_production_start_date]
    AND LN.[MdpProductionDate] < ISNULL(LB.[NextCalvingDate], '9999-12-31')
    AND LN.[CalculatedLactationCycle] = LB.[CalculatedLactationCycle]
  WHERE 
    NOT EXISTS (
      SELECT 1
      FROM LactationNumbered LN2
      WHERE 
        LN2.[MdpAniId] = LN.[MdpAniId]
        AND LN2.[MdpProductionDate] > LN.[MdpProductionDate]
        AND LN2.[CalculatedLactationCycle] = LN.[CalculatedLactationCycle]
        AND DATEDIFF(DAY, LN.[MdpProductionDate], LN2.[MdpProductionDate]) > 7
    )
  GROUP BY LB.[MdpAniId], LB.[CalculatedLactationCycle], LB.[LacCalvingDate], LB.[RemLactation_LacNumber], LB.[LacId], LB.[LacColostrumDate], LB.[LacRemarks], LB.[milk_production_start_date], LB.[NextCalvingDate]
),

-- Step 7: Final summary with milk production data
LactationSummary AS (
  SELECT
    E.[MdpAniId] AS [AniId],
    E.[RemLactation_LacNumber],
    E.[CalculatedLactationCycle],
    E.[LacCalvingDate],
    E.[milk_production_start_date],
    E.[milk_production_end_date],
    E.[LacId],
    E.[LacRemarks],
    E.[LacColostrumDate],
    DATEDIFF(DAY, E.[milk_production_start_date], E.[milk_production_end_date]) AS [lactation_duration],
    SUM(LN.[MdpDayProduction]) AS [total_milk_production],
    AVG(LN.[MdpFatPercentage]) AS [mean_fat_percent],
    AVG(LN.[MdpProteinPercentage]) AS [mean_protein_percent]
  FROM LactationEnds E
  INNER JOIN LactationNumbered LN
    ON LN.[MdpAniId] = E.[MdpAniId]
    AND LN.[MdpProductionDate] BETWEEN E.[milk_production_start_date] AND E.[milk_production_end_date]
    AND LN.[CalculatedLactationCycle] = E.[CalculatedLactationCycle]
  GROUP BY 
    E.[MdpAniId], E.[RemLactation_LacNumber], E.[CalculatedLactationCycle],
    E.[LacCalvingDate], E.[milk_production_start_date], E.[milk_production_end_date], E.[LacId], E.[LacColostrumDate]
, E.[LacRemarks]),

-- Step 8: Add dry-off interval and still-milking flag
WithDryOffs AS (
  SELECT 
    *,
    [milk_production_end_date] AS [dry_off_date],
    LEAD([milk_production_start_date]) OVER (
      PARTITION BY [AniId] ORDER BY [CalculatedLactationCycle]
    ) AS [next_milk_start_date],
    CASE 
      WHEN MAX([milk_production_end_date]) OVER (PARTITION BY [AniId]) = [milk_production_end_date] THEN 1
      ELSE 0
    END AS [still_milking]
  FROM LactationSummary
)

-- Final output
SELECT 
  [AniId],
  [RemLactation_LacNumber],
  [CalculatedLactationCycle],
  [LacCalvingDate],
  [LacId],
  [LacRemarks],
  [LacColostrumDate],
  [milk_production_start_date],
  [milk_production_end_date],
  [lactation_duration],
  [total_milk_production],
  [mean_fat_percent],
  [mean_protein_percent],
  [dry_off_date],
  DATEDIFF(DAY, [milk_production_end_date], [next_milk_start_date]) AS [dry_off_interval],
  [still_milking]
INTO [LelyDB].[dbo].[LactationSummary]
FROM WithDryOffs
ORDER BY [AniId], [CalculatedLactationCycle];

-- Join with animal ID
SELECT 
    lac.*, 
    ani.[AniLifeNumber]
INTO [LelyDB].[dbo].[LactationSummary_WithLifeNumber]
FROM [LelyDB].[dbo].[LactationSummary] AS lac
LEFT JOIN [LelyDB].[dbo].[HemAnimal] AS ani
    ON lac.[AniId] = ani.[AniId]
ORDER BY ani.[AniLifeNumber], lac.[CalculatedLactationCycle];

-- Output full results
SELECT * FROM [LelyDB].[dbo].[LactationSummary_WithLifeNumber];

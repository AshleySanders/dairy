# Diagnostics Index

## Missing_MilkProd_Data.R
- Identifies lactation cycles where cows have calved but show no milk production afterward
- Joins RemLactation, PrmMilkDayProduction, HemAnimal, and animal_history
- Adds exit code information to distinguish between open lactation and culling-related gaps


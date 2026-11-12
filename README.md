# Longitudinal Friendship Network Data
The dataset was collected from 47 students with the same major at a university in southern China. Data collection began shortly after the students entered the university as freshmen and continued through their graduation and transition to postgraduation life. The study employed multiple survey instruments at different temporal resolutions, including a baseline survey, 30 weekly follow-up surveys, 9 longer interval (10-weeks in between) follow-up surveys, and two additional surveys conducted during and after graduation. These surveys collected friendship network data, demographic information, psychological characteristics, and social and health-related behaviors.

### Data files
- recoded-files-final: contain all clean, recoded data files
- survey_collection_dates.csv: data collection dates

### Code files
- clean.R: cleaning raw data
- recode.R: recoding cleaned data
- validate_updated.R: check for overall data validity 
- validity_constructs.R: check for specific questionnaire validity
- validity_cov_weekly.R: check for weekly collected covariates validity
- missingness.R: explore missing data patterns
- attention.R: attention check item visualization
- postgrad.R: analyze questions on postgrad plans

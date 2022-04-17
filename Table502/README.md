# Table 502.20 Visualization

## Dashboard Title: 

1990 - 2019 Labor Statistics in the U.S. by Educational Attainment and Gender


## How to Access:
### Github (depedency packages are needed))
shiny::runGitHub('NISS_Competition_2022', 'xingyanwang-david', subdir = "Table502")

### Dashboard (no depedency packages are needed)
https://xingyanwang-david.shinyapps.io/Table502_20/


### Interpretation and educational function of the dashboard

This interactive map and its associated line plots delivered fruitful information on the National Assessment of Educational Progress (NAEP) Reading Scores of the United States from 1998 to 2019. Based on the data visualizations by the interactive map and the line plots, here is a summary based on our dashboard. Firstly, we found an overall increasing trend in average NAEP reading scores from 1998 to 2013, but there were significant score declines for many states in recent few years (from 2017 to 2019). Secondly, the geographical distribution of reading scores was uneven. According to the interactive map presenting the scores throughout the years, we observed that the Northeast, North Central, and Midwest regions were generally above the national average, while the South and West regions were below the national average. More specifically, Massachusetts had the highest average score and DC fell behind all the other states throughout the data years.

### Issues of accessibilities of the dashboard

The reading scores for Washington, DC were significantly less than other states which may need additional investigation to justify this pattern observed from the NAEP dataset. We need more details of how the NAEP scores were calculated from its sample population and then figure out the reasons for Washington, DC’s lag. Another notable issue was that most states' reading scores declined a lot from 2017 to 2019. We checked the website of NAEP and found that they claimed, "beginning with the 2017 assessment, NAEP reading results are from a digitally based assessment; prior to 2017, results were from a paper-and-pencil-based assessment." The change of assessment method might result in a variety of reading score standards. Also, with the transition from paper-based to digitally based assessment, many school districts without broadband access may not be able to properly collect and report the NAEP scores; further, this led to a potential decrease in its sample size and contribute to the decrease of the score estimation. Thus, the interpretation of the observed downward patterns in recent years might need extra detail to rationalize.

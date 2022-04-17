# Table 502.20 Visualization

## Dashboard Title: 

1990 - 2019 Labor Statistics in the U.S. by Educational Attainment and Gender


## How to Access:
### Github (depedency packages are needed))
shiny::runGitHub('NISS_Competition_2022', 'xingyanwang-david', subdir = "Table502")

### Dashboard (no depedency packages are needed)
https://xingyanwang-david.shinyapps.io/Table502_20/


### Interpretation and educational function of the dashboard

This application focuses on visualizing the median annual earnings, number, and percentage of full-time, year-round workers aged 25 and over by the highest level of educational attainment and gender from 1990 to 2019. Our team developed an R shiny application with interactive graphics to illustrate the trend of U.S. labor statistics. The users can select the labor statistics of interest and visualize the overall trend in population and higher education by gender groups in the default setting. Users are allowed to modify the display options to generate the trend across all degree and gender groups, change the time to either continuous or categorical variable, and add a 95% confidence interval. The generated figure displays the longitudinal trend of the selected labor statistics. Users can click the legends of gender and education types to choose the specific groups in the graphic. This allows users to compare the outcome across gender and education levels with any combination. Although our application provides a simple and interactive way to present the results by education attainment and gender, there are too many legends on the top of the figure, and users might need some time to unselect the disinterested groups.


### Issues of accessibilities of the dashboard

The graphic for the first outcome demonstrates the longitudinal change of median annual income in the current dollar for full-time, year-round workers aged 25 and over from 1990 to 2019. The overall median annual income within gender groups trended up since 1990. Except for 2002, the revenue declined and might be related to the early 2000s recessions.  At the same education level, the median annual income of men was higher than that of women from 1990 to 2019. In terms of the overall higher education group, the gender earnings gap increased over time. The graphic for the second outcome demonstrates the longitudinal change of median annual income for full-time, year-round workers aged 25 and over from 1990 to 2019, adjusted for the consumer price index. Female overall median annual income showed two episodes in the evolution of earning, the first lasting from 1990 to 2000 and the second from 2000 to 2019. The income achieved a significant increase in 2000, stayed at a similar level, and rose again after 2014. The overall median annual income of men reached its peak in 2000 and has fluctuated since then. The income continued to increase after reached its minimal value in 2014. At the same education level, the median annual income of men was higher than that of women from 1990 to 2019. 

The graphic for the third outcome demonstrates the longitudinal change in the number of full-time, year-round workers aged 25 and over from 1990 to 2019. The overall employment within gender groups trended up since 1990 except for a short-term decline from 2007 to 2009. At the same education level, the male full-time workers outnumbered female workers from 1990 to 2019. In terms of the overall higher education group, the gender earning gap decreased over time. The graphic for the fourth outcome demonstrates the longitudinal change in the percentage of full-time, year-round workers aged 25 and over from 2000 to 2019. The most recent overall full-time employment rate within gender groups remained at the same level as in 2000. The percentage reached its minimum in 2009.  At the same education level, the full-time employment rate of men was higher than that of women from 2000 to 2019. In conclusion, our application provides users a convenient approach to display and compare the U.S. labor statistics by educational attainment and gender, which potentially benefits the research on gender earning gaps in the U.S.  


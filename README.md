# ESS Timing of Life interactive dashboard

Visualising data for the 3rd and 9th waves of the European Social Survey (ESS) timing of life module. Currently the user can create boxplots for the three questions regarding attitudes towards child-bearing ages, and compare differences between countries by gender, data collection year, gender asked about, and cohort.

More questions will be added soon in a similar format to the three present.

A map drawer is also being added.

Issues/tasks: \

Add select all/deselect all option to the country filter \
Display mean, meadian values on or under the boxplots, possibly using geom_annotate() \
Add education selection in the demographics section based on ISCO codes \
Get rid of whitespace from the data counter "(n = #### )" -> "(n = ####)" (.noWS = ) does not seem to work for renderText() objects \
In case of a missing answer on any of the three questions, the respondent was discarded as missing data. Find a way to minimise data loss from this. \

You can view the current updated app at https://gregoryswart.shinyapps.io/ess-app/ \
Data used is available at https://www.europeansocialsurvey.org/
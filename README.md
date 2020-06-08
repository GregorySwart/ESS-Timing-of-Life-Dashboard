# ESS Timing of Life interactive dashboard

Visualising data for the 3rd and 9th waves of the European Social Survey (ESS) timing of life module. Currently the user can create boxplots for the three questions: "Before what age would you say a woman/man is generally too young to become a mother/father?", "In your opinion, what is the ideal age for a girl/boy or woman/man to become a mother/father?", "After what age would you say a woman/man is generally too old to consider having any more children?".

The questionnaire was split evenly between asking about a woman becoming a mother and a man becoming a father. This distinction is not made in the app currently.

Issues/tasks: \\
Get rid of whitespace from the data counter "(n = #### )" -> "(n = ####)" (.noWS = ) does not seem to work for renderText() objects \\
Find something worthwhile to put in navbar pages and main panel tabs or remove them. \\
Find map drawer for R and possibly include spacial analysis. \\
Sort out the split ballot issue. \\
In case of a missing answer on any of the three questions, the respondent was discarded as missing data. Find a way to minimise data loss from this. \\

You can view the current updated app at https://gregoryswart.shinyapps.io/ess-app/ \\
Data used is available at https://www.europeansocialsurvey.org/
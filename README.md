## Visualization of Covid-19 in R Shiny

### Overview

This project seeks to provide supplemental visualizations similar to other dashboards that have leveraged confirmed case/ death count data for the COVID-19 pandemic. This data has been provided by the <a href='https://coronavirus.jhu.edu/map.html' target='_blank'>COVID-19 Dashboard by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University</a>.

The visualization methodology employed here utilizes the same approach as posited by the <a href='https://aatishb.com/covidtrends/' target='_blank'>Covid Trends</a> dashboard by Aatish Bhatia. This method produces a 2-D plot of a tracking meteric on the x-axis (confirmed cases or death counts), with the daily rate of change in this total on the y-axis (taken as the 7 day moving average).

In this way, one can ideally obtain a quick reference to a region's current status and trajectory. A positive linear relationship between these two data elements indicates local exponential behavior. By contrast, a negative relationship indicates the rate of new cases is decreasing. Lastly, a constant rate of change will indicate that the diesease is progressing at a linear rate.

### Instructions


#### Running the source from RStudio
- Clone the repository or download the project as a zip file

- Navigate to and open <tt>app.R</tt>

- Execute the appplication by clicking "Run App> on the top right section of the window

#### Running from shinyapps.io

- NYI
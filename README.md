# COVID19-TRACKER

Hi everyone, I wanted to share a website I recently developed an app/website to help track and map the #covid19 cases in the U.S. The app can be accessed from here: [COVID19 Tracker].

![COVID-19 Cases U.S. Map!](/Screenshots/covidapp.png "COVID-19 Cases U.S. Map")

*Updated 5/23:*

Additional interactive charts have been added to this app to visualize county-level case growth statistics. The underlying data is also being automatically updated twice daily via connection to the [NYT GitHub repository].

![COVID-19 County-Level Charts!](/Screenshots/covidapp2.PNG "COVID-19 County-Level Charts")

**Notes on the Code:**

Code Structure:
* Data.R: This is the script used to pre-process COVOID case data as well as census information and geographical shapes. The output includes three R data files saved under the Shiny folder.
* Shiny folder: This is the underlying code for the Shiny application. Read more about R Shiny [here](https://shiny.rstudio.com/). 

Main packages utilized for data visualization include plotly (for interative graphics), leaflet and tigris (for geomapping).

**Background:**

On March 27th, the NYT made public their Coronavirus case data, which is tracked to the county level and fills the gap as no single agency has provided the public with an accurate, up-to-date record of coronavirus cases, according to NYT.

You can also read more reporting on this from the following [NYT website].

[COVID19 Tracker]:      http://covidtracker.schen.org
[NYT website]:          https://lnkd.in/eBzgDw6
[NYT GitHub repository]: https://github.com/nytimes/covid-19-data

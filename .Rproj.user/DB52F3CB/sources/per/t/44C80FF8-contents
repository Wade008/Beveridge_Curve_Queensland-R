#Bring in all installed packages
library(readabs)

library(tidyverse)

library(dplyr)

library(stringr)

library(lubridate)

library(tidyr)

library(fy)

library(psych)

library(plyr)

library(readxl)

library(plotly)

library(formattable)

#**************************************************************************************************************

#Variables and data
#import vacancy rate data from excel file
vac_data_monthly <- read_xlsx(file.choose(), 1)


#Get labour force data from the ABS
lf_monthly <- "6202.0"

startDate <- as_date("2006-01-01")

endDate <- today()


#Labour force indicators

abs_labour_force_base <- read_abs(cat_no = lf_monthly, 
                                 tables = 12,
                                 series_id = NULL,
                                 path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                                 metadata = TRUE,
                                 show_progress_bars = FALSE,
                                 retain_files = FALSE,
                                 check_local = FALSE
)

#filter data on date variables

abs_labour_force_base <- abs_labour_force_base %>%
  filter(between(abs_labour_force_base$date,startDate,endDate))

#split out series column
abs_labour_force_base <- abs_labour_force_base %>%
  separate_series()

#monthly labour force figures

lf_monthly_qld  <- abs_labour_force_base %>%
  filter(series_type %in% "Seasonally Adjusted",
         series_1 %in% "Labour force total", 
         series_2 %in% "Persons",
         series_3 %in% "Queensland")


#monthly unemployment rate

ue_monthly_qld <- abs_labour_force_base %>% 
  filter(series_type %in% "Seasonally Adjusted",
         series_1 %in% "Unemployment rate", 
         series_2 %in% "Persons",
         series_3 %in% "Queensland")


#calculate the vacancy rate
vac_rate_calc <- inner_join(vac_data_monthly, lf_monthly_qld, by="date") %>%
  mutate(vacancy_rate = (vacancies_sa / (value * 1000)) * 100 ) %>%
  select(c(date, vacancy_rate))
  


# create table for chart

bc_chart_data <- inner_join(vac_rate_calc, ue_monthly_qld, by="date") %>%
  mutate(year = as.character(year(date))) %>%
  select(c(date, vacancy_rate , value, year)) %>% 
  dplyr::rename(unemployment_rate = value)


# create scatter plot

#set chart margins
mrg <- list(l = 80, r = 80,
            b = 80, t = 90)


# prep the annotation based on max date

m <- bc_chart_data[which.max(bc_chart_data$date), ]

a <- list(
  x = m$unemployment_rate,
  y = m$vacancy_rate,
  text = format(m$date,'%b %Y'),
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 5,
  arrowsize=0.5,
  ax=-30,
  ay=-30
)



fig <- plot_ly(data=bc_chart_data, 
               type="scatter",
               mode="markers",
               x= ~unemployment_rate, y= ~vacancy_rate, 
               color= ~year, 
               colors="Set1",
               text = ~format(date,'%b %Y'),
               hovertemplate = paste('<b>Date</b>: %{text}',
                                     '<br><b>Unemployment rate</b>: %{x:.1f}%',
                                     '<br><b>Vacancy rate</b>: %{y:.1f}%')
               )

fig <- fig %>% 
  layout(
      title=list(text="Beveridge Curve - Queensland",
                 font=list(size=25)), 
      plot_bgcolor = "#e5ecf6",
      xaxis=list(title = list(text="Unemployment rate (%)", font = list(size = 15), standoff = 20), 
                 dtick=0.5, 
                 tickmode = "linear",
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 range=list(2.5, max(bc_chart_data$unemployment_rate)+0.5)), 
      yaxis=list(title= list(text="Vacancy rate (%)", font = list(size = 15), standoff = 20), 
                 dtick=0.5, 
                 tickmode = "linear",
                 rangemode="tozero",
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 range=list(0,4)),
      legend = list(title=list(text='<b>Year recorded</b>')),
      annotations = a,
      margin=mrg
    )

fig



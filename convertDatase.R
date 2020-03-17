library(readxl)
time_series_19_covid_March12_v2xls <- read_excel("time_series_19-covid_March12_v2xls.xls", 
                                                 sheet = "Confirmed_cases")
View(time_series_19_covid_March12_v2xls)

# transpose dataframe
newdf=t(time_series_19_covid_March12_v2xls)
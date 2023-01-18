library(tidyr)
library(dplyr)
library(stringr)
library(qwraps2)
library(DT)
library(vtable)
library(ggplot2)
library("sf")
library(dplyr)
library(lubridate)






################### PRÁCTICA 2 #######################

# Read data
data <- read.csv("owid-covid-data.csv")


# Transformación de fechas

data$date <- as.Date(data$date, format =  "%Y-%m-%d")

data_date <-  data %>%
  mutate(date = as.Date(date)) %>% 
  mutate_at(vars(date), funs(year, month, day))


data_date_group <- data_date %>%
  group_by(location,year, month) %>%
  summarise(deaths_million=max(total_deaths_per_million),
            cases_million=max(total_cases_per_million),
            icu_million=max(icu_patients_per_million),
            rep_rate=mean(reproduction_rate),
            excess_mortality=sum(excess_mortality),
            vacc_hundred=max(people_fully_vaccinated_per_hundred))


data_date_group$date <- paste(data_date_group$year, data_date_group$month, sep="_", collapse=NULL)


# Elimino los países con menos de 30 fechas registradas

data_date_group2 <- data_date_group[data_date_group$location %in% names(which(table(data_date_group$location) > 29)), ]

dates_sel <- data_date_group$date[data_date_group$location =="North Korea"]

# Seleccionamos países con información para 19 meses

data_date_group2 <- data_date_group2[data_date_group2$date %in%  dates_sel,]


## Crear el dataframe con las columnas de cada uno de los 19 meses

t_data <- t(data_date_group2[4:9])


col1 <- seq(1, 4218, 19)
listdfs <- lapply(col1, function(x) t_data[ , x:(x+18)])

listdfs <- lapply(listdfs, as.data.frame)


for (i in seq_along(listdfs)){
  colnames(listdfs[[i]]) <- colnames(t_data)[1:19]
  
}

df<- data.table::rbindlist(listdfs)

colnames(df) <- dates_sel
df$countries <- countries


## Creación del dataframe definitivo

countries <- rep(unique(data_date_group2$location), each=6)

var_study <- rep(colnames(data_date_group)[4:9], 222)


colnames(df) <- dates_sel

write.table(df, "dataset_def_PRAC2_vd.tsv",dec=",")


save.image(file="Pract2_VD_14012023_2.RData")

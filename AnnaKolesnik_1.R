#Колесник Анна, вариант 1
#регион 8, Калмыкия
# рассчитать урожайность пшеницы в 2000 году, 
#взяв для рассчета средние суммы активных температур за предыдущие 7 лет, с 10 ближайших метеостанций
#но убирая из рассчета активных температур дни с температурой выше 27 градусов
#
#
rm(list=ls())
setwd("C:/Anna_in_R")
getwd()
#install.packages("tidyverse")
#install.packages("rnoaa")
library(tidyverse)
library(rnoaa)
#скачиваем станции (потом закомментировать)
station_data = ghcnd_stations()
#записываем в файл для последующей работы (потом тоже закомментировать)
#write.csv(station_data, "stations.csv")
station_data = read.csv("stations.csv")


#После получения всписка всех станций, получите список станций ближайших к столице вашего региона,

#создав таблицу с именем региона и координатами его столицы

elista = data.frame(id = "ELISTA", latitude = 46.307780,  longitude = 44.255830)

elista_around = meteo_nearby_stations(lat_lon_df = elista, station_data = station_data, limit = 10, var = c("PRCP", "TAVG"), year_min = 1993, year_max = 1999)

#elista_around это список единственным элементом которого является таблица, содержащая идентификаторы 

#метеостанций отсортированных по их 

# удалленности от Элисты, очевидно что первым элементом таблицы будет идентификатор метеостанции Элисты,

#его то мы и попытаемся получить

? meteo_nearby_stations

elista_id = elista_around[["ELISTA"]][["id"]][1]

#получение всех данных с метеостанций

summary (elista_id)

str(elista_around)

all_elista_data = meteo_tidy_ghcnd(stationid = elista_id)

#2)чтобы получить таблицу всех метеостанций вокруг Элисты нужно выбрать целиком первый объект из списка

elista_table = elista_around[[1]]

summary(elista_table)

#в таблице elista_table оказалось 10 объектов, ранжированных по расстоянию от Элисты

#нужно убедится, что этот список включает нужные по условию задачи метеостанции



elista_stations = elista_table 

str(elista_stations)

#Таким образом, мы сформировали список необходимых станций, посмотрим, что он содержит

elista_stations$id


# Создание цикла, в который загружаются необходимые данные с метеостанций 

# Промежуточный объект, куда скачиваются данные с конкретной метеостанции
all_elista_data = meteo_tidy_ghcnd(stationid = elista_id)

#посмотрим, что же скачивается
?meteo_tidy_ghcnd

summary(all_elista_data)

#скачиваются данные только с самой первой станции

#Подумаем, какие из этих данных нам нужны

##нам нужны среднесуточные температуры (tavg) выше 10 и меньше 27 градусов за 1993-1999 гг.

###Нужно создать цикл, в котором бы скачивались нужные данные для всех метеостанций из созданного списка

#Создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции

all_i = data.frame()

#Создадим объект, куда скачаем все данные всех метеостанций

all_elista_meteodata = data.frame()

#Цикл для всех метеостанций

for(i in 1:10)
  
{
  
  all_i = meteo_tidy_ghcnd(stationid = elista_around[["ELISTA"]][["id"]][i])
  
  
  
  #выберем нужные свойства
  
  all_i = all_i[ ,c("id","date","tavg")]
  
  #с помощью команды rbind соединяем данные, полученные на предыдущих и данном #этапах цикла
  
  print(all_i)
  
  all_elista_meteodata=rbind(all_elista_meteodata, all_i)
  
}

#Записываем полученные результаты

write.csv(all_elista_meteodata,"all_elista_meteodata.csv")

#2 часть

################## 4. Разбивка даты на составляющие(год, месяц, день года)

# считываем данные из файла all_elista_meteodata.csv

all_elista_meteodata = read.csv("all_elista_meteodata.csv")
#посмотрим на данные

str(all_elista_meteodata)

#видим, что дата записана в формате "1963-01-02"

#ищем библиотеку из tidyverse, которая может помочь с датой

library(lubridate)

# вытащить год

#проверим, что работает

y = year(all_elista_meteodata$date); y

all_elista_meteodata [,"year"]= year(all_elista_meteodata$date)

#добавим месяц

all_elista_meteodata [,"month"]= month(all_elista_meteodata$date)

#вытащить день от начала года

all_elista_meteodata [,"day_of_the_year"]= yday(all_elista_meteodata$date)

#проверим результат

str(all_elista_meteodata)

#отфильтруем данные за 1993-1999

years_elista_meteodata = filter (all_elista_meteodata, year > 1993 & year < 1999 )

#проверим результат

str(years_elista_meteodata)

summary (years_elista_meteodata)

################## 5. Средняя (по годам и метеостанциям) сумма активных температур за месяц

#Изучаем формулу и видим, что единственное, что нужно расчитать

#- это сумму температур больше 10 град. по месячно, остальное в формуле- константы



#### 1. температурy нужно поделить на 10

years_elista_meteodata[,"tavg"]= years_elista_meteodata$tavg / 10

summary (years_elista_meteodata)

#### 2. Превратим в нули все NA и где tavg больше 10 градусов



years_elista_meteodata [is.na(years_elista_meteodata$tavg), "tavg"] = 0

years_elista_meteodata [years_elista_meteodata$tavg<10, "tavg"] = 0

years_elista_meteodata [years_elista_meteodata$tavg>27, "tavg"] = 0

#проверяем, что температура получилась в или 0 или больше 10 градусов

summary(years_elista_meteodata)

#### 3. суммарная температура за месяц за 10 лет для всех станций

# группирую по метеостанциям, годам и месяцам

alldays= group_by(years_elista_meteodata,id,year,month)

#функция summarize применяет некоторые действия к отдельным группам, полученным

#с помощью функции group_by

#просуммирую температуру по этим группам с помощью sum

sumT_alldays_elista = summarize(alldays, tsum = sum(tavg))

#Получилось - все года, все месяца присутствуют

# максимальная суммарная температура за месяц 27.0

summary(sumT_alldays_elista)


#Сгруппирем данные по месяцам

groups_elista_months = group_by(sumT_alldays_elista,month)

groups_elista_months

#найду для всех метеостанций и ВСЕХ лет среднее по месяцам

sumT_months= summarize(groups_elista_months , St = mean(tsum))

sumT_months

################## 6. Подготовка к расчету по формуле Урожая

### Ввод констант

afi=c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)

bfi=c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)

di=c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)

y = 1.0 # - коэффициент для экспозиции склона - считаем, что все поля идеально ровные;

Kf = 300 # - коэффициент использования ФАР посевом;

Qj = 1600 # - калорийность урожая культуры;

Lj = 2.2 # - сумма частей основной и побочной продукции;

Ej = 25 # - стандартная влажность культуры;

# Рассчитаем Fi по месяцам

#Fi= afi+bfi∗y∗(St>10℃)

sumT_months = mutate(sumT_months, Fi = afi+bfi*y*St)

#Рассчитаем Yi

sumT_months = mutate(sumT_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100 - Ej)))

## Расчитываем урожай как сумму по месяцам и думаем разумный ли он

Yield = sum(sumT_months$Yi)

Yield

# Ответ: 77.50 ц/га 
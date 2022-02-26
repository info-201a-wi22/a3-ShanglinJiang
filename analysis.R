incarceration <- read.csv(file = 'https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv')
head(incarceration)
view(incarceration)


library(dplyr)

sum_1970_2018 <- with(incarceration, sum(total_pop[fips == '1001']))
AL <- filter(incarceration, fips == '1001')
annual_pop_AL <- AL[ , c(6)] 
sum_1990_2018 <- with(incarceration, sum(total_pop[fips == '1001' & year > '1989']))
sum_1990_2018_15to64 <- with(incarceration, sum(total_pop_15to64[fips == '1001' & year > '1989']))
sum_aapi_1990_2018_15to64 <- with(incarceration, sum(aapi_pop_15to64[fips == '1001' & year > '1989']))
sum_black_1990_2018_15to64 <- with(incarceration, sum(black_pop_15to64[fips == '1001' & year > '1989']))
sum_latinx_1990_2018_15to64 <- with(incarceration, sum(latinx_pop_15to64[fips == '1001' & year > '1989']))
sum_native_1990_2018_15to64 <- with(incarceration, sum(native_pop_15to64[fips == '1001' & year > '1989']))
sum_white_1990_2018_15to64 <- with(incarceration, sum(white_pop_15to64[fips == '1001' & year > '1989']))

prop_aapi_1990_2018 <- (sum_aapi_1990_2018_15to64 / sum_1990_2018_15to64)
prop_black_1990_2018 <- (sum_black_1990_2018_15to64 / sum_1990_2018_15to64)
prop_latinx_1990_2018 <- (sum_latinx_1990_2018_15to64 / sum_1990_2018_15to64)
prop_native_1990_2018 <- (sum_native_1990_2018_15to64 / sum_1990_2018_15to64)
prop_white_1990_2018 <- (sum_white_1990_2018_15to64 / sum_1990_2018_15to64)


library(ggplot2)
chart1
  p_line=ggplot(AL)+
    geom_line(aes(x=AL[,2],y=AL[,6]),color="red")+
    geom_line(aes(x=AL[,2],y=AL[,7]),color="blue")+
    geom_line(aes(x=AL[,2],y=AL[,10]),color="yellow")+
    geom_line(aes(x=AL[,2],y=AL[,11]),color="black")+
    geom_line(aes(x=AL[,2],y=AL[,12]),color="brown")+
    geom_line(aes(x=AL[,2],y=AL[,13]),color="gray")+
    geom_line(aes(x=AL[,2],y=AL[,14]),color="white")
  p_line
  p_line +xlab("Years")+
    ylab("Population in jail")+
    ggtitle('Population changes in jail') 
  
chart2
  p_line_2=ggplot(AL)+
    geom_line(aes(x=AL[,2],y=AL[,10]),color="yellow")+
    geom_line(aes(x=AL[,2],y=AL[,13]),color="gray")
  p_line_2
  p_line_2 +xlab("Years")+
    ylab("Population in jail")+
    ggtitle('Comparison of prison population of Asian&Islander and Native') 
  
  install.packages("maps")
  library(maps)
  map('county', region = c('Autauga County'),
      fill = TRUE, col = rainbow(3), mar = c(2, 3, 4, 3))
  title("Autauga County")
  
  
  library(maps)
  install.packages("tmap")
  library(tmap) 
  library(leaflet) 
  library(ggplot2) 
  library(dplyr)
  library(tidyverse)
  install.packages("ggmap")
  library(ggmap)
  
  map <- get_stamenmap(
    bbox = c(left = -87.4141, bottom = 32.0640, right = -85.9172, top = 32.9499),
    maptype = "toner",
    zoom = 10
  )
  
  ggmap(map)
  

  
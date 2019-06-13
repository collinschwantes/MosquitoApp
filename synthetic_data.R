## synthetic data 

library(dplyr)
library(stringr)
library(lubridate)
library(tibble)
library(purrr)
library(ggplot2)

### need to make this happen at 200 locations

r <- 0.2 
k <- 30000 
n <- 100
t <- 1:50

pops <- list()

FullData <- list()

sites <- paste0("A",1:200) 

for(k in 1:length(sites)){
 
pops[[1]] <- n
  
for(i in 2:length(t)) {
  
  nt <- pops[i-1][[1]]
  e <- runif(n = 1,min = -.1,max = .1)
  pops[i] <- nt + (r+e)*nt*(1-(nt/k)) 

  print(pops[i])  
}
  
counts <-  map_dbl(.x = pops,.f = rbind)

lat <- 38.922807 + runif(1,min = -0.00001, max  = 0.00001)
lon <- -77.068259 + runif(1,min = -0.00001, max  = 0.00001)

SynData <- data.frame(
  dateCollected = Sys.Date() + 1:50,
  TrapType = "Ovisposition",
  Species = "Culex pipiens",
  Site = sites[k],
  Sex = "Female",
  lat = lat,
  lon = lon,
  count =  round(counts,digits = 0)
)

FullData[[k]] <- SynData

}


SynData %>% 
  ggplot(aes(x = Date,y = count)) +
  geom_point()

### add noise in dates
### add noise in counts



dir.create("./SyntheticData")

SynData %>% write.csv(file = "./SyntheticData/Culex.csv")


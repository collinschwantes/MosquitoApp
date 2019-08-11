## synthetic data 

library(dplyr)
library(stringr)
library(lubridate)
library(tibble)
library(purrr)
library(ggplot2)

### need to make this happen at 200 locations


pops <- list()

FullData <- list()

sites <- paste0("A",1:200) 

for (j in 1:length(sites)) {
 
  r <- 0.2 
  k <- 3000 
  n <- 10
  t <- 1:50  
  
pops[[1]] <- n
  
for (i in 2:length(t)) {
  
  nt <- pops[i-1][[1]]
  e <- runif(n = 1,min = -.1,max = .1)
  pops[i] <- nt + (r+e)*nt*(1-(nt/k)) 

  #print(pops[i])  
}
  
counts <-  map_dbl(.x = pops,.f = rbind)

lat <- 38.922807 + runif(1,min = -0.05, max  = 0.05)
lon <- -77.068259 + runif(1,min = -0.05, max  = 0.05)

SynData <- data.frame(
  dateCollected = Sys.Date() + 1:50,
  TrapType = "Ovisposition",
  Species = "Culex pipiens",
  Site = sites[j],
  Sex = "Female",
  lat = lat,
  lon = lon,
  count =  round(counts,digits = 0)
)

FullData[[j]] <- SynData

}


SynFull <- do.call(rbind,FullData)

summary(SynFull)

SynFull %>% 
  filter(count == -Inf)

SynFull %>% 
  ggplot(aes(x = dateCollected,y = count)) +
  geom_point()

SynFull %>% 
  ggplot(aes(x = lon,y = lat)) +
  geom_point()

### add noise in dates
### add noise in counts



dir.create("./SyntheticData")

SynFull %>% write.csv(file = "./SyntheticData/Culex.csv")


head(SynFull,10) %>% write.csv(file = "./SyntheticData/ExampleCulex.csv")


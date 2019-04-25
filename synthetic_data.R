## synthetic data 

library(dplyr)
library(stringr)
library(lubridate)
library(tibble)
library(purrr)
library(ggplot2)

r <- 0.2 
k <- 30000 
n <- 100
t <- 1:50

pops <- list()
  
pops[[1]] <- n
  
for(i in 2:length(t)) {
  
  nt <- pops[i-1][[1]]
  e <- runif(n = 1,min = -.1,max = .1)
  pops[i] <- nt + (r+e)*nt*(1-(nt/k)) 

  print(pops[i])  
}
  

counts <-  map_dbl(.x = pops,.f = rbind)

SynData <- data.frame(
  Date = Sys.Date() + 1:50,
  TrapType = "Ovisposition",
  Species = "Culex pipiens",
  Sex = "Female",
  count =  round(counts,digits = 0)
)


SynData %>% 
  ggplot(aes(x = Date,y = count)) +
  geom_point()


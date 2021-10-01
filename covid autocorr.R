library(sf)
library(spdep)
library(tigris)
library(acs)
library(tidyverse)
library(tmap)
s1 <- read_sf("june 22/Admin2.shp")
s <- read_sf("Admin2.shp")
s1$confirmed<-log(0.01 + s1$confirmed)
s1
library(spdep)
nb <- poly2nb(s1, queen=TRUE)
nb[[5]]
#lw1<- spweights.constants(lw, zero.policy=TRUE, adjust.n=TRUE)

lw <- nb2listw(nb, style="W", zero.policy=TRUE)
Inc.lag <- lag.listw(lw, s1$confirmed)

M <- lm(Inc.lag ~ s1$confirmed)
plot( Inc.lag ~ s1$confirmed, pch=20, asp=1, las=1)
coef(M)[2]

n <- 599L   # Define the number of simulations
I.r <- vector(length=n)  # Create an empty vector

for (i in 1:n){
  # Randomly shuffle income values
  x <- sample(s1$confirmed, replace=FALSE)
  # Compute new set of lagged values
  x.lag <- lag.listw(lw, x)
  # Compute the regression slope and store its value
  M.r    <- lm(x.lag ~ x)
  I.r[i] <- coef(M.r)[2]
}

hist(I.r, main=NULL, xlab="Moran's I", las=1)
abline(v=coef(M)[2], col="red")
##
N.greater <- sum(coef(M)[2] > I.r)

p <- min(N.greater + 1, n + 1 - N.greater) / (n + 1)
p
#####
moran.test(s1$confirmed,lw, zero.policy=TRUE, na.action=na.omit)

MC<- moran.mc(s1$confirmed, lw, nsim=599,zero.policy=TRUE, na.action=na.omit)
plot(MC, main="", las=1)
MC
###############################
library(tmap)
tm_shape(s) + tm_polygons(style="quantile", col = "confirmed", palette="Greens") +
  tm_legend(outside = TRUE, text.size = .8) 
----------------------------------------------
neighbours <- poly2nb(s1)
neighbours
listw <- nb2listw(neighbours, style="W", zero.policy=TRUE)
globalMoran <- moran.test(s1$confirmed, listw, zero.policy=TRUE, na.action=na.omit)
globalMoran
listw
local <- localmoran(x = s1$confirmed, listw )#= nb2listw(neighbours, style = "W", zero.policy=TRUE, NAOK = NAOK))
summary(local)
moran.map <- cbind(s1, local)
tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          title = "local moran statistic",
          palette = "Greens",)+
  tm_borders(alpha = 0.8) +
  tm_layout(main.title = "covid", main.title.size = 0.7)+
            #legend.position = c("right", "bottom"), legend.title.size = 0.8)
  tm_legend(outside = TRUE, text.size = .8) 
-------------------------------------------------------------
--------------------------------------------------------------

###########lisa
  #scale the variable of interest and save it to a new column
s1$s_confirmed <- scale(s1$confirmed) %>% as.vector()
s1$lag_s_confirmed <- lag.listw(listw, s1$s_confirmed)
summary(s1$lag_s_confirmed)
x <- s1$s_confirmed
y <- s1$lag_s_confirmed
xx <- data_frame(x,y)
moran.plot(x, listw)
#dataframe$new_variable <- ifelse(dataframe$some_numeric_var < 100, "smaller than 100", "not smaller than 100")
s1 <- st_as_sf(s1)  
  mutate(quad_sig = ifelse(s1$s_confirmed > 0 & 
                             s1$lag_s_confirmed > 0 & 
                             local[,5] <= 0.2, 
                           "high-high",
                           ifelse(s1$s_confirmed <= 0 & 
                                    s1$lag_s_confirmed <= 0 & 
                                    local[,5] <= 0.2, 
                                  "low-low", 
                                  ifelse(s1$s_confirmed > 0 & 
                                           s1$lag_s_confirmed <= 0 & 
                                           local[,5] <= 0.2, 
                                         "high-low",
                                         ifelse(s1$s_confirmed <= 0 & 
                                                  s1$lag_s_confirmed > 0 & 
                                                  local[,5] <= 0.2,
                                                "low-high", 
                                                "outliers")))))
table(s1$quad_sig)
nrow(local[local[,5] <= 0.05,])
qtm(s1, fill="quad_sig", fill.title="LISA")

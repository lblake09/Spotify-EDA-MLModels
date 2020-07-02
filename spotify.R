
library(plyr)
library(dplyr)
library(MASS)
library(glmm)
library(standardize)
install.packages("rmarkdown")
spotify = read.csv("~/Desktop/Spring 2020/Kaagle Data Sets/top2010s.csv", header = TRUE, sep = ",")
names(spotify)
summary(spotify) ## 276 females 
list(spotify$artist)
summary(spotify$artist)
levels(spotify$artist)


### cleaning data by renaming artists with special charecters in name
levels(spotify$artist)[levels(spotify$artist) == "Beyonc\x8e"] <- "Beyonce"
levels(spotify$artist)[levels(spotify$artist) == "Emeli Sand\x8e"] <- "Emili Sande"
levels(spotify$artist)[levels(spotify$artist) == "B\xafRNS"] <- "BORNS"
levels(spotify$artist)[levels(spotify$artist) == "M\xaf"] <- "MO"
levels(spotify$title)[levels(spotify$title) == "BloodPop\xa8"] <- "Bloodpop"
levels(spotify$artist)
summary(spotify$artist)
                        ######### 2010 Decade ###########
                        ######### 2010 Decade ###########
                        ######### 2010 Decade ###########

par(mfrow=c(1,1))
## Creating a varible for Women by subsetting the data + plotting  
WomenOf2010s = droplevels((subset(spotify, gender == "F", select = artist && title)))
list(WomenOf2010s)
summary(WomenOf2010s$artist)
plot(WomenOf2010s$artist, las = 2, cex.names = .5, ylim= c(0,20), xlab = "Artist", ylab= "Number of Songs", main = "Top Female Artist on Spotify: 2010-2019 ", col = "green", pch = 20)
Popularity2010sWomen = droplevels(subset(WomenOf2010s, pop < 100, select = artist && title))
summary(Popularity2010sWomen) 
summary(WomenOf2010s$year)


##  Creating a varible for Men by subsetting the data + plotting  
MenOf2010s = droplevels((subset(spotify, gender == "M", select = artist && title)))
list(MenOf2010s)
summary(MenOf2010s$artist)
plot(MenOf2010s$artist, las = 2, cex.names = .5, ylim = c(0,20), xlab = "Artist", ylab= "Number of Songs", main = "Top Male Artist on Spotify: 2010-2019 ", col = "green2")
Popularity2010sMen = droplevels(subset(MenOf2010s, pop < 100, select = artist && title))
summary(Popularity2010sMen) 

## Plotting the Top Genre's  
summary(spotify$top.genre)
plot(spotify$top.genre, las = 2, cex.names = .5, ylim= c(0,350), xlab = "Genre", ylab= "Number of Songs", main = "Top Genre", col = "green", pch = 20)
                        

                                ######## Subseting data down to each year + sex   ###########
                                ######## + visualizing them and listing the songs ##########
                                ######## from most popular to least popluar each year #########
                       


                        ######### 2010 ###########
                        ######### 2010 ###########
                        ######### 2010 ###########

      # subseting all data to the of 2010
all2010 = (subset(spotify, year == 2010))
all2010 
par(mfrow=c(1,2))

      # subseting data to display all data of female artists in 2010
WomenOf2010 = droplevels((subset(all2010, gender == "F", select = artist && title)))
list(WomenOf2010)
summary(WomenOf2010$artist)
plot(WomenOf2010$artist, las = 2, cex.names = .5, xlab = "Artist", ylab= "Number of Songs", main = "Top Female Artist on Spotify: 2010", col = " limegreen ")
      # subseting data to display all data of male artists in 2010
MenOf2010 = droplevels((subset(all2010, gender == "M", select = artist && title)))
list(MenOf2010)
summary(MenOf2010$artist)
plot(MenOf2010$artist, las = 2, cex.names = .5, xlab = "Artist", ylab= "Number of Songs", main = "Top Male Artist on Spotify: 2010", col = "limegreen")


      # listing songs from 2010 most pop to least 
Popularity2010 = droplevels(subset(all2010, pop < 83, select = artist && title))
Popularity2010

                    ######### 2011 ###########
                    ######### 2011 ###########
                    ######### 2011 ###########

  # subseting all data to the of 2011
all2011 = (subset(spotify, year == 2011))
all2011 
  # subseting data to display all data of female artists in 2011
WomenOf2011 = droplevels((subset(all2011, gender == "F", select = artist && title)))
list(WomenOf2011)
summary(WomenOf2011$artist) # 22 of the 51 top songs in 2011 were made by 10 different women
plot(WomenOf2011$artist, las = 2, cex.names = .5, xlab = "Artist", ylab= "Number of Songs", main = "Top Women on Spotify: 2011", col = "green")
  # subseting data to display all data of male artists in 2011
MenOf2011 = droplevels((subset(all2011, gender == "M", select = artist && title)))
list(MenOf2011)
summary(MenOf2011$artist)
plot(MenOf2011$artist, las = 2, cex.names = .5, xlab = "Artist", ylab= "Number of Songs", main = "Top Male Artist on Spotify: 2011", col = "green")
  # listing songs from 2011 most pop to least 
Popularity2011 = droplevels(subset(all2011, pop < 100, select = artist && title))
Popularity2011



                    ######### 2012 ###########
                    ######### 2012 ###########
                    ######### 2012 ###########

# subseting all data to the of 2012
all2012 = (subset(spotify, year == 2012))
all2012
# subseting data to display all data of female artists in 2011
WomenOf2012 = droplevels((subset(all2012, gender == "F", select = artist && title)))
list(WomenOf2012)
summary(WomenOf2012$artist) # 22 of the 51 top songs in 2011 were made by 10 different women
plot(WomenOf2012$artist, las = 2, cex.names = .5, xlab = "Artist", ylab= "Number of Songs", main = "Top Women on Spotify: 2012", col = "green")
# subseting data to display all data of male artists in 2011
MenOf2012 = droplevels((subset(all2012, gender == "M", select = artist && title)))
list(MenOf2012)
summary(MenOf2012$artist)
plot(MenOf2012$artist, las = 2, cex.names = .5, xlab = "Artist", ylab= "Number of Songs", main = "Top Male Artist on Spotify: 2012", col = "green")
# listing songs from 2011 most pop to least 
Popularity2012 = droplevels(subset(all2011, pop < 100, select = artist && title))
Popularity2012

                  ######### 2013 ###########
                  ######### 2013 ###########
                  ######### 2013 ###########

# subseting all data to the of 2013
all2013 = (subset(spotify, year == 2013))
all2013
# subseting data to display all data of female artists in 2013
WomenOf2013 = droplevels((subset(all2013, gender == "F", select = artist && title)))
list(WomenOf2013)
summary(WomenOf2013$artist) # 22 of the 51 top songs in 2013 were made by 10 different women
plot(WomenOf2013$artist, las = 2, cex.names = .5, xlab = "Artist", ylab= "Number of Songs", main = "Top Women on Spotify: 2013", col = "green")
# subseting data to display all data of male artists in 2013
MenOf2013 = droplevels((subset(all2013, gender == "M", select = artist && title)))
list(MenOf2013)
summary(MenOf2013$artist)
plot(MenOf2013$artist, las = 2, cex.names = .5, xlab = "Artist", ylab= "Number of Songs", main = "Top Male Artist on Spotify: 2013", col = "green")
# listing songs from 2013 most pop to least 
Popularity2013 = droplevels(subset(all2013, pop < 100, select = artist && title))
Popularity2013


                  ######### 2014 ###########
                  ######### 2014 ###########
                  ######### 2014 ###########

# subseting all data to the of 2014
all2014 = (subset(spotify, year == 2014))
all2014
# subseting data to display all data of female artists in 2013
WomenOf2014 = droplevels((subset(all2014, gender == "F", select = artist && title)))
list(WomenOf2014)
summary(WomenOf2014$artist) # 22 of the 51 top songs in 2013 were made by 10 different women
plot(WomenOf2014$artist, las = 2, cex.names = .5, xlab = "Artist", ylab= "Number of Songs", main = "Top Women on Spotify: 2014", col = "green")
# subseting data to display all data of male artists in 2013
MenOf2014 = droplevels((subset(all2014, gender == "M", select = artist && title)))
list(MenOf2014)
summary(MenOf2014$artist)
plot(MenOf2014$artist, las = 2, cex.names = .5, xlab = "Artist", ylab= "Number of Songs", main = "Top Male Artist on Spotify: 2014", col = "green")
# listing songs from 2013 most pop to least 
Popularity2014 = droplevels(subset(all2014, pop < 100, select = artist && title))
Popularity2014


                  ######### 2015 ###########
                  ######### 2015 ###########
                  ######### 2015 ###########

# subseting all data to the of 2015
all2015 = (subset(spotify, year == 2015))
all2015
# subseting data to display all data of female artists in 2013
WomenOf2015 = droplevels((subset(all2015, gender == "F", select = artist && title)))
list(WomenOf2015)
summary(WomenOf2015$artist) # 22 of the 51 top songs in 2013 were made by 10 different women
plot(WomenOf2015$artist, las = 2, cex.names = .5, xlab = "Artist", ylab= "Number of Songs", main = "Top Women on Spotify: 2015", col = "green")
# subseting data to display all data of male artists in 2013
MenOf2015 = droplevels((subset(all2015, gender == "M", select = artist && title)))
list(MenOf2015)
summary(MenOf2015$artist)
plot(MenOf2015$artist, las = 2, cex.names = .5, ylim= c(0,10), xlab = "Artist", ylab= "Number of Songs", main = "Top Male Artist on Spotify: 2015", col = "green")
# listing songs from 2013 most pop to least 
Popularity2015 = droplevels(subset(all2015, pop < 100, select = artist && title))
Popularity2015


                  ######### 2016 ###########
                  ######### 2016 ###########
                  ######### 2016 ###########

# subseting all data to the of 2016
all2016 = (subset(spotify, year == 2016))
all2016
# subseting data to display all data of female artists in 2013
WomenOf2016 = droplevels((subset(all2016, gender == "F", select = artist && title)))
list(WomenOf2016)
summary(WomenOf2016$artist) # 22 of the 51 top songs in 2013 were made by 10 different women

summary(WomenOf2016$artist) 
plot(WomenOf2016$artist, las = 2, cex.names = .5, xlab = "Artist", ylab= "Number of Songs", main = "Top Women on Spotify: 2016", col = "green")
# subseting data to display all data of male artists in 2013
MenOf2016 = droplevels((subset(all2016, gender == "M", select = artist && title)))
list(MenOf2016)
summary(MenOf2016$artist)
plot(MenOf2016$artist, las = 2, cex.names = .5, xlab = "Artist", ylab= "Number of Songs", main = "Top Male Artist on Spotify: 2016", col = "green")
# listing songs from 2013 most pop to least 
Popularity2016 = droplevels(subset(all2016, pop < 100, select = artist && title))
Popularity2016

                    ######### 2017 ###########
                    ######### 2017 ###########
                    ######### 2017 ###########

# subseting all data to the of 2017
all2017 = (subset(spotify, year == 2017))
all2017
# subseting data to display all data of female artists in 2013
WomenOf2017 = droplevels((subset(all2017, gender == "F", select = artist && title)))
list(WomenOf2017)
summary(WomenOf2017$artist) # 22 of the 51 top songs in 2013 were made by 10 different women
plot(WomenOf2017$artist, las = 2, cex.names = .5, xlab = "Artist", ylab= "Number of Songs", main = "Top Women on Spotify: 2017", col = "green")
# subseting data to display all data of male artists in 2013
MenOf2017 = droplevels((subset(all2017, gender == "M", select = artist && title)))
list(MenOf2017)
summary(MenOf2017$artist)
plot(MenOf2017$artist, las = 2, cex.names = .5, xlab = "Artist", ylab= "Number of Songs", main = "Top Male Artist on Spotify: 2017", col = "green")
# listing songs from 2013 most pop to least 
Popularity2017 = droplevels(subset(all2017, pop < 100, select = artist && title))
Popularity2017



                      ######### 2018 ###########
                      ######### 2018 ###########
                      ######### 2018 ###########

# subseting all data to the of 2018
all2018 = (subset(spotify, year == 2018))
all2018
# subseting data to display all data of female artists in 2013
WomenOf2018 = droplevels((subset(all2018, gender == "F", select = artist && title)))
list(WomenOf2018)
summary(WomenOf2018$artist) # 22 of the 51 top songs in 2013 were made by 10 different women
plot(WomenOf2018$artist, las = 2, cex.names = .5, xlab = "Artist", ylab= "Number of Songs", main = "Top Women on Spotify: 2018", col = "green")
# subseting data to display all data of male artists in 2013
MenOf2018 = droplevels((subset(all2018, gender == "M", select = artist && title)))
list(MenOf2018)
summary(MenOf2018$artist)
plot(MenOf2018$artist, las = 2, cex.names = .5, xlab = "Artist", ylab= "Number of Songs", main = "Top Male Artist on Spotify: 2018", col = "green")
# listing songs from 2013 most pop to least 
Popularity2018 = droplevels(subset(all2018, pop < 100, select = artist && title))
Popularity2018



                    ######### 2019 ###########
                    ######### 2019 ###########
                    ######### 2019 ###########

# subseting all data to the of 2019
all2019 = (subset(spotify, year == 2019))
all2019
# subseting data to display all data of female artists in 2013
WomenOf2019 = droplevels((subset(all2019, gender == "F", select = artist && title)))
list(WomenOf2019)
summary(WomenOf2019$artist) # 22 of the 51 top songs in 2013 were made by 10 different women
plot(WomenOf2019$artist, las = 2, cex.names = .5, xlab = "Artist", ylab= "Number of Songs", main = "Top Women Artist on Spotify: 2019", col = "green")
# subseting data to display all data of male artists in 2013
MenOf2019 = droplevels((subset(all2019, gender == "M", select = artist && title)))
list(MenOf2019)
summary(MenOf2019$artist)
plot(MenOf2019$artist, las = 2, cex.names = .5, xlab = "Artist", ylab= "Number of Songs", main = "Top Male Artist on Spotify: 2019", col = "green")
# listing songs from 2013 most pop to least 
Popularity2019 = droplevels(subset(all2016, pop < 100, select = artist && title))
Popularity2019

                ##### Multiple Linear Regression Model ######
                ##### Multiple Linear Regression Model ######
                ##### Multiple Linear Regression Model ######

# building the linaer model on spotifys song attribute metrics with popularity
par(mfrow=c(1,1))
scatter.smooth(spotify$pop ~ spotify$dnce + spotify$nrgy + spotify$val + spotify$spch + spotify$acous + spotify$live, xlab = "Dance, Energy, Valance, Speech, Acoustic, Liveliness", ylab = "Popularity", pch =21, main = "Scatter Plot", col = "limegreen", bg = "black", lwd = 1)
popularity.lm <- lm(spotify$pop ~ spotify$dnce + spotify$nrgy + spotify$val + spotify$spch + spotify$acous  + spotify$live, data = spotify)
popularity.lm
summary(popularity.lm)

par(mfrow=c(2,2))
plot(popularity.lm) 
        # 139, 51, 363 seem to be outliers, 443 has high leverage? 
        ### all seem to have errors due to the popularity listed at 0, so we remove them 
spotify[139,]
spotify[51,] 
spotify[363,] 
spotify[443,] # error in metrics
spotify[104,]
spotify[268,]
spotify[362,] 
spotify[442,] 
spotify2 = spotify[-c(139,51,363,443,104,268,362,442),] # removing outliers

## refitting the model without outliers, in conclusion we can see from these models that there is no correaltation 
## betweens spotifys metrics system and a songs popularity, so its more so determined by an artists clout
popularity2.lm <- lm(spotify2$pop ~ spotify2$dnce + spotify2$nrgy + spotify2$val + spotify2$spch + spotify2$acous + spotify2$live, data = spotify2)
popularity2.lm
summary(popularity2.lm)
plot(popularity2.lm) 
popularity.aic = step(popularity.lm, k = log(n))
BIC(popularity.lm)
AIC(popularity.lm)
par(mfrow=c(1,1)) 




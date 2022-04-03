            
                              # Dinanath Dahal
                              # NP000391

install.packages("ggplot2")
library(ggplot2)
install.package("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)

wd <- read.csv("dataset.csv", sep = ",", TRUE)
print(wd)


lgaData<- subset(data, origin == 'LGA')

lgaData

jfkData<- subset(data, origin == 'JFK')
jfkData


summary(data)





#data Exploration


View(wd) #to view dataset on rows and column in r studio

nrow(wd) #to find no of rows on dataset
ncol(wd) #to find no of col in dataset

y=na.omit(wd) #To omit empty column
print(y) 
summary(wd) #Summary of each column of dataset
head(wd,20)
tail(wd,20)
wd[2,2]
wd[3,1:14]
max(wd$temp)



#Analysis 1
#This analysis shows, a scatterplot is drawn between Temperature and Dewpoint
plot(jfkData$temp, jfkData$dewp, main="Temperature & Dewpoint Scatterplot (JFK)", 
     xlab = "Temperature", 
     ylab="Dewpoint", pch = 22, col = "green")
abline(lm(jfkData$dewp ~ jfkData$temp), col = "black", lwd=2)
legend("topleft", legend = paste("cor = ", round(cor(jfkData$temp,
                                                     jfkData$dewp), 1), sep = ""),lty = 1, col = "red")

plot(lgaData$temp, lgaData$dewp, main=" Temperature & Dewpoint Scatterplot(LGA)", 
     xlab = "Temperature(F)", 
     ylab="Dewpoint", pch = 22, col = "green")
abline(lm(lgaData$dewp ~ lgaData$temp), col = "black", lwd=2)
legend("topleft", legend = paste("Corr. = ", 
                                 round(cor(lgaData$temp, lgaData$dewp), 2), 
                                 sep = ""),lty = 1, col = "red")


#Analysis 2
#This analysis shows, a Boxplot is drawn between Dewpoint and Humidity
ggplot(data = data, mapping = aes(x = humid, y = dewp, color = origin)) + 
  geom_boxplot(alpha = 0.2)+ stat_smooth(method = "lm") +
  labs(title = 'Box Plot of Dew Point against humidity',
       x = 'Humidity ', y = 'Dew Point') +
  theme(legend.position = c(0.9, 0.2))
legend("topleft", legend = paste("Corr. (JFK) = ", 
                                 round(cor(jfkData$humid, jfkData$dewp), 
                                       2), sep = ""), lty = 1, col = "red")


#fdvndfljnbldfk 

#Analysis 3
# This analysis shows analysis boxplot of temp
ggplot(data = data, mapping = aes(x = temp, fill = origin)) +
  geom_boxplot() +
  labs(title = "Temperature boxplot", x="temperature (F)") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.7, size = 20)) +
  facet_wrap(~month)

ggplot(data = lgaData, mapping = aes(x = temp)) +
  geom_boxplot() +
  labs(title = "Temperature boxplot (LGA)", x="temperature (F)") +
  facet_wrap(~month)
print(jfkData$month)

cor.tem <- cor.test(x = jfkData$temp, y = jfkData$humid)
cor.tem

#Analysis 4
#This analysis shows, a scatterplot is drawn between Temperature and Humidity
plot(jfkData$humid, jfkData$temp, main=" Temperature & Humidity Scatterplot (JFK)", 
     xlab = "Humidity(H)", 
     ylab="Temperature(T)", pch = 20, col = "green")
abline(lm(jfkData$temp ~ jfkData$humid), col = "black")
legend("topleft", legend = paste("cor = ", round(cor(jfkData$humid, jfkData$temp), 2), 
                                 sep = ""),lty = 1, col = "red")

plot(lgaData$humid, lgaData$temp, main="Temperature and Humidity Scatterplot (LGA)", 
     xlab = "Humidity", 
     ylab="Temperature", pch = 16, col = "green")
abline(lm(lgaData$temp ~ lgaData$humid), col = "black")
legend("topleft", legend = paste("cor = ", round(cor(lgaData$humid, lgaData$temp),
                                                 2), sep = ""),lty = 1, col = "red")


#Analysis 5
#This analysis shows, a boxplot is drawn between Wind Speed and Wind gust speed.
prep
ggplot(data = prep, aes(x = wind_speed, y = wind_gust, color = origin)) +
  geom_boxplot(alpha = 0.4) + stat_smooth(method = "lm") +
  labs(title = "Wind Speed vs Wind gust Speed(box Plot)", 
       x = "Wind Speed (mph)", y= 'Wind Gust speed (mph)') 


# Analysis 6
#This analysis shows, a boxplot is drawn between Wind Speed and Wind gust speed on monthly basis.
ggplot(data = prep, aes(x = wind_speed, y = wind_gust, color = origin, shape = origin)) +
  geom_boxplot(alpha = 0.8) + stat_smooth(method = "lm", se=FALSE) +
  labs(title = "  Wind Speed vs Wind gust Speed (Box Plot)", 
       x = "Wind Speed (mph)", y= 'Wind Gust speed (mph)') +
  facet_wrap(~month)
 
  
  


#Analysis 7
#This analysis shows boxplot is made for Humidity
ggplot(data = data, mapping = aes(x = humid,  fill = origin)) +
  geom_boxplot() +
  labs(title = "boxplot of Humidity", x = "Boxplot", y = "Count") +
  theme(panel.background = element_rect(fill = "purple"),
        panel.grid = element_blank()) +
  facet_wrap(~origin)


#Analysis 8
# This analysis shows Histogram is made for pressure
prep<-na.omit(data)
prep
prep<-na.omit(prep)
prep

ggplot(data = prep, mapping = aes(x = na.omit(pressure) )) +
  geom_histogram(color = "black", fill = "purple") +
  labs(title = "Pressure Histogram", x="Pressure") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.8, size = 25),
        panel.border = element_blank()) +
  facet_wrap(~origin)

#Analysis 9
# This analysis shows, Box plot made for Precipitation against visibility
precipData<- subset(data, precip > 0)
precipData
ggplot(data = precipData, mapping = aes(x = factor(visib) , y = precip, na.rm = TRUE)) +
  geom_point(alpha = 0.9) +
  labs(title = "Box plot Precipitation against Visibility", x = "Visibility", 
       y = "Precipitation (F) ") +
  facet_wrap(~origin) +
  # guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(palette = "Greens", name = "Origin") +
  theme(plot.title = element_text(hjust = 0.7, size = 23),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "lightblue1"),
        axis.text = element_blank(),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 14)) 

  
#Analysis 10
#This analysis shows, Box plot made for Wind gust speed against month
ggplot(data = data, mapping = aes(x = factor(month) , y = wind_gust, na.rm = TRUE, 
                                  color = origin)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Box plot Wind Gust against Month", x = "Month", 
       y = "Wind Gust Speed (Mph)") +
  facet_wrap(~origin) +
  # guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(palette = "Greens", name = "Origin") +
  theme(plot.title = element_text(hjust = 0.7, size = 23),
        
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "lightgreen"),
        
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 14)) 

#Analysis 11
# This analysis shows, histogram  made for visibility.
ggplot(data = prep, mapping = aes(x = visib, color = origin, fill = origin)) +
  geom_histogram() +
  labs(title = "  Visibility Histogram in Miles", x = "visibility(miles)") +
  facet_wrap(~month)


#Analysis 12
# This analysis shows, Box plot made for humidity against month
ggplot(data = prep, mapping = aes(x=factor(month), y = humid, na.rm = TRUE, 
  color = month, fill = month)) +
  geom_boxplot(alpha = 0.9) +
  geom_jitter(alpha = 0.5,color = "black") +
  labs(title = " Humidity of Box plot against Month", x = "Month", y = "Humidity") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.6),
        ) +
  facet_wrap(~origin)
 


#Analysis 13
# This analysis shows, Box plot made for Dewpoint against month
ggplot(data = data, mapping = aes(x = factor(month) , y = dewp, na.rm = TRUE, 
                                  color = origin)) +
  geom_boxplot(alpha = 0.9) +
  labs(title = "Box plot Dewpoint against Month (F)", x = "Month", y = "Dewpoint (F) ") +
  facet_wrap(~origin) +
# guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(palette = "Greens", name = "Origin") +
  theme(plot.title = element_text(hjust = 0.7, size = 23),
        
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "light green"),
       
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 14)) 


#Analysis 14
# This analysis shows, wind drection
#wind direction
#to visualize the wind direction of 360 degreee 
ggplot(data = data) +
  geom_bar(mapping = aes(x = wind_dir, na.rm = TRUE)) +
  coord_polar() +
  labs(title = '360 degree Polar Bar Plot of Wind Direction',x = 'Wind Direction ')
+ theme(plot.title = element_text(hjust=0.5))


#  feature 1
# This analysis shows heatmap is drawn for Temperature

ggplot(data = data, aes(x = wind_speed, y = temp)) +
  geom_bin2d(bins = 90) +
  scale_fill_continuous(type = "viridis") +
  labs(title = " Wind Speed against Heat map of Temperature ") +
  theme_bw()

# features 2
# This analysis shows density plot is drawn for wind speecd

ggplot(data = data, aes(x = wind_speed)) +
  geom_density( aes(fill = origin, color = origin, alpha = 0.3) ) +
  labs(title = "Density graph weight Wind Speed(Mph)", 
       x = "Wind Speed (Mph)", y = "Density") +
  theme(plot.title = element_text(hjust = 0.7, size = 21),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "purple")
  )

                      #Thank YOU..!!

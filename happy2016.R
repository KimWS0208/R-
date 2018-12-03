library(dplyr)
library(ggalt)
library(ggplot2)
library(readr)
library(OIdata)
library(RColorBrewer)
library(classInt)
library(plotly)
library(gridExtra)

#2016행복점수별로 지도 색칠
happy2016 <- read.csv("C:\\data\\happy2016.csv", header = T)

happy_5.9_2 = wrld_simpl@data$NAME %in% c("Denmark",
                                        "Switzerland",
                                        "Iceland",
                                        "Norway",
                                        "Finland",
                                        "Canada",
                                        "Netherlands",
                                        "New Zealand",
                                        "Australia",
                                        "Sweden",
                                        "Israel",
                                        "Austria",
                                        "United States",
                                        "Costa Rica",
                                        "Puerto Rico",
                                        "Germany",
                                        "Brazil",
                                        "Belgium",
                                        "Ireland",
                                        "Luxembourg",
                                        "Mexico",
                                        "Singapore",
                                        "United Kingdom",
                                        "Chile",
                                        "Panama",
                                        "Argentina",
                                        "Czech Republic",
                                        "United Arab Emirates",
                                        "Uruguay",
                                        "Malta",
                                        "Colombia",
                                        "France",
                                        "Thailand",
                                        "Saudi Arabia",
                                        "Taiwan",
                                        "Qatar",
                                        "Spain",
                                        "Algeria",
                                        "Guatemala",
                                        "Suriname",
                                        "Kuwait",
                                        "Bahrain",
                                        "Trinidad and Tobago",
                                        "Venezuela",
                                        "Slovakia",
                                        "El Salvador",
                                        "Malaysia",
                                        "Nicaragua",
                                        "Uzbekistan",
                                        "Italy",
                                        "Greenland",
                                        "Ecuador",
                                        "Belize",
                                        "Japan",
                                        "Kazakhstan",
                                        "Moldova",
                                        "Russia",
                                        "Poland",
                                        "Korea, Republic of",
                                        "Bolivia",
                                        "Lithuania",
                                        "Belarus")

aaa2<-ifelse(happy_5.9_2==T,1,happy_5.9_2)

happy_4.8_2 = wrld_simpl@data$NAME %in% c(
                                        "North Cyprus",
                                        "Slovenia",
                                        "Peru",
                                        "Turkmenistan",
                                        "Mauritius",
                                        "Libyan Arab Jamahiriya",
                                        "Latvia",
                                        "Cyprus",
                                        "Paraguay",
                                        "Romania",
                                        "Estonia",
                                        "Jamaica",
                                        "Croatia",
                                        "Hong Kong",
                                        "Somalia",
                                        "Kosovo",
                                        "Turkey",
                                        "Indonesia",
                                        "Jordan",
                                        "Azerbaijan",
                                        "Philippines",
                                        "China",
                                        "Bhutan",
                                        "Kyrgyzstan",
                                        "Serbia",
                                        "Bosnia and Herzegovina",
                                        "Montenegro",
                                        "Dominican Republic",
                                        "Morocco",
                                        "Hungary",
                                        "Pakistan",
                                        "Lebanon",
                                        "Portugal",
                                        "Macedonia",
                                        "Vietnam",
                                        "Somaliland Region",
                                        "Tunisia",
                                        "Greece",
                                        "Tajikistan",
                                        "Mongolia",
                                        "Laos",
                                        "Nigeria",
                                        "Honduras",
                                        "Iran")
aaa2<-ifelse(happy_4.8_2==T,2,aaa2)

happy_2.8_2 = wrld_simpl@data$NAME %in% c(
                                        "Zambia",
                                        "Nepal",
                                        "Palestinian Territories",
                                        "Albania",
                                        "Bangladesh",
                                        "Sierra Leone",
                                        "Iraq",
                                        "Namibia",
                                        "Cameroon",
                                        "Ethiopia",
                                        "South Africa",
                                        "Sri Lanka",
                                        "India",
                                        "Myanmar",
                                        "Egypt",
                                        "Armenia",
                                        "Kenya",
                                        "Ukraine",
                                        "Ghana",
                                        "Congo (Kinshasa)",
                                        "Georgia",
                                        "Congo (Brazzaville)",
                                        "Senegal",
                                        "Bulgaria",
                                        "Mauritania",
                                        "Zimbabwe",
                                        "Malawi",
                                        "Sudan",
                                        "Gabon",
                                        "Mali",
                                        "Haiti",
                                        "Botswana",
                                        "Comoros",
                                        "Ivory Coast",
                                        "Cambodia",
                                        "Angola",
                                        "Niger",
                                        "South Sudan",
                                        "Chad",
                                        "Burkina Faso",
                                        "Uganda",
                                        "Yemen",
                                        "Madagascar",
                                        "Tanzania",
                                        "Liberia",
                                        "Guinea",
                                        "Rwanda",
                                        "Benin",
                                        "Afghanistan",
                                        "Togo",
                                        "Syria",
                                        "Burundi"
)

aaa2<-ifelse(happy_2.8==T,3,aaa2)

colours <- c("#00C853","#FFFF00","#FFAB00")

plot(wrld_simpl, col = c(gray(.80),"#3182BD","#9ECAE1","#DEEBF7")[aaa2+1],
     main = "2016 Happness Score",cex.main=3.0)
data(state)

x11()
nclr <- 3 
min <- 0 
max <- 100 
breaks <- (max - min) / nclr

plotclr <- brewer.pal(nclr, "Blues")
plotvar <- state$coal
class <- classIntervals(plotvar,
                        nclr,
                        style = "fixed",
                        fixedBreaks = seq(min, max, breaks))

colcode <- findColours(class, plotclr)


legend("left", # position
       legend = c('2.8이상','4.8이상','5.9이상'), 
       fill = attr(colcode, "palette"),
       cex = 2.7,
       bty = "n")
x11()

#다중 선형 회귀 2016
normalize<-function(x) { 
  return((x-min(x))/ (max(x)-min(x))) 
} 

happy2016_n <- as.data.frame(lapply(happy2016[,c(4:11)], normalize)) 

x<-lm(Happiness_Score~Economy_GDP + Family +
        Health + Freedom + Trust + Generosity +
        Dystopia_Residual, data=happy2016_n)

summary(x)

###R.squared 구하는 그래프 함수 
ggplotRegression <- function (z) {
  
  ggplot(z$model, aes_string(x = names(z$model)[2], y = names(z$model)[1])) + 
    geom_point(shape=1,size=3,color="#003399") +
    stat_smooth(method = "lm", col = "red") +
     labs(title = paste("R.squared(회귀계수) = ", signif(summary(z)$r.squared, 5),",             ",
                       " 기울기 =", signif(z$coef[[2]], 5)) )
}

##2016 회귀 계수 그래프
gg1<-ggplotRegression(lm(Happiness_Score ~ Dystopia_Residual, data = happy2016))
gg2<-ggplotRegression(lm(Happiness_Score ~ Generosity, data = happy2016))
gg3<-ggplotRegression(lm(Happiness_Score ~ Freedom, data = happy2016))
gg4<-ggplotRegression(lm(Happiness_Score ~ Economy_GDP, data = happy2016))
gg5<-ggplotRegression(lm(Happiness_Score ~ Family, data = happy2016))
gg6<-ggplotRegression(lm(Happiness_Score ~ Health, data = happy2016))
gg7<-ggplotRegression(lm(Happiness_Score ~ Trust, data = happy2016))

grid.arrange(gg1,gg2, nrow=2)
grid.arrange(gg3,gg4, nrow=2)
grid.arrange(gg5,gg6, nrow=2)
gg7


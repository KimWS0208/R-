library(dplyr)
library(ggalt)
library(ggplot2)
library(readr)
library(OIdata)
library(RColorBrewer)
library(classInt)
library(plotly)
library(gridExtra)
#2015행복점수별로 지도 색칠
happy2015 <- read.csv("C:\\data\\happy2015_2.csv",header = T)

happy_5.9 = wrld_simpl@data$NAME %in% c("Switzerland",
                                        "Iceland",
                                        "Denmark",
                                        "Norway",
                                        "Canada",
                                        "Finland",
                                        "Netherlands",
                                        "Sweden",
                                        "New Zealand",
                                        "Australia",
                                        "Israel",
                                        "Costa Rica",
                                        "Austria",
                                        "Mexico",
                                        "United States",
                                        "Brazil",
                                        "Luxembourg",
                                        "Ireland",
                                        "Belgium",
                                        "United Arab Emirates",
                                        "United Kingdom",
                                        "Oman",
                                        "Venezuela",
                                        "Singapore",
                                        "Panama",
                                        "Germany",
                                        "Chile",
                                        "Qatar",
                                        "France",
                                        "Argentina",
                                        "Czech Republic",
                                        "Uruguay",
                                        "Colombia",
                                        "Thailand",
                                        "Saudi Arabia",
                                        "Spain",
                                        "Malta",
                                        "Taiwan",
                                        "Kuwait",
                                        "Suriname",
                                        "Trinidad and Tobago",
                                        "El Salvador",
                                        "Guatemala",
                                        "Uzbekistan",
                                        "Slovakia",
                                        "Japan",
                                        "Korea, Republic of",
                                        "Ecuador",
                                        "Bahrain",
                                        "Greenland"
)

aaa<-ifelse(happy_5.9==T,1,happy_5.9)

happy_4.8 = wrld_simpl@data$NAME %in% c("Bolivia",
                                        "Moldova",
                                        "Paraguay",
                                        "Kazakhstan",
                                        "Slovenia",
                                        "Lithuania",
                                        "Nicaragua",
                                        "Peru",
                                        "Belarus",
                                        "Poland",
                                        "Malaysia",
                                        "Croatia",
                                        "Libyan Arab Jamahiriya",
                                        "Russia",
                                        "Jamaica",
                                        "North Cyprus",
                                        "Cyprus",
                                        "Algeria",
                                        "Kosovo",
                                        "Turkmenistan",
                                        "Mauritius",
                                        "Hong Kong",
                                        "Estonia",
                                        "Indonesia",
                                        "Vietnam",
                                        "Turkey",
                                        "Kyrgyzstan",
                                        "Nigeria",
                                        "Bhutan",
                                        "Azerbaijan",
                                        "Pakistan",
                                        "Jordan",
                                        "Montenegro",
                                        "China",
                                        "Zambia",
                                        "Romania",
                                        "Serbia",
                                        "Portugal",
                                        "Latvia",
                                        "Philippines",
                                        "Somalia",
                                        "Morocco",
                                        "Macedonia",
                                        "Mozambique",
                                        "Albania",
                                        "Bosnia and Herzegovina",
                                        "Lesotho",
                                        "Dominican Republic",
                                        "Laos",
                                        "Mongolia",
                                        "Swaziland",
                                        "Greece",
                                        "Lebanon",
                                        "Hungary"
)
aaa<-ifelse(happy_4.8==T,2,aaa)

happy_2.8 = wrld_simpl@data$NAME %in% c("Honduras",
                                        "Tajikistan",
                                        "Tunisia",
                                        "Palestinian Territories",
                                        "Bangladesh",
                                        "Iran",
                                        "Ukraine",
                                        "Iraq",
                                        "South Africa",
                                        "Ghana",
                                        "Zimbabwe",
                                        "Liberia",
                                        "India",
                                        "Sudan",
                                        "Haiti",
                                        "Congo",
                                        "Nepal",
                                        "Ethiopia",
                                        "Sierra Leone",
                                        "Mauritania",
                                        "Kenya",
                                        "Djibouti",
                                        "Armenia",
                                        "Botswana",
                                        "Myanmar",
                                        "Georgia",
                                        "Malawi",
                                        "Sri Lanka",
                                        "Cameroon",
                                        "Bulgaria",
                                        "Egypt",
                                        "Yemen",
                                        "Angola",
                                        "Mali",
                                        "Comoros",
                                        "Uganda",
                                        "Senegal",
                                        "Gabon",
                                        "Niger",
                                        "Cambodia",
                                        "Tanzania",
                                        "Madagascar",
                                        "Central African Republic",
                                        "Chad",
                                        "Guinea",
                                        "Ivory Coast",
                                        "Burkina Faso",
                                        "Afghanistan",
                                        "Rwanda",
                                        "Benin",
                                        "Syria",
                                        "Burundi",
                                        "Togo",
                                        "Democratic Republic of the Congo"
)

aaa<-ifelse(happy_2.8==T,3,aaa)

colors <- c("#00C853","#FFFF00","#FFAB00")

data(state)

nclr <- 3 
min <- 0 
max <- 100 
breaks <- (max - min) / nclr

library(OIdata)
library(RColorBrewer)
library(classInt)

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

plot(wrld_simpl, col = c(gray(.80),"#3182BD","#9ECAE1","#DEEBF7")[aaa+1],
     main = "2015 Happness Score",cex.main=3.0)

##지도와 패키지안에 나라이름이 다른 부분 찾는방법 
unique(wrld_simpl@data$NAME)
happy2015[(happy2015$Country %in% zzz)==FALSE, c("Country")]
graphics.off()

###다중 선형 회귀 2015
normalize<-function(x) { 
  return((x-min(x))/ (max(x)-min(x))) 
} 

happy2015_n <- as.data.frame(lapply(happy2015[,c(4:11)], normalize)) 

x<-lm(Happiness_Score~Economy_GDP + Family +
     Health + Freedom + Trust + Generosity +
     Dystopia_Residual, data=happy2015_n)

summary(x)

#2015행복 점수보다 2016행복 점수가 오른나라 시각화 
happy2015 <- read.csv("C:\\data\\happy2015_2.csv",header = T)
happy2016 <- read.csv("C:\\data\\happy2016.csv", header = T)

install.packages("ggalt")
library(ggalt)
library(dplyr)
library(ggplot2)
library(readr)

xx15<-happy2015 %>% select(Country,Region,yy15=Happiness_Score)
xx16<-happy2016 %>% select(Country,Region,yy16=Happiness_Score)

score<-inner_join(xx15,xx16)%>% mutate(score_diff= yy16-yy15)%>% filter(score_diff>0)

score$Country <- factor(score$Country, levels=as.character(score$Country))

gg <- ggplot(score, aes(x=yy15, xend=yy16, y=Country, group=Country)) + 
              geom_dumbbell(size=2, color="#e3e2e1", 
                colour_x = "#B71C1C", colour_xend = "#F48FB1",
                dot_guide=TRUE, dot_guide_size=0.25) + 
                labs(x=NULL, y=NULL, title="2015 vs 2016") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7") )
x11()
plot(gg)

###R.squared 구하는 그래프 함수 
ggplotRegression <- function (z) {
  
  ggplot(z$model, aes_string(x = names(z$model)[2], y = names(z$model)[1])) + 
    geom_point(shape=1,size=3,color="#003399") +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("R.squared(회귀계수) = ", signif(summary(z)$r.squared, 5),",             ",
                       " 기울기 =", signif(z$coef[[2]], 5)) )
}

#2015 회귀 계수 그래프
g1<-ggplotRegression(lm(Happiness_Score ~ Dystopia_Residual, data = happy2015))
g2<-ggplotRegression(lm(Happiness_Score ~ Economy_GDP, data = happy2015))
g3<-ggplotRegression(lm(Happiness_Score ~ Family, data = happy2015))
g4<-ggplotRegression(lm(Happiness_Score ~ Health, data = happy2015))
g5<-ggplotRegression(lm(Happiness_Score ~ Generosity, data = happy2015))
g6<-ggplotRegression(lm(Happiness_Score ~ Freedom, data = happy2015))
g7<-ggplotRegression(lm(Happiness_Score ~ Trust, data = happy2015))

grid.arrange(g1,g2, nrow=2)
grid.arrange(g3,g4, nrow=2)
grid.arrange(g5,g6, nrow=2)
g7


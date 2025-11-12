getwd()
#load ggplot
install.packages("ggplot2")
library(ggplot2)


#creating a data subset
nutritionData <- read.csv("nutritionData.csv", stringsAsFactors = FALSE)
smoking <- nutritionData[,c(1,5,15)]
unique(smoking$currently_smoke)


#methods: plot continuous variable
histogram <- ggplot(smoking, aes(KCAL_EXPENDITURE_RECR)) + geom_histogram( binwidth=30, fill="#E6E6FA", color="#000000", alpha=0.9) +
   theme_classic() + theme(plot.title = element_text(size=15, face="bold", hjust = 0.5)) + labs(title="DAILY RA CALORIE EXPENDITURE", 
        y = "Frequency", x= "KCal Expenditure")
histogram
ggsave("histogram.jpg",histogram,dpi = 600)


#Mean,median,mode,IQR,variance
mode <- function(x){as.numeric(names(sort(-table(x)))[1])}
summary(smoking$KCAL_EXPENDITURE_RECR)
IQR(smoking$KCAL_EXPENDITURE_RECR)
mode(smoking$KCAL_EXPENDITURE_RECR)
var(smoking$KCAL_EXPENDITURE_RECR)
sd(smoking$KCAL_EXPENDITURE_RECR)


#graph
graphSmoking <- ggplot(smoking,aes(x=currently_smoke,y = KCAL_EXPENDITURE_RECR, fill=currently_smoke))+geom_boxplot() +theme_classic() +
 stat_boxplot(geom = "errorbar",width = 0.2) + theme(plot.title = element_text(size=13, face="bold", hjust = 0.5), 
  axis.title.x = element_blank(),axis.text = element_text(size = 9,hjust = 0.5, colour = "black"), axis.title.y = element_text(size = 11,
   colour = "black")) + labs(title="Effect of Smoking on Daily Caloric Expenditure \n From Recreational Activity", 
    y= "Number of kcals Expended", fill="Current\nSmoker") + scale_fill_manual(values=c("#E97451","#EDEADE"))
graphSmoking
ggsave("graphSmoking.jpg",graphSmoking,dpi = 600)


#Statistical Test
#Independent t-test
smokers <- smoking[smoking$currently_smoke=="Yes",]
nonsmokers <- smoking[smoking$currently_smoke=="No",]
t.test(nonsmokers$KCAL_EXPENDITURE_RECR,smokers$KCAL_EXPENDITURE_RECR, var.equal= TRUE)
sd(smokers$KCAL_EXPENDITURE_RECR)
sd(nonsmokers$KCAL_EXPENDITURE_RECR)
#mean difference
446.0267-191.3340
#significant difference
313.9972-137.773







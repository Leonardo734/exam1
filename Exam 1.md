Question 1}
#xtabs(~age_recode + s_e_support_lack)
 #         s_e_support_lack
#age_recode    0    1
#   [15,25] 1232 1039
#   (25,35] 5429 3708
#   (35,45] 7316 5282
#   (45,55] 6615 4439
#   (55,65] 8894 4293
#   (65,75] 9308 2913
#   (75,85] 2956  677


#Confident level: 95% 
#1 - 0.95 = 0.05. 
#z = 1.645

#The null hypothesis is the that from ages to 15 to 25 who telework have the same emotional support as those from ages from 75 to 85. 
#p1 = p2

#The altervative Hypothesis: Those from 15 to 25 who telework have more emotional support than those from ages from 75 to 85. 
#p1 > p2. 
#Right tailed test. 

n1 <- 1232 + 1039
#n1 = 2271
n2 <- 2956 + 677
#n2 = 3633
p1 <- 1039 /(n1)
#p1 = 0.458
p2 <- 677/ (n2)
#p2 = 0.186
P <- (1039 + 677)/ (n1 + n2)
#P = 0.291

t <- (p1 - p2)/(P*(1-P)*(1/n1 + 1/n2))^0.5
#t = 22.324
##SR <- ()

MOE <- 1.645*(p1*(1-p1)/n1 + p2*(1-p2)/n2)^0.5
CR_lower <- (p1 - p2)- MOE
CR_higher <- (p1 - p2) + MOE

#MOE = 0.020
#CR_lower = 0.251
#CR_highwer = 0.291

#For the null hypothesis I assumed that al teleworkers reviced the same level of 
#emotional support, while the Alterative hypothesis suppests that younger teleworkers
#recieve more than older workers. The t stat I calcuated was 22.324 and the rejection area is  <= 1.645. 22.324 > 1.645 so with sufficient evidence we must reject the null hypothesis.


Question 2: 
#educ_recode	  noWFH	WFH
#not 4yr degree	25064	5677
#4yr or more  	17918	15592

#Null hypothsis: Individuals without a 4 yr degree work the same chance to work from home as individuals with a 4yr or more. p1 = p2

#Altervative hypothesis: Individuals without a 4 yr degree have a smaller chance to work from home as individuals with a 4yr or more. p1 < p2

#This is a left tailed test. 

#Confident: 95%. 1 - 0.95 = 0.05
#z = 1.645

n1 <- 25064	+ 5677
#n1= 30741
p1 <-  5677 / n1
#p1 = 0.185
n2 <- 17918 + 15592
#n2 = 33510
p2 <- 15592/ n2
#p2 = 0.465
P <- (5677 + 15592)/ (n1 + n2)
#P = 0.331
t <- (p1 - p2)/(P*(1-P)*(1/n1 + 1/n2))^0.5
#t= -75.508
MOE <- 1.645*(p1*(1-p1)/n1 + p2*(1-p2)/n2)^0.5
CR_lower <- (p1 - p2)- MOE
CR_higher <- (p1 - p2) + MOE

#MOE = 0.006
#CR_lower = -0.286
#CR_higher = -0.275

#The null hypothsis assumes that all these indivduals have the same chance of working from home but the altervative hypothesis assumes those with less education have a smaller chance of working from home, The area of rejection is <= -1.645 and the t stat is -75.508. Since -75.508 is less than -1.645 then we must reject the null hypothesis with sufficient evidence. 


Question 3: 

load("Household_Pulse_data_ph4c2.RData")

#The subgroup I would like to see if there is a gender divide between people that do not have enough to eat. So this divide would between men and women so it is 0.5 and 0.5 for the whole population. 

#QUestion 4: 

#Hypothsis test: 
#Null hypotheis:Men and women have the same chance of often not having enough to eat.  

#ALtervative hypothesis: Men have less of a chance ofoften not having enough to eat than women 

pop1 <- Household_Pulse_data %>% filter(EGENID_BIRTH == "male")
#pop1= 313510
pop2 <- Household_Pulse_data %>% filter(EGENID_BIRTH == "female") 
#pop2 = 39642
P1 <- Household_Pulse_data %>% filter(EGENID_BIRTH == "male", CURFOODSUF == "often not enough food" )
#P1 = 741
P2 <- P1 <- Household_Pulse_data %>% filter(EGENID_BIRTH == "female", CURFOODSUF == "often not enough food" )
#P2 = 741

P_1 <- (741 - 741)/ (31510 - 39642)
#P_1 = 0 






#Question 5:
library(modelsummary)
library(plotly)
library(ggthemes)
library(flexdashboard)
library(dplyr)
library(tidyverse)
library(plotly)

Y <- ggplot(data = Household_Pulse_data %>% group_by(EGENID_BIRTH,CURFOODSUF),count(), mapping = aes(x = EGENID_BIRTH, y = ,CURFOODSUF, fill = EGENID_BIRTH)) + geom_point()+ geom_smooth(method = "lm") 

Y

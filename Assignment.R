#Assignment de Haan

onlinedata <- read.csv("C:/Users/Grohmann/Documents/Uni_Kurse/De Haan/data.csv")

#Question 1
summary(onlinedata)

#2. Calculate some descriptive statistics. How large are the different groups
#and how high is the overall conversion?
summary(onlinedata)
onlinedata$n <- 1
xtabs(onlinedata$n ~ onlinedata$Flyer_region + onlinedata$Firm_ad)

#3. How much does the conversion differ between the four different groups 
#(i.e. with and without flyer and with and without display ads).

xtabs(onlinedata$Sale ~ onlinedata$Flyer_region + onlinedata$Firm_ad)
xtabs(onlinedata$Sale ~ onlinedata$Flyer_region + onlinedata$Firm_ad)/xtabs(onlinedata$n ~ onlinedata$Flyer_region + onlinedata$Firm_ad)

#4. To formally test hypotheses in cases where the dependent variable
#is binary (1/0), a logistic regression model can be used. In the case 
#you want to test the effectiveness and synergy of flyers and display
#ads, the following regression can be used:

model1 <- glm(Sale ~ Flyer_region + Firm_ad + Flyer_region*Firm_ad, data=onlinedata, family=binomial)
summary(model1)

#5. In the two experimental conditions for online display ads, there are also 
#people who have zero ad exposures. How do you think the conversion differs 
#between people who have not seen a display ad in the 'firm display ad group'
#and the 'charity display ad group'? Please explain why you assume this and 
#after that formally test this by extending the cross tab from step 3 and 
#the logistic regression model from step 4. Interpret the results, is there 
#anything surprising?

#Hypothesis:
#I would expect that the poeple who have seen the frim add, have a higher porbability of conversion 
#than those, that have seen the charity add. This means, that the firm add has an effect on the buying
#behavior of the consumers. The selection bias is addressed by that too.

model3 <- glm(Sale ~  Flyer_region + Ad_seen + Firm_ad + factor(Amount_ads) * Firm_ad, data=onlinedata, family=binomial)
summary(model3)
model4 <- glm(Sale ~  Flyer_region + Firm_ad + factor(frim_amount), data=onlinedata, family=binomial)
summary(model4)

#7. Does the flyer and display ad effectiveness differ between new and existing customers?
#Is the finding in line with what you would expect and what are the consequences of this? 
#(hint: extent the logistic regression model)

model5 <- glm(Sale ~ Flyer_region + Firm_ad + Flyer_region*Firm_ad +factor(frim_amount)+ Existing_customer, data=onlinedata, family=binomial)
summary(model5)


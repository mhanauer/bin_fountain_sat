---
title: "telehealth_bin_fountain"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Load in data
```{r}
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks/satisfaction")
bin_fount_sat_dat = read.csv("binary_client_sat_dat_5_4_20.csv", header = TRUE, na.strings = c("null", "Not Applicable", "YES, DEFINITELY", "I am not receiving services addressing substance use", "", " "))
head(bin_fount_sat_dat)
```
Need to rename all variables
```{r}
bin_fount_sat_dat = bin_fount_sat_dat[,1:22]
colnames(bin_fount_sat_dat) = c("survey", "return_client", "instructions", "quick", "communicate", "substance", "manage", "recovery", "recommend", "expectation", "prefer_service", "benefits", "barriers", "contact_info", "first_name", "last_name", "email", "phone_number", "state", "age", "race", "gender")
head(bin_fount_sat_dat)

```
Plot, n and percentage for return client
```{r}
library(ggplot2)

head(bin_fount_sat_dat)

return_client_dat = na.omit(bin_fount_sat_dat$return_client)
return_client_dat = data.frame(return_client = return_client_dat)

return_client_dat = data.frame(freq(return_client_dat$return_client))
## Get rid of total
return_client_dat = return_client_dat[-3,]
var_names =  rownames(return_client_dat)
return_client_dat$var_names = var_names
return_client_dat$Frequency = as.factor(return_client_dat$Frequency)
return_client_dat$Percent = return_client_dat$Percent / 100

return_client_dat$Frequency = paste0("n=",return_client_dat$Frequency)
plot_return_client = ggplot(return_client_dat, aes(x = var_names,y = Percent, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title="Are you a new or returning client?", x ="Response option", y = "Percent")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
    scale_y_continuous(labels = scales::percent, limits = c(0,1))+scale_fill_manual(values = c("red", "blue", "green", "purple"))+
  theme(legend.position = "none")+
  geom_text_repel(label = return_client_dat$Frequency, vjust = -.5)
plot_return_client

```
Plot, n and percentage for instructions

######################
Not done need to add Most clear when that option is stated
```{r}
library(ggplot2)
head(bin_fount_sat_dat)

instructions_dat = na.omit(bin_fount_sat_dat$instructions)
instructions_dat = data.frame(instructions = instructions_dat)

instructions_dat = data.frame(freq(instructions_dat$instructions))
## Get rid of total change to 5 later
instructions_dat = instructions_dat[-4,]
var_names =  rownames(instructions_dat)
instructions_dat$var_names = var_names
instructions_dat$Frequency = as.factor(instructions_dat$Frequency)
instructions_dat$Percent = instructions_dat$Percent / 100

instructions_dat$var_names = factor(instructions_dat$var_names,levels = c("Very Clear", "Mostly clear", "Somewhat Clear", "Not at all Clear"))

instructions_dat$Frequency = paste0("n=",instructions_dat$Frequency)
plot_instructions = ggplot(instructions_dat, aes(x = var_names,y = Percent, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title="Were the instructions for how to access your appointment online clear?", x ="Response option", y = "Percent")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+scale_fill_manual(values = c("red", "blue", "green", "purple"))+
  theme(legend.position = "none")+
  geom_text_repel(label = instructions_dat$Frequency, vjust = -.5)
plot_instructions


```
Plot, n and percentage for quick

######################
Not done need to add Most clear when that option is stated
```{r}
library(ggplot2)
head(bin_fount_sat_dat)

quick_dat = na.omit(bin_fount_sat_dat$quick)
quick_dat = data.frame(quick = quick_dat)

quick_dat = data.frame(freq(quick_dat$quick))
## Get rid of total change to 5 later
quick_dat = quick_dat[-4,]
var_names =  rownames(quick_dat)
quick_dat$var_names = var_names
quick_dat$Frequency = as.factor(quick_dat$Frequency)
quick_dat$Percent = quick_dat$Percent / 100

quick_dat$var_names = factor(quick_dat$var_names,levels = c("Very Satisfied", "Mostly Satisfied", "Somewhat Satisfied", "Not Satisfied"))


quick_dat$Frequency = paste0("n=",quick_dat$Frequency)
plot_quick = ggplot(quick_dat, aes(x = var_names,y = Percent, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title="Were you satisfied with how quickly you got an appointment?", x ="Response option", y = "Percent")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  scale_fill_manual(values = c("red", "blue", "green", "purple"))+
  theme(legend.position = "none")+
  geom_text_repel(label = quick_dat$Frequency, vjust = -.5)
plot_quick
```
Next item: communicate
```{r}
library(ggplot2)
head(bin_fount_sat_dat)

communicate_dat = na.omit(bin_fount_sat_dat$communicate)
communicate_dat = data.frame(communicate = communicate_dat)

communicate_dat = data.frame(freq(communicate_dat$communicate))
## Get rid of total change to 6 later
communicate_dat = communicate_dat[-5,]
var_names =  rownames(communicate_dat)
communicate_dat$var_names = var_names
communicate_dat$Frequency = as.factor(communicate_dat$Frequency)
communicate_dat$Percent = communicate_dat$Percent / 100

communicate_dat$var_names = factor(communicate_dat$var_names,levels = c("Strongly agree", "Agree", "Undecided", "Disagree", "Strongly disagree"))

communicate_dat$Frequency = paste0("n=",communicate_dat$Frequency)
plot_communicate = ggplot(communicate_dat, aes(x = var_names,y = Percent, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title="The use of technology accessed through Centerstone has helped me \n reduce my substance use.", x ="Response option", y = "Percent")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+scale_fill_manual(values = c("red", "blue", "green", "purple"))+
  theme(legend.position = "none")+
  geom_text_repel(label = communicate_dat$Frequency, vjust = -.5)
plot_communicate

```
Testing communicate by variable 
```{r}
#### Test graph by a demographic.  If you want more demographics, just create interaction term (e.g., black females) into another variable and then plot by that.
communicate_test =  na.omit(bin_fount_sat_dat[c("communicate", "gender")])
communicate_test
communicate_test$communicate = factor(communicate_test$communicate,levels = c("Strongly agree", "Agree", "Undecided", "Disagree", "Strongly disagree"))
communicate_test$gender = factor(communicate_test$gender,levels = c("Female", "Male"))

count_gender = count(communicate_test, communicate, gender)
count_gender$percent = round(count_gender$n / dim(communicate_test)[1],2)
count_gender
count_gender$comm_gender = paste0(count_gender$communicate, ",", count_gender$gender)

count_gender$n_freq = paste0("n=", " ",count_gender$n)
plot_communicate_gender = ggplot(count_gender, aes(x = comm_gender,y = percent, fill = comm_gender))+
  geom_bar(stat = "identity")+
  labs(title="The use of technology accessed through Centerstone has helped me \n reduce my substance use.", x ="Response option", y = "Percent")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+scale_fill_manual(values = c("red", "blue", "red", "blue", "red"))+
  theme(legend.position = "none")+
  geom_text_repel(label = count_gender$n_freq, vjust = -.5)
plot_communicate_gender


```


Next item: substance
```{r}
library(ggplot2)
head(bin_fount_sat_dat)

substance_dat = na.omit(bin_fount_sat_dat$substance)
substance_dat = data.frame(substance = substance_dat)

substance_dat = data.frame(freq(substance_dat$substance))
## Get rid of total change to 6 later
substance_dat = substance_dat[-4,]
var_names =  rownames(substance_dat)
substance_dat$var_names = var_names
substance_dat$Frequency = as.factor(substance_dat$Frequency)
substance_dat$Percent = substance_dat$Percent / 100

substance_dat$var_names = factor(substance_dat$var_names,levels = c("Strongly agree", "Agree", "Undecided", "Disagree", "Strongly disagree"))

substance_dat$Frequency = paste0("n=",substance_dat$Frequency)
plot_substance = ggplot(substance_dat, aes(x = var_names,y = Percent, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title="The use of technology accessed through Centerstone has helped \n me reduce my substance use.", x ="Response option", y = "Percent")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  scale_fill_manual(values = c("red", "blue", "green", "purple"))+
  theme(legend.position = "none")+
  geom_text_repel(label = substance_dat$Frequency, vjust = -.5)
plot_substance



```
Next item: manage
```{r}
library(ggplot2)
head(bin_fount_sat_dat)

manage_dat = na.omit(bin_fount_sat_dat$manage)
manage_dat = data.frame(manage = manage_dat)

manage_dat = data.frame(freq(manage_dat$manage))
## Get rid of total change to 6 later
manage_dat = manage_dat[-4,]
var_names =  rownames(manage_dat)
manage_dat$var_names = var_names
manage_dat$Frequency = as.factor(manage_dat$Frequency)
manage_dat$Percent = manage_dat$Percent / 100

manage_dat$var_names = factor(manage_dat$var_names,levels = c("Strongly agree", "Agree", "Undecided", "Disagree", "Strongly disagree"))

manage_dat$Frequency = paste0("n=",manage_dat$Frequency)
plot_manage = ggplot(manage_dat, aes(x = var_names,y = Percent, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title="The use of technology accessed through Centerstone has helped \n me manage my mental health symptoms.", x ="Response option", y = "Percent")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  scale_fill_manual(values = c("red", "blue", "green", "purple"))+
  theme(legend.position = "none")+
  geom_text_repel(label = manage_dat$Frequency, vjust = -.5)
plot_manage


```
Next item: recovery
```{r}
library(ggplot2)
head(bin_fount_sat_dat)

recovery_dat = na.omit(bin_fount_sat_dat$recovery)
recovery_dat = data.frame(recovery = recovery_dat)

recovery_dat = data.frame(freq(recovery_dat$recovery))
## Get rid of total change to 6 later
recovery_dat = recovery_dat[-5,]
var_names =  rownames(recovery_dat)
recovery_dat$var_names = var_names
recovery_dat$Frequency = as.factor(recovery_dat$Frequency)
recovery_dat$Percent = recovery_dat$Percent / 100

recovery_dat$var_names = factor(recovery_dat$var_names,levels = c("Strongly agree", "Agree", "Undecided", "Disagree", "Strongly disagree"))

recovery_dat$Frequency = paste0("n=",recovery_dat$Frequency)
plot_recovery = ggplot(recovery_dat, aes(x = var_names,y = Percent, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title="The use of technology accessed through Centerstone has helped \n me support my mental health recovery.", x ="Response option", y = "Percent")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  scale_fill_manual(values = c("red", "blue", "green", "purple"))+
  theme(legend.position = "none")+
  geom_text_repel(label = recovery_dat$Frequency, vjust = -.5)
plot_recovery



```
Next item: recommend
```{r}
library(ggplot2)
head(bin_fount_sat_dat)

recommend_dat = na.omit(bin_fount_sat_dat$recommend)
recommend_dat = data.frame(recommend = recommend_dat)

recommend_dat = data.frame(freq(recommend_dat$recommend))
## Get rid of total change to 6 later
recommend_dat = recommend_dat[-3,]
var_names =  rownames(recommend_dat)
recommend_dat$var_names = var_names
recommend_dat$Frequency = as.factor(recommend_dat$Frequency)
recommend_dat$Percent = recommend_dat$Percent / 100

recommend_dat$var_names = factor(recommend_dat$var_names,levels = c("Definitely", "Very Likely", "Somewhat Likely", "Unlikely", "Would not recommend"))


recommend_dat$Frequency = paste0("n=",recommend_dat$Frequency)
plot_recommend = ggplot(recommend_dat, aes(x = var_names,y = Percent, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title="Would you recommend Centerstone to your family and friends?", x ="Response option", y = "Percent")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  scale_fill_manual(values = c("red", "blue", "green", "purple"))+
  theme(legend.position = "none")+
  geom_text_repel(label = recommend_dat$Frequency, vjust = -.5)
plot_recommend




```
Next item: expectation
```{r}
library(ggplot2)
head(bin_fount_sat_dat)

expectation_dat = na.omit(bin_fount_sat_dat$expectation)
expectation_dat = data.frame(expectation = expectation_dat)

expectation_dat = data.frame(freq(expectation_dat$expectation))
## Get rid of total change to 11 later
expectation_dat = expectation_dat[-7,]
var_names =  rownames(expectation_dat)
expectation_dat$var_names = var_names
expectation_dat$Frequency = as.factor(expectation_dat$Frequency)
expectation_dat$Percent = expectation_dat$Percent / 100

expectation_dat$var_names = factor(expectation_dat$var_names,levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

expectation_dat$Frequency = paste0("n=",expectation_dat$Frequency)
plot_expectation = ggplot(expectation_dat, aes(x = var_names,y = Percent, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title="How would you rate your overall experience with telehealth at \n Centerstone?", x ="Response option", y = "Percent")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  geom_text_repel(label = expectation_dat$Frequency, vjust = -.5)+
  theme(legend.position = "none")
plot_expectation



```
Next item: prefer_service
```{r}
library(ggplot2)
head(bin_fount_sat_dat)

prefer_service_dat = na.omit(bin_fount_sat_dat$prefer_service)
prefer_service_dat = data.frame(prefer_service = prefer_service_dat)

prefer_service_dat = data.frame(freq(prefer_service_dat$prefer_service))
## Get rid of total change to 7 later
prefer_service_dat = prefer_service_dat[-6,]
var_names =  rownames(prefer_service_dat)
prefer_service_dat$var_names = var_names
prefer_service_dat$Frequency = as.factor(prefer_service_dat$Frequency)
prefer_service_dat$Percent = prefer_service_dat$Percent / 100

prefer_service_dat$Frequency = paste0("n=",prefer_service_dat$Frequency)
plot_prefer_service = ggplot(prefer_service_dat, aes(x = var_names,y = Percent, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title="In the future, how would you prefer to receive services from \n Centerstone?", x ="Response option", y = "Percent")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  theme(legend.position = "none")+
  geom_text_repel(label = prefer_service_dat$Frequency, vjust = -.5)
plot_prefer_service
## Open in new window


```
Next item: state
```{r}
library(ggplot2)
head(bin_fount_sat_dat)

state_dat = na.omit(bin_fount_sat_dat$state)
state_dat = data.frame(state = state_dat)

state_dat = data.frame(freq(state_dat$state))
## Get rid of total change rows in state_dat
state_dat = state_dat[-6,]
var_names =  rownames(state_dat)
state_dat$var_names = var_names
state_dat$Frequency = as.factor(state_dat$Frequency)
state_dat$Percent = state_dat$Percent / 100

state_dat$Frequency = paste0("n=",state_dat$Frequency)
plot_state = ggplot(state_dat, aes(x = var_names,y = Percent, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title="Which state do you currently live in?", x ="Response option", y = "Percent")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  theme(legend.position = "none")+
  geom_text_repel(label = state_dat$Frequency, vjust = -.5)
plot_state



```
Age create buckets from NOMS


Race
```{r}
library(ggplot2)
head(bin_fount_sat_dat)

race_dat = na.omit(bin_fount_sat_dat$race)
race_dat = data.frame(race = race_dat)

race_dat = data.frame(freq(race_dat$race))
## Get rid of total change rows in race_dat
race_dat = race_dat[-6,]
var_names =  rownames(race_dat)
race_dat$var_names = var_names
race_dat$Frequency = as.factor(race_dat$Frequency)
race_dat$Percent = race_dat$Percent / 100

race_dat$Frequency = paste0("n=",race_dat$Frequency)
plot_race = ggplot(race_dat, aes(x = var_names,y = Percent, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title="Which race do you currently live in?", x ="Response option", y = "Percent")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  theme(legend.position = "none")+
  geom_text_repel(label = race_dat$Frequency, vjust = -.5)
plot_race


```
Gender
```{r}
library(ggplot2)
head(bin_fount_sat_dat)

gender_dat = na.omit(bin_fount_sat_dat$gender)
gender_dat = data.frame(gender = gender_dat)

gender_dat = data.frame(freq(gender_dat$gender))
## Get rid of total change rows in gender_dat
gender_dat = gender_dat[-3,]
var_names =  rownames(gender_dat)
gender_dat$var_names = var_names
gender_dat$Frequency = as.factor(gender_dat$Frequency)
gender_dat$Percent = gender_dat$Percent / 100

gender_dat$Frequency = paste0("n=",gender_dat$Frequency)

plot_gender = ggplot(gender_dat, aes(x = var_names,y = Percent, fill = var_names))+
  geom_bar(stat = "identity")+
  labs(title="Which gender do you currently live in?", x ="Response option", y = "Percent")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  theme(legend.position = "none")+
  geom_text_repel(label = gender_dat$Frequency, vjust = -.5)
plot_gender
```

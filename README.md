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
bin_fount_sat_dat = read.csv("binary_client_sat_dat_5_4_20.csv", header = TRUE, na.strings = c("null", "Not Applicable", "YES, DEFINITELY"))
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

plot_return_client = ggplot(return_client_dat, aes(x = var_names,y = Percent, fill = Frequency))+
  geom_bar(stat = "identity")+
  labs(title="Are you a new or returning client?", x ="Response option", y = "Percent")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))
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

plot_instructions = ggplot(instructions_dat, aes(x = var_names,y = Percent, fill = Frequency))+
  geom_bar(stat = "identity")+
  labs(title="Were the instructions for how to access your appointment online clear?", x ="Response option", y = "Percent")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))
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

plot_quick = ggplot(quick_dat, aes(x = var_names,y = Percent, fill = Frequency))+
  geom_bar(stat = "identity")+
  labs(title="Were you satisfied with how quickly you got an appointment?", x ="Response option", y = "Percent")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))
plot_quick
```
Next item
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

plot_quick = ggplot(quick_dat, aes(x = var_names,y = Percent, fill = Frequency))+
  geom_bar(stat = "identity")+
  labs(title="Were you satisfied with how quickly you got an appointment?", x ="Response option", y = "Percent")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))
plot_quick
```




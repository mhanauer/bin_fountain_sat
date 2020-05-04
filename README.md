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
bin_fount_sat_dat = read.csv("binary_client_sat_dat_5_4_20.csv", header = TRUE, na.strings = c("null", "Not Applicable"))
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

return_client_dat = na.omit(bin_fount_sat_dat$return_client)
return_client_dat = data.frame(return_client = return_client_dat)
count_dat = describe.factor(return_client_dat$return_client,decr.order= FALSE)
count_dat = ifelse(return_client == "New", count_dat[1,1], count_dat[1,2])
return_client_dat$count_dat = paste0(return_client_dat$return_client, " ", "n=", count_dat)

plot_return_client = ggplot(return_client_dat, aes(x = count_dat, fill = count_dat))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  labs(title="Are you a new or returning client?", x ="Category", y = "Percent")+
  theme(legend.position = "none")
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
count_dat = describe.factor(instructions_dat$instructions,decr.order= FALSE)
count_dat = ifelse(instructions_dat$instructions == "Not at all Clear", count_dat[1,1], ifelse(instructions_dat$instructions == "Somewhat Clear", count_dat[1,2], ifelse(instructions_dat$instructions ==  "Very Clear", count_dat[1,3], ifelse(instructions_dat$instructions == "YES, DEFINITELY", count_dat[1,4], "Wrong"))))
instructions_dat$count_dat = paste0(instructions_dat$instructions, " ", "n=", count_dat)

plot_instructions = ggplot(instructions_dat, aes(x = count_dat, fill = count_dat))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  labs(title="Were the instructions for how to access your appointment online clear?", x ="Category", y = "Percent")+
  theme(legend.position = "none")
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
quick_dat$quick = factor(quick_dat$quick,levels = c("Very Satisfied", "Mostly Satisfied", "Somewhat Satisfied", "Not Satisfied"))

count_dat = describe.factor(quick_dat$quick,decr.order= FALSE)
count_dat = ifelse(quick_dat$quick == "Very Satisfied", count_dat[1,1], ifelse(quick_dat$quick == "Mostly Satisfied", count_dat[1,2], ifelse(quick_dat$quick ==  "Somewhat Satisfied", count_dat[1,3], ifelse(quick_dat$quick == "Not Satisfied", count_dat[1,4], "Wrong"))))
quick_dat$count_dat = paste0(quick_dat$quick, " ", "n=", count_dat)


plot_quick = ggplot(quick_dat, aes(x = quick, fill = count_dat))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  labs(title="Were the quick for how to access your appointment online clear?", x ="Category", y = "Percent")+
  theme(legend.position = "none")
plot_quick
```




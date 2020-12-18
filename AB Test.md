---
title: "A/B Test in R"
author: "Mumitul Al-Anik"
date: "December 17, 2020"
output: html_document
---
```{r}
options(warn = -1) 
```

```{r}
#Installing necessary packages
```

```{r message=FALSE}
library(dplyr)
library(readr)
library(tidyverse)
library(lubridate)
library(ggplot2)
```

```{r}
#Loading data and making it machine readable
```


```{r}
ab_data <- read_csv("C:/Users/Mumitul/Desktop/ab_data.csv")
View(ab_data)
df<- read.csv("C:/Users/Mumitul/Desktop/ab_data.csv")
df <- data.frame(df)
colnames(df)
nrow(df)
```

```{r}
#find out not aligned info between 'group' and 'landing_page'

notaligned_user=df %>% filter((df$group=='treatment'& landing_page == "old_page")|(df$group=='control'& landing_page == "new_page"))

#aligned info between 'group' and 'landing_page'
df1=df[(df$group=='control'& df$landing_page == "old_page")|(df$group=='treatment'& df$landing_page == "new_page"),]

# I am only keeping the unique ids for the anlysis
unique_id <- unique(df1$user_id)
length(unique_id)
```

```{r}
#if a user clicked several times, only keep the first result for analysis

df1 <- df1 %>% group_by(user_id) %>% arrange(timestamp)
df2=df1[!duplicated(df1$user_id),]   
nrow(df2)
```


```{r}
#Converting the time stamp to yyyy-mm-dd format 
#Calculating the conversion rate by the mean

df2$date<-as.Date(df2$timestamp)
df_new<-df2 %>% 
group_by(date,group) %>% 
summarize(conversion_rate = mean(converted))
```

```{r}
# Some desciptive data anlysis using ggplot

ggplot(df_new,aes(x=date,y=conversion_rate,
  color = group,group = group)) +
  geom_point(size = 2) +
  geom_line(lwd = 1) +
  labs(x = "date",  y = "Conversion Rate")
```

```{r}
ggplot(df2, aes(x = group, fill = factor(converted))) +geom_bar(position = "fill") + theme(axis.text.x = element_text( size=8, angle=90)) + scale_fill_discrete(name = "Group")
```

```{r}
ggplot(df2, aes(x = date, fill = factor(group))) +geom_bar(position = "fill") + theme(axis.text.x = element_text( size=8, angle=90)) + scale_fill_discrete(name = "Group")
```

```{r}
ggplot(df2, aes(x = date, fill = factor(converted))) +geom_bar(position = "fill") + theme(axis.text.x = element_text( size=8, angle=90)) + scale_fill_discrete(name = "Group")
```


```{r}
#AB Testing: Organize variables and run logistic regression
#H0:Pnew<=Pold  H1:Pnew>Pold

df2$group = factor(df2$group, levels = c("control", "treatment"))
fit<-glm(converted ~ group, family = "binomial", data =df2) 
summary(fit)
# group treatment p-value 0.19>0.05
#Finding is not statistically significant, cannot reject H0
#Visitors do not like the new interface of the webpage
```
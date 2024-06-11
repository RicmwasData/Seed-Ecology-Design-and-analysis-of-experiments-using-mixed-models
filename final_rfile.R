
library(tidyverse)
raw.files <- data_frame(filename = list.files('kaya_jayne-attachments/'))

raw.file.paths <- raw.files  %>%
  mutate(filepath = paste0("kaya_jayne-attachments/", filename))
raw.data <- raw.file.paths %>%
  # 'do' the function for each row in turn
  rowwise() %>%
  do(., read_csv(file=.$filepath))

#filter the two condtions i.e h20 and peg
raw.data<- raw.data%>%
  filter(condition %in% c("h2o", "peg"))
CountDay <- as.integer(substr(names(raw.data)[9:15], 5, 99))
ColNDay  <- names(raw.data)[9:15]

# seeds<- aggregate(
#   cbind(day_2,day_4,day_6,day_8,day_10,day_12,day_14,N)~
#     bench+tray+genus+habitat+condition,
#   raw.data, sum
# )

seeds.long <- gather(data = raw.data, key = day, value = count, any_of(ColNDay))


seeds.long$day <- as.integer(substr(seeds.long$day, 5, 99))
seeds.long<- seeds.long%>%
  #filter(condition %in% c("h2o", "peg"))%>%
  mutate_if(is.character, as.factor)

seeds.long<-seeds.long%>%
  group_by(genus,tray,condition,day)%>%
  summarise(count= sum(count, na.rm = T))

ggplot(seeds.long, aes(x = day, y = count, color = genus:condition)) + 
  geom_line()+
  labs(title = "Total Seed Germination",
       x="Time in Days", "Germinated Seed",
       color= "Species*Condition",
       caption = "Total number of germiated seeds in each tray at time x")+
  theme_bw()+
  facet_grid(~tray)+
  theme(plot.caption = element_text(hjust=0))


#calculate the 50% 
calcTG50 <- function(n_day){
  #return NA if there was no germination
  if(sum(n_day, na.rm = T) < 1)
    return(NA)
  
  #remove days for which the observations (the photos) are missing:
  #if there was no observation, NA is placed in the n_day column
  #an additional 'day 0' is added to handle cases where 50% of
  #the seeds germinated already before the first day of counting
  days  <- c(0, CountDay[!is.na(n_day)])
  n_day <- c(0, n_day[!is.na(n_day)])
  
  #calculate ng, and for convenience ng2 = ng / 2:
  ng  <- n_day[length(n_day)]
  ng2 <- ng / 2
  
  #calculate ni and nj:
  ni <- tail(n_day[(n_day <= ng2)], n = 1)
  nj <- head(n_day[(n_day > ng2)], n = 1)
  
  
  #calculate ti and tj:
  ti <- tail(days[(n_day <= ng2)], n = 1)
  tj <- head(days[(n_day > ng2)], n = 1)
  
  #calculate TG50 according to the equation:
  ti + ((ng2 - ni) * (ti - tj)) / (ni - nj)
}

tg50 <- apply(raw.data[, ColNDay], 1, calcTG50)

raw.data$TG50 <- round(tg50, 2)

seeds.stats <- raw.data[, c("tray", "genus", "condition", "TG50")]
seeds.stats<- seeds.stats%>%
  mutate_if(is.character,as.factor)



## error bar 
bar <- aggregate(TG50  ~ condition + genus,
                 data = seeds.stats, function(x) c(mean = mean(x), sd = sd(x)))

#calculate the ranges of the error bars, e.g. mean +/- sd
ymin <- bar$TG50[, "mean"] - bar$TG50[, "sd"]
ymax <- bar$TG50[, "mean"] + bar$TG50[, "sd"]
###
bar2<- data.frame(condition=bar$condition,genus=bar$genus,
                  mean=bar$TG50[, "mean"], sd=bar$TG50[, "sd"] )

#### barplot ##
ggplot(bar2, aes(x=genus, y=mean, group=condition, color=condition)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(0.05))+
  labs(title = "",x="Species",
       y="Mean",
       caption = "Figure 2: Time to 50% Germination Error Bar")+
  theme_bw()




### anova model.. 
seeds.tgf.aov <- aov(TG50 ~ genus * condition + tray, data = seeds.stats)
#seeds.tgf.aov
summary(seeds.tgf.aov)


## mixed effect ###
library(lme4)
library(lmerTest)
library(emmeans)

seeds.tgf.lm_mix <- lmer(TG50 ~ genus * condition + (1|tray), data = seeds.stats)
#seeds.tgf.lm_mix
anova(seeds.tgf.lm_mix)

#means for the group
seeds.tgf.means <- aggregate(TG50 ~ genus + condition, seeds.stats, mean)
seeds.tgf.means

#diagnistic plots 
library(performance)
library(parameters)
library(kableExtra)
check_model(seeds.tgf.aov)
check_model(seeds.tgf.lm_mix)

# TukeyHSD pairwise test
seeds.tgf.hsd <- TukeyHSD(seeds.tgf.aov)
seeds.tgf.hsd

#mixed model
#species
emmeans(seeds.tgf.lm_mix, pairwise ~ genus, adjust = "tukey")$contrasts

#condition 
emmeans(seeds.tgf.lm_mix, pairwise ~ condition, adjust = "tukey")$contrasts

#interation
emmeans(seeds.tgf.lm_mix, pairwise ~ genus:condition, adjust = "tukey")$contrasts

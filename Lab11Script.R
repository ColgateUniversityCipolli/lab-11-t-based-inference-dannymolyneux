#Lab 11
install.packages("tidyverse")
library(tidyverse)
#Task 1: Power Analysis
install.packages("pwr")
library(pwr)
#n = 20.58039, so 21 observations required
power.analysis = pwr.t.test(d=0.65, sig.level = 0.05, power = 0.80, type = "one.sample", alternative = "two.sided")

#Task 2: Collect data
dopamine.data = read_csv("DopamineData.csv")
dopamine.data = dopamine.data |>
  mutate(difference = Closer_vals - Farther_vals)
view(dopamine.data)

#Task 3: Summarize the data
library(patchwork)
summary = function(data) {
  mean = mean(data)
  min = min(data)
  max = max(data)
  sd = sd(data)
  median = median(data)
  return(c(min, max, median, mean, sd))
}
farther = summary(dopamine.data$`Farther_vals`)
closer = summary(dopamine.data$`Closer_vals`)
differences = summary(dopamine.data$difference)

table = data.frame()
table = rbind(farther,
              closer,
              differences) |> as.data.frame()
summary.table = table |>
  rename(min = "V1",
         max = "V2",
         median = "V3",
         mean = "V4",
         sd = "V5")
view(summary.table)
farther.plot = ggplot(data = dopamine.data, aes(x=`Farther_vals`))+         
  geom_histogram(aes(y=after_stat(density)),
                 breaks=seq(-0.7,0,0.07)) +
  geom_hline(yintercept=0)+                      
  theme_bw()+                                     
  geom_density(color = "blue") +
  xlab("Change in dopamine")+                 
  ylab("Density")+             
  labs(color = "", title = "Change in dopamine levels when further away") +
  theme(plot.title = element_text(size = 8))
closer.plot = ggplot(data = dopamine.data, aes(x=`Closer_vals`))+         
  geom_histogram(aes(y=after_stat(density)),
                 breaks=seq(0,0.35,0.035)) +
  geom_hline(yintercept=0)+                      
  theme_bw()+                                     
  geom_density(color = "blue") +
  xlab("Change in dopamine")+                 
  ylab("Density")+             
  labs(color = "", title = "Change in dopamine levels when closer") +
  theme(plot.title = element_text(size = 8))
differences.plot = ggplot(data = dopamine.data, aes(x=difference))+         
  geom_histogram(aes(y=after_stat(density)),
                 breaks=seq(0,1,0.08)) +
  geom_hline(yintercept=0)+                      
  theme_bw()+                                     
  geom_density(color = "blue") +
  xlab("Difference between changes in dopamine")+                 
  ylab("Density")+             
  labs(color = "", title = "Difference in dopamine levels for young zebra finches further away vs closer") +
  theme(plot.title = element_text(size = 9))
(farther.plot + closer.plot) / differences.plot

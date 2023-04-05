# Author: Espen Geelmuyden Rod
# Main analysis for "From bad to worse? How protest can foster armed conflict in autocracies"
# Published in Political Geography (2023)

rm(list = ls())

library(tidyverse)

data <- readRDS(file = "data_for_analysis.rds")

library(lfe)

m1 <- felm(ged_events_dich ~ mmad_protest_tlag1 + 
             mmad_protest_slag1 + ged_events_slag1 +
             poly(months_since_ged_events, 3, raw=T)
           | 0 | 0 | gid, data = data)
summary(m1)

m2 <- felm(ged_events_dich ~ mmad_protest_tlag1 + 
             mmad_protest_slag1 + ged_events_slag1 +
             poly(months_since_ged_events, 3, raw=T)
           | gid + year | 0 | gid, data = data)
summary(m2)

m3 <- felm(ged_events_dich ~ mmad_secviolent_tlag1 + mmad_not_secviolent_tlag1 +
             mmad_protest_slag1 + ged_events_slag1 +
             poly(months_since_ged_events, 3, raw=T)
           | 0 | 0 | gid, data = data)
summary(m3)

m4 <- felm(ged_events_dich ~ mmad_secviolent_tlag1 + mmad_not_secviolent_tlag1 +
             mmad_protest_slag1 + ged_events_slag1 +
             poly(months_since_ged_events, 3, raw=T)
           | gid + year | 0 | gid, data = data)
summary(m4)

library(stargazer)

stargazer(m1,m2,m3,m4,
          title="Estimated effect of protest on armed conflict. Excluding observations with armed conflict in the past 2 years.", align=TRUE, 
          dep.var.labels = c("Armed conflict 0/1"),
          covariate.labels=c("Protest t-1",
                             "Repressed protest t-1",
                             "Not repressed protest t-1",
                             "Protest s-1",
                             "Armed conflict s-1",
                             "Time since armed conflict ",
                             "Time since armed conflict sq.",
                             "Time since armed conflict cu."),
          star.cutoffs=c(0.05,0.01), label = "tab:dich_ac_24months", font.size="tiny",
          keep.stat=c("adj.rsq","n"),
          add.lines=list(c("Grid cell SE", "Yes", "Yes", "Yes", "Yes"), 
                         c("Year FE", "No", "Yes", "No", "Yes"), 
                         c("Grid cell FE", "No", "Yes", "No", "Yes")),
          out="gridcellmonth_table2.tex")






m1 <- felm(ged_events ~ mmad_protest_tlag1 + 
             mmad_protest_slag1 + ged_events_slag1 +
             poly(months_since_ged_events, 3, raw=T)
           | 0 | 0 | gid, data = data)
summary(m1)

m2 <- felm(ged_events ~ mmad_protest_tlag1 + 
             mmad_protest_slag1 + ged_events_slag1 +
             poly(months_since_ged_events, 3, raw=T)
           | gid + year | 0 | gid, data = data)
summary(m2)

m3 <- felm(ged_events ~ mmad_secviolent_tlag1 + mmad_not_secviolent_tlag1 +
             mmad_protest_slag1 + ged_events_slag1 +
             poly(months_since_ged_events, 3, raw=T)
           | 0 | 0 | gid, data = data)
summary(m3)

m4 <- felm(ged_events ~ mmad_secviolent_tlag1 + mmad_not_secviolent_tlag1 +
             mmad_protest_slag1 + ged_events_slag1 +
             poly(months_since_ged_events, 3, raw=T)
           | gid + year | 0 | gid, data = data)
summary(m4)

library(stargazer)

stargazer(m1,m2,m3,m4,
          title="Estimated effect of protest on armed conflict. Excluding observations with armed conflict in the past 2 years.", align=TRUE, 
          dep.var.labels = c("Armed conflict nr. events"),
          covariate.labels=c("Protest t-1",
                             "Repressed protest t-1",
                             "Not repressed protest t-1",
                             "Protest s-1",
                             "Armed conflict s-1",
                             "Time since armed conflict ",
                             "Time since armed conflict sq.",
                             "Time since armed conflict cu."),
          star.cutoffs=c(0.05,0.01), label = "tab:nr_ac_24months", font.size="tiny",
          keep.stat=c("adj.rsq","n"),
          add.lines=list(c("Grid cell SE", "Yes", "Yes", "Yes", "Yes"), 
                         c("Year FE", "No", "Yes", "No", "Yes"), 
                         c("Grid cell FE", "No", "Yes", "No", "Yes")),
          out="gridcellmonth_table4.tex")

# libries we'll need
library(ggplot2)
library(reshape2)

# read in data
formantData <- read.csv("formantMeasures.csv")

# add token column so individual formant trajectories can be identified
formantData$token <- seq_len(nrow(formantData))

# get F1 measures in long format
ehF1 <- formantData[c(1:7, 18)]
colnames(ehF1) <- c("sound", "vowel", "20", "35", "50", "65", "80", "token")
ehF1 <- melt(ehF1, id = c("sound","vowel", "token"))

# get F2 measures in long format
ehF2 <- formantData[c(1:2, 8:12, 18)]
colnames(ehF2) <- c("sound", "vowel", "20", "35", "50", "65", "80", "token")
ehF2 <- melt(ehF2, id = c("sound","vowel", "token"))

# identify the formant
ehF1$formant <- "f1"
ehF2$formant <- "f2"

# store in a single table
fs <- rbind(ehF1, ehF2)
fs_head <- subset(fs, vowel == "head")

# plot mean w. 90% confidence interval
ggplot(fs_head, aes(x = variable, colour = sound)) +
  stat_summary(aes(y = value, group = interaction(sound, formant), fill = sound),
               fun.data = mean_cl_boot, fun.args = list(conf.int = .9),
               geom = "ribbon", alpha = .5) +
  stat_summary(aes(y = value,  group = interaction(sound, formant)),
               fun.y = mean, geom = "line") +
  labs(x = "Timepoint of measure (as % of vowel)",
       y = "Formant Freq. in Hz",
       title = "Head")

# same as above with no outline to CI
ggplot(fs_head, aes(x = variable, colour = sound)) +
  stat_summary(aes(y = value, group = interaction(sound, formant),
                   fill = sound, color = NULL),
               fun.data = mean_cl_boot, fun.args = list(conf.int = .9),
               geom = "ribbon", alpha = .5) +
  stat_summary(aes(y = value,  group = interaction(sound, formant)),
               fun.y = mean, geom = "line") +
  labs(x = "Timepoint of measure (as % of vowel)",
       y = "Formant Freq. in Hz",
       title = "Head")

# spaghetti plot
ggplot(fs_head, aes(x = variable, y = value, colour = sound)) +
  geom_line(aes(group = interaction(token, formant)), alpha = .25) +
  stat_summary(aes(group = interaction(formant, sound), color = sound),
               fun.y = mean, geom = "line", size = 1.1) +
  labs(x = "Timepoint of measure (as % of vowel)",
       y = "Formant Freq. in Hz",
       title = "Head")


# Why not?
ggplot(fs, aes(x = variable, y = value, colour = sound)) +
  geom_line(aes(group = interaction(token, formant)), alpha = .25) +
  stat_summary(aes(group = interaction(formant, sound), color = sound),
               fun.y = mean, geom = "line", size = 1.1) +
  facet_wrap("vowel") +
  labs(x = "Timepoint of measure (as % of vowel)",
       y = "Formant Freq. in Hz")


setwd("~/Google_Drive/Research/simplification-data")

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# get filenames of all csvs
all_csvs <- list.files(path = ".", pattern = "\\.csv")

# load all csvs into a single dataframe
all_runs <- data.frame()
for (filename in all_csvs) {
  tmp_df <- read.csv(filename, stringsAsFactors = F)
  colnames(tmp_df) <- sapply(colnames(tmp_df), str_trim)
  
  s <- unlist(str_split(filename, '[.-]'))
  s <- s[4:length(s)-1]
  
  tmp_df$problem <- str_c(s, sep = '-', collapse = '-')
  all_runs <<- bind_rows(all_runs, tmp_df)
}

#
# Nics plot
#

unsimplified_info <- all_runs %>%
  filter(type == "unsimplified") %>%
  mutate(genPre = testError == 0) %>%
  mutate(originalSize = programSize) %>%
  select(problem, log, method, originalSize, genPre)

all_runs %>%
  filter(type == "simplified") %>%
  mutate(genPost = testError == 0,
         simplifiedSize = programSize) %>%
  inner_join(unsimplified_info, by = c("problem", "log", "method")) %>%
  unite(quad, genPre, genPost, sep = "_") %>%
  select(problem, method, originalSize, simplifiedSize, quad) %>%
  ggplot(aes(x = originalSize,
             y = simplifiedSize)) +
  geom_point(aes(color = problem),
             alpha = 0.3,
             size = 0.3, 
             shape = 4) +
  geom_abline(slope = 1, 
              intercept = 0,
              color = 'grey') +
  geom_smooth(method = "lm",
              se = FALSE,
              color = 'black',
              linetype = 2,
              size = 0.5) +
  facet_grid(quad ~ method) +
  theme_bw() +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

#
# Nics plot rotated
#

unsimplified_info <- all_runs %>%
  filter(type == "unsimplified") %>%
  mutate(genPre = testError == 0) %>%
  mutate(originalSize = programSize) %>%
  select(problem, log, method, originalSize, genPre)

all_runs %>%
  filter(type == "simplified") %>%
  mutate(genPost = testError == 0,
         simplifiedSize = programSize) %>%
  inner_join(unsimplified_info, by = c("problem", "log", "method")) %>%
  unite(quad, genPre, genPost, sep = "_") %>%
  select(problem, method, originalSize, simplifiedSize, quad) %>%
  ggplot(aes(x = originalSize,
             y = simplifiedSize)) +
  geom_point(aes(color = method),
             alpha = 0.3,
             size = 0.5, 
             shape = 4) +
  geom_abline(slope = 1, 
              intercept = 0,
              color = 'grey') +
  geom_smooth(method = "lm",
              se = FALSE,
              color = 'black',
              linetype = 2,
              size = 0.5) +
  facet_grid(quad ~ problem,
             scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))


#
# Making Tables
#

# Program Size

simp_runs <- all_runs %>%
  filter(type == "simplified") %>%
  group_by(method, problem) %>%
  summarise(avgProgramSize = round(mean(programSize), 3)) %>%
  spread(method, avgProgramSize)

prog_size_table <- all_runs %>%
  filter(type == "unsimplified") %>%
  group_by(problem) %>%
  summarise(none = round(mean(programSize), 3)) %>%
  inner_join(simp_runs, by = c("problem"))
View(prog_size_table)



  
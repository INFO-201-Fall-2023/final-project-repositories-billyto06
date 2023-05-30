library(stringr)
library(dplyr)
df <- read.csv("GamingStudy_data.csv")
df2 <- read.csv("Book1.csv")
rank_map <- c("iron", "bronze", "silver", "gold", "platinum", "diamond", "master", "grandmaster", "challenger")

filtered_df <- filter(df, Game == "League of Legends", !is.na(League))
filtered_df <- head(filtered_df, n = 50)
filtered_df$League <- case_when(
  grepl(paste(rank_map, collapse = "|"), filtered_df$League, ignore.case = TRUE) ~ 
    tolower(sub("^.*(iron|bronze|silver|gold|plat|diamond|master|grandmaster|challenger).*$", "\\1", filtered_df$League, ignore.case = TRUE)),
  TRUE ~ "unranked"
)
df2_avg <- df2 %>%
  group_by(rank) %>%
  summarize(Depression_index = mean(Depression_index), Rounds = mean(Rounds))

filtered_df_avg <- filtered_df %>%
  group_by(League) %>%
  summarize(Narcissism = mean(Narcissism), SPIN14 = mean(SPIN14), GAD_T = mean(GAD_T))

merged_df <- merge(filtered_df_avg, df2_avg, by.x = "League", by.y = "rank")

merged_df$round.index <- merged_df$Depression_index / merged_df$Rounds

merged_df$global_avg_dep_index <- 3.77
merged_df$avg_comparison <- merged_df$Depression_index > merged_df$global_avg_dep_index
write.csv(merged_df, "data_cleaned.csv", row.names = FALSE)
df_summary <- summary(merged_df)
df_summary <- as.data.frame(df_summary)
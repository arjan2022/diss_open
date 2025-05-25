# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, quanteda, readtext, textclean)

# Set file paths 
text_dir <- "d:/arkiva/hoxha/"
csv_file <- file.path(text_dir, "veprat_total.csv")

# Load and inspect file list and metadata
text_files <- list.files(text_dir, pattern = "\\.txt$", full.names = TRUE)
metadata <- read.csv(csv_file)

# Bind text file paths to metadata
metadata <- metadata %>% mutate(filepath = text_files)
glimpse(metadata)

# Read full texts into quanteda format
texts <- readtext(metadata$filepath, docvarnames = metadata$periudha)

# Clean and standardize text content
texts$text <- texts$text %>%
  textclean::replace_non_ascii() %>%
  gsub("A<<", "e", ., ignore.case = TRUE) %>%
  gsub("nAz", "e", ., ignore.case = TRUE) %>%
  gsub("F\\.S", "es", ., ignore.case = TRUE) %>%
  gsub("f:", "e", ., ignore.case = TRUE) %>%
  textclean::replace_kern()

# Clean metadata
metadata <- metadata %>%
  mutate(
    permbajtja = textclean::replace_non_ascii(permbajtja),
    periudha = textclean::replace_non_ascii(periudha)
  )

# Create corpus
corpus_hoxha <- corpus(texts)
shifra_pattern <- "(\\d+)"
docvars(corpus_hoxha, "Periudhat") <- as.numeric(str_extract(metadata$periudha, shifra_pattern))

# Summary: tokens per document
token_summary <- summary(corpus_hoxha)

# Group by time periods
token_summary <- token_summary %>%
  mutate(Periudha2 = cut(Periudhat, breaks = 10))

# Plot tokens by period
token_summary %>%
  group_by(Periudha2) %>%
  summarise(TotalTokens = sum(Tokens)) %>%
  ggplot(aes(x = Periudha2, y = TotalTokens)) +
  geom_bar(stat = "identity") +
  labs(title = "Token Count by Time Period", x = "Period", y = "Total Tokens")

# Line plot (optional)
plot(y = token_summary$Tokens, x = as.factor(token_summary$Periudha2), main = "Token Distribution by Period")

# Define keyword dictionary
keyword_list <- list(
  shkenca = c("shkenc", "teknologji", "akademi*", "eksperiment", "studiues*", "hulumtim"),
  arsimi = c("arsim", "shkoll", "universitet"),
  racionalizim = c("plani*", "strategj", "parashikim", "perllogari*"),
  inteligjenca = c("inteligjenci*", "teknokra*"),
  albanologjia = c("albanolog*", "studime shqiptare", "ilir*", "pellazg*")
)

keyword_dict <- dictionary(keyword_list, separator = "|", tolower = TRUE, encoding = "auto")

# Keyword-in-context search
kwic_results <- kwic(corpus_hoxha, pattern = phrase(keyword_dict), valuetype = "regex", case_insensitive = TRUE)

# Example: focus on "shkenca" keywords
kwic_shkenca <- kwic_results %>%
  filter(str_detect(keyword, paste(keyword_list$shkenca, collapse = "|")))

# Count keyword hits per document
shkenca_counts <- kwic_shkenca %>%
  group_by(docname) %>%
  tally(name = "KeywordHits")

print(head(shkenca_counts))
# Save results to CSV
write.csv(shkenca_counts, file = file.path(text_dir, "shkenca_keyword_hits.csv"), row.names = FALSE)
# Save the cleaned corpus to a file
saveRDS(corpus_hoxha, file = file.path(text_dir, "corpus_hoxha.rds"))
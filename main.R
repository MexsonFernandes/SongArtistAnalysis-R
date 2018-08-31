# Analyzing song lyrics of two artist from their album

# STEP 1 - Add libraries
library(geniusR)
library(tidytext)
library(tidyverse)

# STEP 2 - Downloading lyrics of artist
Taylor_Swift = genius_album(artist = "Taylor Swift", album = "Reputation")
Ed_Sheeran = genius_album(artist = "Ed Sheeran", album = "Divide")
cat(Taylor_Swift)
cat(Ed_Sheeran)

# STEP 3 - Cleaning data (remove stop words, tokenize data)
tidy_Taylor <- Taylor_Swift %>%
                unnest_tokens(word, lyric) %>%
                anti_join(stop_words) %>%
                count(word, sort=TRUE)
tidy_Ed <- Ed_Sheeran %>%
                unnest_tokens(word, lyric) %>%
                anti_join(stop_words) %>%
                count(word, sort=TRUE)
head(tidy_Taylor)
head(tidy_Ed)

# STEP 4 - Balancing both the dataset
tidy_Taylor <- tidy_Taylor %>%
                rename(swift = n) %>%
                mutate(swift_prop = swift/sum(swift))
tidy_Ed <- tidy_lyrics2 %>%
                rename(ed = n) %>%
                mutate(ed_prop = ed/sum(ed))
head(tidy_Taylor)
head(tidy_Ed)

# STEP 5 - Comparing both lyrics
compare_words <- tidy_Taylor %>%
                 full_join(tidy_Ed, by = "word")
summary(compare_words)

# STEP 6 - Plotting graph for visualization
ggplot(compare_words, aes(x=shawn_prop1, y=shawn_prop2)) +
      geom_abline() +
      geom_text(aes(label=word), check_overlap=TRUE, vjust=1.5) +
      labs(y="Stitches", x="In my blood") + theme_classic()

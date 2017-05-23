# Exercise-1
# Developed from: http://tidytextmining.com/

# Set up (install packages that you don't have)
library(janeaustenr)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
data("stop_words")

# Load booksinto a dataframe using the austen_books() function
books <- austen_books()

# How many books are in the dataset?
summarized <- books %>% group_by(book) %>% summarise(n = n())
nrow(summarized)

# Which book has the most lines?
most.book <- summarized %>% filter(n == max(n))

# Use the unnest_tokens function to generate the full list of words
unnested <- books %>% unnest_tokens(word, text)

# Which words are most common (regardless of which book them come from)?
counted <- unnested %>%
  count(word, sort = TRUE) 

# Remove stop words by performing an anti_join with the stop_words dataframe
removed <- unnested %>%
  anti_join(stop_words)

# Which non stop-words are most common?
counted <- removed %>%
  count(word, sort = TRUE) 
counted

# Use ggplot to make a horizontal bar chart of the word frequencies of non-stop words
removed %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

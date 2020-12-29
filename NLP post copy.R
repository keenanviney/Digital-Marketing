##--------------------------
#  NLP Post Copy Analysis
##--------------------------


#libraries
library(tm)
library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(wordcloud2) #creative visualizations
library(formattable)
library(knitr)
library(kableExtra)

#load data
post <- read.csv("/Users/joeyl/Documents/post copy analysis.csv", header = TRUE)
attach(post)
post$post_copy= as.character(post$post_copy)

#remove nflst
post=post[!grepl("NFL", post$Creative.ID),]


post_order = order(post$CTR)
post <- post[post_order,]
post$rank <- rank(post$CTR)

post <- post %>%
  mutate(top_ctr = 
           ifelse(post$rank %in% 822:1021, "Top 200", 
                  ifelse(post$rank %in% 622:821, "next 200", "meh")))



#tokenize
post_filtered <- post %>%
  unnest_tokens(word, post_copy) %>%
  anti_join(stop_words)


#wordcount by ID
word_count <- post %>%
  unnest_tokens(word, post_copy) %>%
  group_by(Creative.ID) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words)) 

word_count[1:10,] %>%
  ungroup(num_words, Creative.ID) %>%
  mutate(num_words = color_bar("lightblue")(num_words)) %>%
  mutate(Creative.ID = color_tile("lightpink","lightpink")(Creative.ID)) %>%
  kable("html", escape = FALSE, align = "c", caption = "posts with highest word count") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)

#word count histogram
#can add fill = to geom_histogram to fill color by another dimension (partition CTR??)
word_count %>%
  ggplot() +
  geom_histogram(aes(x = num_words, fill = )) + 
  ylab("Ad Count") + 
  xlab("Word Count per Ad") +
  ggtitle("Word Count Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank())

#most frequently used words
post_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n)) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Ad Count") +
  ggtitle("Most Frequently Used Words in AT&T Ads") +
  coord_flip()

#word cloud
words_counts <- post_filtered %>%
  count(word, sort = TRUE) 

wordcloud2(words_counts[1:300, ], size = .5)

#word cloud shape
#wordcloud2(prince_words_counts[1:300, ], figPath = "guitar_icon.png", 
#           color = "random-dark", size = 1.5)

#letterCloud(prince_words_counts[1:300, ], word = "PRINCE", size = 2)

#most popular words
success_words <- post_filtered %>% 
  group_by(top_ctr) %>%
  count(word, top_ctr, sort = TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(top_ctr,n) %>%
  mutate(row = row_number())

success_words %>%
  ggplot(aes(row, n, fill = top_ctr)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "ad count") +
  ggtitle("success Words by CTR") + 
  #theme_lyrics() +  
  facet_wrap(~top_ctr, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = success_words$row, # notice need to reuse data frame
    labels = success_words$word) +
  coord_flip()







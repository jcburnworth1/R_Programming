##### LDA Notes #####
## mod <- LDA(x = dtm, k = 2, method = 'Gibbs', control=list(alpha=1, delta=0.1, seed=10005))
## Random search through the space of parameters to find the model with the largest log-likelihood
## Likelihood = Plausability of parameters in the model given the data
## Gibbs Sampling = a type of Monte Carlo Markov Chain (MCMC) algorithm (method = 'Gibbs')
## MCMC tries different combinations of probability of topics in documents
## alpha = The alpha controls the mixture of topics for any given document. Turn it down
## and the documents will likely have less of a mixture of topics. Turn it up and the documents
## will likely have more of a mixture of topics.
## seed = Specified to ensure replication of results
## iter = controls the number of iterations of the algorithm - More iterations - Better model but this takes longer
## thin - Specified how often to return the results of each searc - Best result is picked

##### Load Required Libraries #####
require(tidytext); require(topicmodels); require(tidyverse); require(wordcloud);

##### Topic Modeling in R Scratch #####
##### Counting Words #####
## Specify the input column
word_freq <- chapters %>% 
  unnest_tokens(output = word, 
                input = text, 
                token = "words", 
                format = "text") %>% 
  ## Obtain word frequencies
  count(chapter, word) 

## Test equality
word_freq %>% filter(word == "after")

##### Topic Modeling Example 1 #####
dtm <- corpus %>% 
  # Specify the input column
  unnest_tokens(input = text, output = word, drop = TRUE) %>% 
  count(id, word) %>% 
  # Specify the token
  cast_dtm(document = id, term = word, value = n)

mod <- LDA(x = dtm, k = 2, method = "Gibbs",
           control = list(alpha = 1, delta = 0.1, seed = 10005))

posterior(mod)$topics

##### Topic Modeling Hands On #####
## Example Corpus
corpus <- data.frame('id' = c('d_1','d_2','d_3','d_4','d_5'),
                     'text' = c('Due to bad loans, the bank agreed to pay the fines',
                                'If you are late to pay off your loans to the bank, you will face fines',
                                'A new restaurant opened in downtown',
                                'There is a new restaurant that just opened on Warwick street',
                                'How will you pay off the loans you will need for the restaurant you want opened?'),
                     stringsAsFactors = FALSE)

## Generate the document-term matrix
dtm <- corpus %>% 
  unnest_tokens(input = text, output = word) %>% 
  count(id, word) %>% 
  cast_dtm(document = id, term = word, value = n)

## Run the LDA for two topics
mod <- LDA(x = dtm, k = 2, method = 'Gibbs',
           control = list(alpha = 1, delta = 0.1, seed = 10005))

## beta = Contains probabilities of words in topics
## gamme = Contains probabilities of topics in documents
## Retrieve the probabilities of word `will` belonging to topics 1 and 2
tidy(mod, matrix = "beta") %>%
  filter(term == "will")

## Make a stacked column chart showing the probabilities of documents belonging to topics
## Plot gamma = Probability document is in a topic
tidy(mod, matrix = "gamma") %>% 
  mutate(topic = as.factor(topic)) %>% 
  ggplot(aes(x = document, y = gamma)) + 
  geom_col(aes(fill = topic), position = position_dodge()) + 
  geom_text(aes(label = round(gamma, 4)))

## Plot beta = Probability word is in a topic
tidy(mod, matrix = "beta") %>% 
  mutate(topic = as.factor(topic)) %>% 
  ggplot(aes(x = term, y = beta)) + 
  geom_col(aes(fill = topic), position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 90))

##### Retrieving Probabilities of words #####
## Display column names
colnames(dtm)

## Fit an LDA model for 2 topics using Gibbs sampling
mod <- LDA(x = dtm, k = 2, method="Gibbs",
           control = list(alpha = 1, seed = 10005, thin = 1))

## Convert matrix beta into tidy format and filter on topic number and term
tidy(mod, matrix = "beta") %>%
  filter(topic == 2, term == 'opened')

##### Exploring alpha #####
## alpha
## mod1
## Fit LDA topic model using Gibbs sampling for 2 topics
mod1 <- LDA(x = dtm, k = 2, method = "Gibbs",
            control = list(alpha = 1, seed = 10005, thin = 1))

## Display gamma for mod1
tidy(mod1, matrix = "gamma") %>% 
  spread(topic, gamma)

## mod2
## Fit LDA topic model using Gibbs sampling for 2 topics
mod2 <- LDA(x = dtm, k = 2, method = "Gibbs",
            control = list(alpha = 15, seed = 10005, thin = 1))

## Display gamma for mod1
tidy(mod2, matrix = "gamma") %>% 
  spread(topic, gamma)

##### Stopwords #####
## Example Corpus
corpus <- data.frame('id' = c('d_1','d_2','d_3','d_4','d_5'),
                     'text' = c('Due to bad loans, the bank agreed to pay the fines',
                                'If you are late to pay off your loans to the bank, you will face fines',
                                'A new restaurant opened in downtown',
                                'There is a new restaurant that just opened on Warwick street',
                                'How will you pay off the loans you will need for the restaurant you want opened?'),
                     stringsAsFactors = FALSE)

## Mock dictionary
dictionary <- data.frame(word = c('bank','fines','loans','pay',
                                  'new','opened','restaurant'), stringsAsFactors = FALSE)

## Generate the document-term matrix
dtm <- corpus %>% 
  unnest_tokens(input = text, output = word) %>% 
  ## Remove stopwords - Pre-built df in the topicmodels package
  anti_join(stop_words) %>% 
  ## Keep dictionary words - Custom list of words to keep
  # inner_join(dictionary) %>%
  count(id, word) %>% 
  cast_dtm(document = id, term = word, value = n)

## Display the dtm as a matrix
as.matrix(dtm)

##### Wordclouds #####
## Example Corpus
corpus <- data.frame('id' = c('d_1','d_2','d_3','d_4','d_5'),
                     'text' = c('Due to bad loans, the bank agreed to pay the fines',
                                'If you are late to pay off your loans to the bank, you will face fines',
                                'A new restaurant opened in downtown',
                                'There is a new restaurant that just opened on Warwick street',
                                'How will you pay off the loans you will need for the restaurant you want opened?'),
                     stringsAsFactors = FALSE)

## Generate the counts of words in the corpus
word_frequencies <- corpus %>% 
  unnest_tokens(input = text, output = word) %>%
  count(word)

## Create a wordcloud
wordcloud(words = word_frequencies$word,
          freq = word_frequencies$n,
          min.freq = 1,
          max.words = 10,
          colors = c('DarkOrange','Blue'),
          random.order = FALSE,
          random.color = FALSE)

##### History of the Byzantine Empire #####
## Load gutenbergr to download the needed text
require(gutenbergr)

## Download the book
history <- gutenberg_download('37756')

## See https://campus.datacamp.com/courses/topic-modeling-in-r/wordclouds-stopwords-and-control-arguments?ex=11
## Below not currently working as downloaded data is different
## Construct a document-term matrix
# Construct a document-term matrix
dtm <- history %>% 
  unnest_tokens(input = text, output = word) %>% 
  anti_join(stop_words) %>% 
  count(chapter, word) %>% 
  cast_dtm(document = chapter, term = word, value = n)

## Fit an LDA model
mod <- LDA(dtn, k = 4, method = 'Gibbs',
           control = list(alpha = 1, seed = 10005))

## Display top 15 words of each topic
terms(mod, k = 15)

## Display the structure of the verbs dataframe
str(verbs)

## Construct a document-term matrix
dtm <- history %>% 
  unnest_tokens(input = text, output = word) %>% 
  inner_join(stop_words, by=c("word"="word")) %>% 
  count(chapter, word) %>% 
  cast_dtm(document = chapter, term = word, value = n)

## Fit an LDA model
mod <- LDA(dtm, k = 4, method = 'Gibbs',
           control = list(alpha = 1, seed = 10005))

## Plot the model results
# Extract matrix gamma and plot it
tidy(mod, "gamma") %>% 
  mutate(document = as.numeric(document)) %>% 
  ggplot(aes(x = document, y = gamma)) + 
  geom_col(aes(fill = factor(topic)))

## Display the words whose probability is above the threshold
terms(mod, threshold = 0.0075)

## Extract matrix gamma and plot it
tidy(mod, "gamma") %>% 
  mutate(document = as.numeric(document)) %>% 
  ggplot(aes(x = document, y = gamma)) + 
  geom_line(aes(color = factor(topic))) + 
  labs(x = "Chapter", y = "Topic Probability") +
  scale_color_manual(values = brewer.pal(n = 4, "Set1"), name = "Topic")

## Display wordclouds one at a time
for (j in 1:4) {
  # Generate a table with word frequences for topic j
  word_frequencies <- tidy(mod, matrix = "beta") %>% 
    mutate(n = trunc(beta * 10000)) %>% 
    filter(topic == j)
  
  # Display word cloud
  wordcloud(words = word_frequencies$term, 
               freq = word_frequencies$n,
               max.words = 20,
               scale = c(3, 0.5),
               colors = c("DarkOrange", "CornflowerBlue", "DarkRed"), 
               rot.per = 0.3)
}

##### Topic Models for Classification: Impact of alpha #####
## Alpha impact on modeling
## Parameter alpha determines the values of probabilities that a 
## document belongs to a topic. Parameter delta does the same for
## probability distribution of words over topics. By default,
## delta is set to 0.1. You will fit a model with a different
## delta and make a plot of results.

## df working with
the_corpus <- data.frame('id' = c('d_1','d_2','d_3','d_4','d_5'),
                         'text' = c('Due to bad loans, the bank agreed to pay the fines',
                                    'If you are late to pay off your loans to the bank, you will face fines',
                                    'A new restaurant opened in downtown',
                                    'There is a new restaurant that just opened on Warwick street',
                                    'How will you pay off the loans you will need for the restaurant you want opened?'),
                         stringsAsFactors = FALSE)

## Create the dtm
dtm <- the_corpus %>% 
  unnest_tokens(input = text, output = word) %>% 
  inner_join(stop_words, by=c("word"="word")) %>% 
  count(id, word) %>% 
  cast_dtm(document = id, term = word, value = n)

## Create the LDA models with alpha of 0.5, 1, 2, NULL
## 0.5
mod_0.5 <- LDA(x = dtm, k = 2, method = 'Gibbs',
               control = list(iter = 500, thin = 1, seed = 12345, alpha = 0.5))

## Display topic prevalance in documents as a table
tidy(mod_0.5, "gamma") %>% 
  mutate(topic = as.factor(topic)) %>% 
  ggplot(aes(x = document, y = gamma)) + 
  geom_col(aes(fill = topic), position = position_dodge()) + 
  geom_text(aes(label = round(gamma, 4))) + 
  labs(title = "Alpha = 0.5")

## 1
mod_1 <- LDA(x = dtm, k = 2, method = 'Gibbs',
               control = list(iter = 500, thin = 1, seed = 12345, alpha = 1))

## Display topic prevalance in documents as a table
tidy(mod_1, "gamma") %>% 
  mutate(topic = as.factor(topic)) %>% 
  ggplot(aes(x = document, y = gamma)) + 
  geom_col(aes(fill = topic), position = position_dodge()) + 
  geom_text(aes(label = round(gamma, 4))) + 
  labs(title = "Alpha = 1")

## 2
mod_2 <- LDA(x = dtm, k = 2, method = 'Gibbs',
             control = list(iter = 500, thin = 1, seed = 12345, alpha = 2))

## Display topic prevalance in documents as a table
tidy(mod_2, "gamma") %>% 
  mutate(topic = as.factor(topic)) %>% 
  ggplot(aes(x = document, y = gamma)) + 
  geom_col(aes(fill = topic), position = position_dodge()) + 
  geom_text(aes(label = round(gamma, 4))) + 
  labs(title = "Alpha = 2")

## NULL
mod_NULL <- LDA(x = dtm, k = 2, method = 'Gibbs',
             control = list(iter = 500, thin = 1, seed = 12345, alpha = NULL))

## Display topic prevalance in documents as a table
tidy(mod_NULL, "gamma") %>% 
  mutate(topic = as.factor(topic)) %>% 
  ggplot(aes(x = document, y = gamma)) + 
  geom_col(aes(fill = topic), position = position_dodge()) + 
  geom_text(aes(label = round(gamma, 4))) + 
  labs(title = "Alpha = NULL")

##### Topic Models for Classification: Impact of delta #####
## Delta impact on modeling
## df working with
the_corpus <- data.frame('id' = c('d_1','d_2','d_3','d_4','d_5'),
                         'text' = c('Due to bad loans, the bank agreed to pay the fines',
                                    'If you are late to pay off your loans to the bank, you will face fines',
                                    'A new restaurant opened in downtown',
                                    'There is a new restaurant that just opened on Warwick street',
                                    'How will you pay off the loans you will need for the restaurant you want opened?'),
                         stringsAsFactors = FALSE)

## Create the dtm
dtm <- the_corpus %>% 
  unnest_tokens(input = text, output = word) %>% 
  inner_join(stop_words, by=c("word"="word")) %>% 
  count(id, word) %>% 
  cast_dtm(document = id, term = word, value = n)

## Words to exam
my_terms = c("loans", "bank", "opened", "pay", "restaurant", "you")

## delta = 0.1
mod_0.1 <- LDA(x = dtm, k = 2, method = 'Gibbs',
             control = list(iter = 500, seed = 12345, alpha = 1, delta = 0.1))

## Tidy the model for plotting
t <- tidy(mod_0.1, "beta") %>% filter(term %in% my_terms)

# Make a stacked column chart of word probabilities
ggplot(t, aes(x = term, y = beta)) +
  geom_col(aes(fill =factor(topic))) +
  theme(axis.text.x = element_text(angle = 90))
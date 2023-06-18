library("quanteda")
library("readxl")
library("dplyr")
library("tidyverse")
library("stringr")
library("tidytext")
library("lubridate")
library("plotly")
library("htmlwidgets")
library("wordcloud2")
press <- read_xlsx("CMFA_PressCon_v3.xlsx")
press$date <- as.Date(press$date)
press$month <- format(press$date, "%Y-%m")
press$month <- ymd(paste(press$month, "01"))

us <- press[str_detect(press$q_loc,'US|United states|U.S.|White House|Washington D.C|Washington'), ]
colnames(us)[13] ="text"
us %>% rename("answer_lem" = "text")
us_cor <- corpus(us)
us_cor <- tokens(us_cor, "word", remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_remove(stopwords('en')) %>%
  tokens_wordstem(language = "en")

us_cor[1]

bing <- get_sentiments("bing")
dict_sent_bing <- as.dictionary(bing)
bing_us <- tokens_lookup(us_cor, dict_sent_bing,
                                        valuetype = "glob") %>% dfm()
bing_us%>%dfm_group(month)
bing_us_df <- convert(bing_us, "data.frame") %>%
  bind_cols(docvars(bing_us))
bing_us_df <- bing_us_df %>%
  mutate(sentiment = log((positive + 0.5)/(negative + 0.5)))

bing_us_df %>%
  group_by(month) %>%
  select(positive, negative, sentiment, month) %>%
  summarize_all(mean)

monthly_avg <- aggregate(sentiment ~ month, bing_us_df, mean)

plot <- plot_ly(monthly_avg, x = ~month, y = ~sentiment, type = "scatter", mode = "lines",
                name = "Sentiment", line = list(color = "blue"))
plot <- layout(plot, title = "Sentiment Analysis of US-China Relations (Monthly)",
               xaxis = list(title = "Month", titlefont = list(size = 14)),
               yaxis = list(title = "Sentiment", titlefont = list(size = 14)),
               plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)")
library(dplyr)

monthly_avg <- monthly_avg %>%
  arrange(month) %>%
  mutate(moving_avg = zoo::rollmean(sentiment, k = 3, fill = NA, align = "right"))

plot <- plot_ly() %>%
  add_trace(
    x = ~monthly_avg$month,
    y = ~monthly_avg$sentiment,
    type = "scatter",
    mode = "lines",
    name = "Sentiment",
    line = list(color = "lightblue")
  ) %>%
  add_trace(
    x = ~monthly_avg$month,
    y = ~monthly_avg$moving_avg,
    type = "scatter",
    mode = "lines",
    name = "Moving Average",
    line = list(color = "red")
  ) %>%
  layout(
    title = "Sentiment Analysis of US-China Relations (Monthly)",
    xaxis = list(title = "Month", titlefont = list(size = 14)),
    yaxis = list(title = "Sentiment", titlefont = list(size = 14)),
    plot_bgcolor = "rgb(240, 240, 240)",
    paper_bgcolor = "rgb(240, 240, 240)",
    showlegend = TRUE,
    legend = list(x = 1, y = 1, bgcolor = "rgba(255, 255, 255, 0.5)")
  )

plot <- plot %>% add_annotations(
  x = ~"2005-09-01", y = ~0.533066, xref= "x", yref = "y", text = ~paste("Responsible Stakeholder"),
  showarrow = TRUE, arrowhead = 2, ax = -5, ay = 30, font = list(size = 8)
) %>% add_annotations(
  x = ~"2006-10-01", y = -0.2034709, xref= "x", yref = "y", text = ~paste("2006 North Korean nuclear test"),
  showarrow = TRUE, arrowhead = 2, ax = 5, ay = 30, font = list(size = 8)
)%>% add_annotations(
  x = ~"2008-08-01", y = ~0.90565640, xref= "x", yref = "y", text = ~paste("Peking Olymipics"),
  showarrow = TRUE, arrowhead = 2, font = list(size = 8)
)%>% add_annotations(
  x = ~"2012-11-01", y = -0.06297935, xref= "x", yref = "y", text = ~paste("Xi Jinping in Office"),
  showarrow = TRUE, arrowhead = 2, ax = 5, ay = 30, font = list(size = 8)
)%>% add_annotations(
  x = ~"2014-10-01", y = -0.2441341, xref= "x", yref = "y", text = ~paste("2014 Hongkong Protests"),
  showarrow = TRUE, arrowhead = 2, ax = 5, ay = 30, font = list(size = 8)
)%>% add_annotations(
  x = ~"2016-07-01", y = ~0.1849464, xref= "x", yref = "y", text = ~paste("THAAD in South Korea"),
  showarrow = TRUE, arrowhead = 2, ax = 5, ay = 30, font = list(size = 8)
)%>% add_annotations(
  x = ~"2017-01-01", y = ~0.8709168943, xref= "x", yref = "y", text = ~paste("Trump in Office"),
  showarrow = TRUE, arrowhead = 2, ax = 20, ay= -40, font = list(size = 8)
)%>% add_annotations(
  x = ~"2018-03-01", y = ~0.7351988944, xref= "x", yref = "y", text = ~paste("Trade War"),
  showarrow = TRUE,  arrowhead = 2,  ax= -30, ay = -20,  font = list(size = 8)
)%>% add_annotations(
  x = ~"2018-09-01", y = ~0.7426555531, xref= "x", yref = "y", text = ~paste("2nd-round taxation"),
  showarrow = TRUE, arrowhead = 2,  font = list(size = 8)
)%>% add_annotations(
  x = ~"2018-12-01", y = ~0.6507374502, xref= "x", yref = "y", text = ~paste("2nd-round negotiation"),
  showarrow = TRUE, arrowhead = 2, ax = -5, ay = 30, font = list(size = 8)
)%>% add_annotations(
  x = ~"2019-05-01", y = ~0.6160053045, xref= "x", yref = "y", text = ~paste("3rd-round taxation"),
  showarrow = TRUE, arrowhead = 2,  ax= 40, ay = -40, font = list(size = 8)
)%>% add_annotations(
  x = ~"2020-01-01", y = ~0.3389319526, xref= "x", yref = "y", text = ~paste("Covid-19"),
  showarrow = TRUE, arrowhead = 2, ax = 5, ay= -30, font = list(size = 8)
)%>% add_annotations(
  x = ~"2021-03-01", y = 0.3220248, xref= "x", yref = "y", text = ~paste("First meeting since Biden"),
  showarrow = TRUE, arrowhead = 2, ax = 20, ay = -40,  font = list(size = 8)
)%>% add_annotations(
  x = ~"2022-02-01", y = -0.1163095526, xref= "x", yref = "y", text = ~paste("War in Ukraine"),
  showarrow = TRUE, arrowhead = 2, ax = -5, ay = 40,  font = list(size = 8)
)%>% add_annotations(
  x = ~"2022-07-01", y = -0.039647, xref= "x", yref = "y", text = ~paste("Pelosi to Taiwan"),
  showarrow = TRUE, arrowhead = 2, ax = 5, ay = -30,  font = list(size = 8)
)

plot
### Api key is removed ###
api_create(plot, filename = "Blogbeitrag")
saveWidget(plot, "interactive_plot.html", selfcontained = TRUE)

### Words clound ###
start_date <- as.Date("2005-12-01")
end_date <- as.Date("2006-09-01")

decline1 <- us %>% 
  filter(month >= start_date & month <= end_date)
decline_cor <- corpus(decline1)
decline_cor <- tokens(decline_cor, "word", remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_remove(stopwords('en')) %>%
  tokens_wordstem(language = "en")

dfm1 <- TermDocumentMatrix(decline_cor)
wordfreq1 <- rowSums(as.matrix(dfm1))
wordfreq1 <- wordfreq1[wordfreq1 >= 30 & wordfreq1 <= 70]

wordfreq_df <- data.frame(word = names(wordfreq1), freq = wordfreq1)
wordcould1 <- wordcloud2(data = wordfreq_df, size = 0.3)
saveWidget(wordcould1, "wordcloud1.html", selfcontained = TRUE)
##### 2 ######
start_date <- as.Date("2018-03-01")
end_date <- as.Date("2019-05-01")

decline2 <- us %>% 
  filter(month >= start_date & month <= end_date)

decline_cor2 <- corpus(decline2)
decline_cor2 <- tokens(decline_cor2, "word", remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_remove(stopwords('en')) %>%
  tokens_wordstem(language = "en")

dfm2 <- TermDocumentMatrix(decline_cor2)
wordfreq2 <- rowSums(as.matrix(dfm2))
wordfreq2 <- wordfreq2[wordfreq2 >= 100 & wordfreq2 <= 1000]

wordfreq_df2 <- data.frame(word = names(wordfreq2), freq = wordfreq2)
wordcould2 <- wordcloud2(data = wordfreq_df2, size =0.5)
wordcould2

#########Wolfratio#############
zhao <- filter(press, spokesperson == 'Zhao Lijian')
colnames(zhao)[13] <- "text"

other <- filter(press, spokesperson != 'Zhao Lijian' & month %in% unique(zhao$month))
colnames(other)[13] <- "text"

nrc <- get_sentiments("nrc")
dict_sent_nrc <- as.dictionary(nrc)

zhao_cor <- corpus(zhao)
zhao_cor <- tokens(zhao_cor, "word", remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
    tokens_remove(stopwords('en')) %>%
    tokens_wordstem(language = "en")
  
nrc_zhao <- tokens_lookup(zhao_cor, dict_sent_nrc, valuetype = "glob") %>% dfm()
nrc_zhao%>%dfm_group(month)
nrc_zhao_df <- convert(nrc_zhao, "data.frame") %>%
  bind_cols(docvars(nrc_zhao))

nrc_zhao_df <-  nrc_zhao_df %>%
  mutate(wolfratio = anger + disgust)
  
nrc_zhao_df %>%
  group_by(month) %>%
  select(wolfratio, month) %>%
  summarize_all(mean)
  
other_cor <- corpus(other)
other_cor <- tokens(other_cor, "word", remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
    tokens_remove(stopwords('en')) %>%
    tokens_wordstem(language = "en")
  
nrc_other <- tokens_lookup(other_cor, dict_sent_nrc, valuetype = "glob") %>% dfm()
nrc_other_df <- convert(nrc_other, "data.frame") %>%
    bind_cols(docvars(nrc_other))

nrc_other_df <-  nrc_other_df %>%
  mutate(wolfratio = anger + disgust)

nrc_other_df %>%
  group_by(month) %>%
  select(wolfratio, month) %>%
  summarize_all(mean)

zhao_plot <- nrc_zhao_df %>%
  group_by(month) %>%
  summarize(mean_wolfratio = mean(wolfratio))

other_plot <- nrc_other_df %>%
  group_by(month) %>%
  summarize(mean_wolfratio = mean(wolfratio))

combined_plot <- bind_rows(zhao_plot, other_plot, .id = "spokesperson")

wolf<- ggplot(combined_plot, aes(x = month, y = mean_wolfratio, color = spokesperson)) +
  geom_line() +
  labs(x = "Month", y = "Mean Wolfratio", color = "Spokesperson") +
  scale_color_manual(values = c("red", "lightblue"), 
                     labels = c("Zhao Lijian", "Other")) +
  ggtitle("Wolfratio: Zhao Lijian and other spokespersons") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.title.position = "plot")

ggplotly(wolf)

wolfratio <- plot_ly(data = combined_plot, x = ~month, y = ~mean_wolfratio, 
        colors  = list(color = ~spokesperson, colors = c("red", "lightblue")), 
        name = ~factor(spokesperson, labels = c("Zhao Lijian", "Other"))) %>%
  add_lines() %>%
  layout(xaxis = list(title = "Month"),
         yaxis = list(title = "Mean Wolfratio"),
         legend = list(title = "Spokesperson"),
         title = "Wolfratio: Zhao Lijian and other spokespersons",
         showlegend = TRUE)
wolfratio

api_create(wolfratio, filename = "Blogbeitrag2")

import googleapiclient.discovery
import pandas as pd
import nltk
# Since nltk always has SSL certificate error, I need to forbidde it by the following code
import ssl
try:
    _create_unverified_https_context = ssl._create_unverified_context
except AttributeError:
    pass
else:
    ssl._create_default_https_context = _create_unverified_https_context
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from nltk.stem import WordNetLemmatizer
from langdetect import detect
# Set up Youtube API
api_service_name="youtube"
api_version ="v3"
DEVELOPER_KEY="AIzaSyCj07DTGus74Jz0hFFI6AeXaW3jE5Ki3tA"
youtube = googleapiclient.discovery.build(api_service_name,api_version,developerKey=DEVELOPER_KEY)
# Prepare a list to store comments data
comments_data = []

# Crawling comments
response = youtube.commentThreads().list(
    part="snippet",
    videoId="_bHT1JoEHFA",  # Replace with your video ID
    maxResults=100
).execute()

# Rotation of comments
while response:
    for item in response['items']:
        comment = item['snippet']['topLevelComment']['snippet']
        comments_data.append([
            comment['publishedAt'],
            comment['updatedAt'],
            comment['likeCount'],
            comment['textDisplay']
        ])
    
    if 'nextPageToken' in response:
        response = youtube.commentThreads().list(
            part="snippet",
            videoId="_bHT1JoEHFA",  # Replace with your video ID
            maxResults=100,
            pageToken=response['nextPageToken']
        ).execute()
    else:
        break

# Create a DataFrame
df = pd.DataFrame(comments_data, columns=['publishedAt', 'updatedAt', 'likeCount', 'textDisplay'])

# Drop duplicate comments
df.drop_duplicates(subset=['textDisplay'], inplace=True)

# Set up language detection function
def detect_lang(x):
    try:
        return detect(x)
    except:
        return "error"
    
df['language'] = df['textDisplay'].apply(detect_lang)

# Filter out non-English comments
df_english = df[df['language'] == 'en']

# Define a function for text preprocessing
def preprocess_text(text):
    # Tokenization
    tokens = word_tokenize(text.lower())

    # Remove punctuation and stopwords
    tokens = [word for word in tokens if word.isalnum() and word not in stopwords.words('english')]

    # Lemmatization
    lemmatizer = WordNetLemmatizer()
    tokens = [lemmatizer.lemmatize(word) for word in tokens]

    return ' '.join(tokens)

# Conduct text preprocessing
df_english['preprocessed'] = df_english['textDisplay'].apply(preprocess_text)

# Sentiment analysis
nltk.download("vader_lexicon")
from nltk.sentiment.vader import SentimentIntensityAnalyzer
sia=SentimentIntensityAnalyzer()

def get_sentiment(text):
    return sia.polarity_scores(text)['compound']

df_english['sentiment'] = df_english['preprocessed'].apply(get_sentiment)

# To save time, I create a CSV file
df_english.to_csv('youtube_comments.csv', index=False)

# Plot the distribution of sentiment
numpos = len(df_english[df_english["sentiment"] > 0])
numneg = len(df_english[df_english["sentiment"] < 0])
numneu = len(df_english[df_english["sentiment"] == 0])
print(numpos,numneg,numneu)

# Import matplotlib and plot the sentiment
import matplotlib.pyplot as plt
plt.bar(["posit","negat","neut"],[numpos,numneg,numneu],color=['green', 'red', 'blue'])
plt.xlabel('Variables')
plt.ylabel('Values')
plt.title('Bar Plot of posit,negat,neut')
plt.suptitle('Figure 1', fontsize=14, fontweight='bold')
# Display the plot
plt.show()

# Plot the scatter plot of sentiment score vs. like count
plt.scatter(df_english['sentiment'], df_english['likeCount'], alpha=0.5)  # alpha adds transparency

# Add labels and title
plt.xlabel('Sentiment Score')
plt.ylabel('Like Count')
plt.title('Scatter Plot of Sentiment Score vs. Like Count')
plt.suptitle('Figure 2', fontsize=14, fontweight='bold')
# Show the plot
plt.show()

from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler

# Select the features you want to cluster on
X = df_english[['sentiment', 'likeCount']]

# Standardize the features becuase like-count varies 0 to 2500 and sentiment varies -1 to 1
scaler = StandardScaler()
X_standardized = scaler.fit_transform(X)

# Specify the number of clusters (you can adjust this based on your needs)
num_clusters = 3

# Create the K-means clustering model
kmeans = KMeans(n_clusters=num_clusters, random_state=0)

# Fit the model to your standardized data
kmeans.fit(X_standardized)

# Add cluster labels to your DataFrame
df_english['cluster_label'] = kmeans.labels_

# Visualize the clusters
plt.figure(figsize=(8, 6))
for label in range(num_clusters):
    cluster_data = df_english[df_english['cluster_label'] == label]
    plt.scatter(cluster_data['sentiment'], cluster_data['likeCount'], label=f'Cluster {label + 1}')

# Add labels and title
plt.xlabel('Sentiment Score (Standardized)')
plt.ylabel('Like Count (Standardized)')
plt.title('K-means Clustering of Standardized Features: Sentiment Score vs. Like Count')
plt.suptitle('Figure 3', fontsize=14, fontweight='bold')
# Add a legend
plt.legend()

# Show the plot
plt.show()

# Calculate the correlation coefficient
from scipy.stats import pearsonr
coor, p_value=pearsonr(df_english["sentiment"],df_english["likeCount"])
print(coor,p_value)
# 0.04754878727341977 0.04729106461812696

# Calculate the correlation coefficient for negative comments
neg_df = df_english[df_english['sentiment'] < 0]
correlation, p_value = pearsonr(neg_df['sentiment'], neg_df['likeCount'])
print(coor,p_value)
# 0.04754878727341977 0.10502566884630948

# However，the sentiment above don't mean anything, because it's bases on scores not contents.
# Mathmatical calculation can't understand the meaning of words.
# For political science, what makes sense is to measure the polarity in public opinion.
from nltk.tokenize import sent_tokenize   
def plot_sentiment_by_sentence(df, keywords, title):
    sentiments = []
    like_counts = []
    
    for index, row in df.iterrows():
        sentences = sent_tokenize(row['textDisplay'])
        for sentence in sentences:
            if any(keyword.lower() in sentence.lower() for keyword in keywords):
                sentiments.append(row['sentiment'])
                like_counts.append(row['likeCount'])
                
    plt.scatter(sentiments, like_counts, alpha=0.5)
    plt.title(title)
    plt.xlabel('Sentiment')
    plt.ylabel('Like Count')
    plt.show()

# Polarity_scores() returns a dictionary of scores
plot_sentiment_by_sentence(df_english, ["Israel", "Israeli", "IDF"], "Sentiment for Israel by Sentence")
plot_sentiment_by_sentence(df_english, ["Hamas"], "Sentiment for Palestine by Sentence")

# But problem: When two keywords appear in the same sentence, the sentiment score will be counted twice.
# So I will measure the sentence's sentiment to different keywords.
# Named Entity Recognition (NER) can divide sentences into different parts of speech.
import spacy
nlp = spacy.load("en_core_web_sm")

sia = SentimentIntensityAnalyzer()

def analyze_entity_sentiment_vader(texts, entity):
    positive, negative, neutral = 0, 0, 0
    
    for text in texts:
        doc = nlp(text)
        for ent in doc.ents:
            if ent.text.lower() == entity.lower() and ent.label_ in ["ORG", "PRODUCT", "PERSON", "GPE"]:# !!Israel can only label as GPE
                scores = sia.polarity_scores(text)
                
                if scores['compound'] > 0:
                    positive += 1
                elif scores['compound'] < 0:
                    negative += 1
                else:
                    neutral += 1
                
    return positive, negative, neutral



def visualize_sentiment(positive, negative, neutral, title):
    labels = ['Positive', 'Negative', 'Neutral']
    sizes = [positive, negative, neutral]
    colors = ['#ff9999','#66b3ff','#99ff99']
    
    fig1, ax1 = plt.subplots()
    ax1.pie(sizes, colors = colors, labels=labels, autopct='%1.1f%%', startangle=90)
    ax1.axis('equal')  # Draw plot as a pie
    plt.title(title)
    plt.show()

positive, negative, neutral = analyze_entity_sentiment_vader(df_english["textDisplay"], "hamas")
visualize_sentiment(positive, negative, neutral, title="Figure 6: Sentiment Analysis of Hamas")

positive, negative, neutral = analyze_entity_sentiment_vader(df_english["textDisplay"], "Israel")
visualize_sentiment(positive, negative, neutral, title="Figure 7: Sentiment Analysis of Israel")

positive, negative, neutral = analyze_entity_sentiment_vader(df_english["textDisplay"], "Palestine")
visualize_sentiment(positive, negative, neutral, title="Figure 8: Sentiment Analysis of Palestine")
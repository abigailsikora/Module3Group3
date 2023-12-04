import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import time
import nltk
from nltk.sentiment import SentimentIntensityAnalyzer
from tqdm.notebook import tqdm
from textblob import TextBlob
from sklearn.feature_extraction.text import TfidfVectorizer
from wordcloud import WordCloud

plt.style.use('ggplot')
nltk.download('punkt')
nltk.download('vader_lexicon')


sia = SentimentIntensityAnalyzer()

# Read in data
df = pd.read_csv('filtered_reviews.csv')
# insert ID column for later use
df.insert(0, 'id', range(1, 1 + len(df)))

# Run the polarity score on the entire dataset
# create Result dictionary res{}
res = {}
for i, row in tqdm(df.iterrows(), total=len(df)):
    text = row['text']
    myid = row['id']
    res[myid] = sia.polarity_scores(text)

# Convert dictionary to dataframe
# dataframe is oriented wrong way, run .T to pivot table
vaders = pd.DataFrame(res).T
# Merge onto original dataframe
vaders = vaders.reset_index().rename(columns={'index': 'id'})
vaders = vaders.merge(df, how='left')
# seaborn barplot to view VADERS

ax = sns.barplot(data=vaders, x='stars', y='compound')
ax.set_title('Sentiment Score by Review')
plt.savefig("compund score by review")
plt.show()

plt.figure(figsize=(10, 6))
plt.bar(sorted_df['Category'], sorted_df['Frequency'], color='skyblue')
plt.xlabel('Atmosphere')
plt.ylabel('Reviews')
plt.title('Reviews of Atmosphere')
plt.xticks(rotation=45)
plt.savefig("Reviews of Atmosphere")
plt.show()

df = pd.read_csv("filtered_reviews.csv")
df['sentiment'] = df['text'].apply(lambda x: TextBlob(str(x)).sentiment.polarity)
df.to_csv('review_sentiment.csv', index=False)

positive_reviews = df[df['stars'].isin([4, 5])]
negative_reviews = df[df['stars'].isin([1, 2])]
positive_reviews.to_csv('positive_reviews.csv', index=False)
negative_reviews.to_csv('negative_reviews.csv', index=False)


#tokenizer

from spacy.tokens import Token
from spacy.lang.en.stop_words import STOP_WORDS 
import pandas as pd
from sklearn.feature_extraction.text import TfidfVectorizer
from wordcloud import WordCloud
import matplotlib.pyplot as plt
import spacy
nlp = spacy.load('en_core_web_sm')

stop_words_getter = lambda token: token.is_stop or token.lower_ in STOP_WORDS or token.lemma_ in STOP_WORDS
Token.set_extension('is_stop', getter=stop_words_getter)
def filter_token(token):
    if(token.is_punct or \
       token.is_digit or \
       token.is_space or \
       token.like_num or \
       token.lemma_ == '-PRON-' or \
       token._.is_stop):
        return(False)
    else:
        return(True)
df=pd.read_csv("review_token.csv")
df = df.dropna(subset=['processed_text'])

from sklearn.feature_extraction.text import TfidfVectorizer

custom_stop_words = ['good', 'great', 'nashville', 'come', 'order', 'bar', 'like', 'love', 'amazing', 'try',\
                     'nice', 'definitely', 'eat', 'recommend', 'awesome', 'visit', 'want', 'look', 'little', 'enjoy','bad']

def get_top_words(reviews):
    vectorizer = TfidfVectorizer(max_df=0.5, min_df=10, max_features=1000, stop_words=custom_stop_words)
    X = vectorizer.fit_transform(reviews['processed_text'])
    feature_names = vectorizer.get_feature_names_out()

    # Corrected line to use the TF-IDF matrix generated in this function
    tfidf_dict = dict(zip(feature_names, X.sum(axis=0).A1))
    
    # Get the indices along the feature axis
    indices = X.sum(axis=0).argsort()[0, ::-1]
    sorted_feature_names = [feature_names[i] for i in indices]
    
    return sorted_feature_names

def generate_wordcloud(words, title):
    # Convert any non-string elements to strings
    words = [str(word) for word in words]
    wordcloud = WordCloud(width=800, height=400, background_color='white').generate(' '.join(words))
    plt.figure(figsize=(8, 5))
    plt.imshow(wordcloud, interpolation='bilinear')
    plt.axis('off')
    plt.title(title)
    plt.savefig(title+'.png')
    plt.show()

generate_wordcloud(top_words, 'Reviews WordCloud')

positive_token=pd.read_csv('positive_token.csv')
negative_token=pd.read_csv('negative_token.csv')
positive_top_words=get_top_words(positive_token)
negative_top_words=get_top_words(negative_token)

print(positive_top_words)
print(negative_top_words)

generate_wordcloud(positive_top_words, 'Positive Reviews WordCloud')
generate_wordcloud(negative_top_words, 'Negative Reviews WordCloud')

positive_token['processed_text'] = positive_token['processed_text'].str.lower()
negative_token['processed_text'] = negative_token['processed_text'].str.lower()
# Calculate frequency of the word 'service' in positive reviews
service_frequency_positive = positive_token['processed_text'].str.contains('service').sum()

print("Frequency of 'service' in positive reviews:", service_frequency_positive/len(positive_token['processed_text']))

import pandas as pd
def calculate_word_frequency(file, target_word):
    file=pd.read_csv(file)
    file['processed_text'] = file['processed_text'].str.lower()
    # Count the number of reviews where the text contains the target word
    reviews_with_word = file[file['processed_text'].str.contains(target_word, case=False)]  
    total_rows = len(file['processed_text'])
    count = len(reviews_with_word)
    print(f"Frequency of the word '{target_word}': {count/total_rows* 100}%")

# Example usage
# Replace 'positive_reviews.csv' with the actual file path containing your positive reviews data
calculate_word_frequency('positive_token.csv', 'service')
calculate_word_frequency('positive_token.csv', 'place')
calculate_word_frequency('positive_token.csv', 'food')
calculate_word_frequency('positive_token.csv', 'time')
calculate_word_frequency('positive_token.csv', 'drink')

calculate_word_frequency('negative_token.csv', 'service')
calculate_word_frequency('negative_token.csv', 'place')
calculate_word_frequency('negative_token.csv', 'food')
calculate_word_frequency('negative_token.csv', 'time')
calculate_word_frequency('negative_token.csv', 'drink')

#print complete reviews
def find_reviews_with_word(file, target_word, num_reviews=5):
    file=pd.read_csv(file)
    file['processed_text'] = file['processed_text'].str.lower()

    # Find and display five reviews where the text contains the target word
    pd.set_option('display.max_colwidth', 1000)
    reviews_with_word = file[file['processed_text'].str.contains(target_word, case=False)].head(num_reviews)
   
    print(f"{num_reviews} reviews with the word '{target_word}':")
    print(reviews_with_word[['processed_text']])

# Example usage
# Replace 'negative_reviews.csv' with the actual file path containing your negative reviews data
find_reviews_with_word('negative_token.csv', 'service', num_reviews=5)

#woed frequency analysis
data = pd.read_csv('review_sentiment.csv')
filtered_data = data[data['text'].str.lower().str.contains('beer')]
average_sentiment = filtered_data['sentiment'].mean()
standard_error = filtered_data['sentiment'].sem()  # sem() calculates the standard error of the mean
print("Average Sentiment of cocktail:", average_sentiment)
print("Standard Errorof cocktail:", standard_error)
print(filtered_data.shape[0])
filtered_data = data[data['text'].str.lower().str.contains('cocktail|cocktails')]
average_sentiment = filtered_data['sentiment'].mean()
standard_error = filtered_data['sentiment'].sem()  # sem() calculates the standard error of the mean
print("Average Sentiment of cocktail:", average_sentiment)
print("Standard Errorof cocktail:", standard_error)
print(filtered_data.shape[0])

filtered_data = data[data['text'].str.lower().str.contains('wine|wines')]
average_sentiment = filtered_data['sentiment'].mean()
standard_error = filtered_data['sentiment'].sem()  # sem() calculates the standard error of the mean
print("Average Sentiment of wine:", average_sentiment)
print("Standard Errorof wine:", standard_error)
print("brandy:",filtered_data.shape[0])

filtered_data = data[data['text'].str.lower().str.contains('whisky|whiskies')]
average_sentiment = filtered_data['sentiment'].mean()
standard_error = filtered_data['sentiment'].sem()  # sem() calculates the standard error of the mean
print("Average Sentiment of whisky:", average_sentiment)
print("Standard Errorof whisky:", standard_error)
print("brandy:",filtered_data.shape[0])

filtered_data = data[data['text'].str.lower().str.contains('brandy|brandies')]
average_sentiment = filtered_data['sentiment'].mean()
standard_error = filtered_data['sentiment'].sem()  # sem() calculates the standard error of the mean
print("Average Sentiment of brandy:", average_sentiment)
print("Standard Errorof brandy:", standard_error)
print("brandy:",filtered_data.shape[0])

filtered_data = data[data['text'].str.lower().str.contains('vodka')]
average_sentiment = filtered_data['sentiment'].mean()
standard_error = filtered_data['sentiment'].sem()  # sem() calculates the standard error of the mean
print("Average Sentiment of vodka:", average_sentiment)
print("Standard Errorof vodka:", standard_error)
print("vodka:",filtered_data.shape[0])

filtered_data = data[data['text'].str.lower().str.contains('sake')]
average_sentiment = filtered_data['sentiment'].mean()
standard_error = filtered_data['sentiment'].sem()  # sem() calculates the standard error of the mean
print("Average Sentiment of sake:", average_sentiment)
print("Standard Errorof sake:", standard_error)
print("sake:",filtered_data.shape[0])

filtered_data = data[data['text'].str.lower().str.contains('tequila')]
average_sentiment = filtered_data['sentiment'].mean()
standard_error = filtered_data['sentiment'].sem()  # sem() calculates the standard error of the mean
print("Average Sentiment of tequila:", average_sentiment)
print("Standard Errorof tequila:", standard_error)
print("tequila:",filtered_data.shape[0])

import pandas as pd
import matplotlib.pyplot as plt

# Given data
categories = ["beer", "cocktail", "wine", "whisky", "brandy", "vodka", "sake", "tequila"]
frequencies = [20543, 10179, 7199, 202, 89, 1144, 821, 861]
df = pd.DataFrame({'Category': categories, 'Frequency': frequencies})
sorted_df = df.sort_values('Frequency', ascending=False)41041

# Creating a bar plot
plt.figure(figsize=(10, 6))
plt.bar(sorted_df['Category'], sorted_df['Frequency'], color='skyblue')
plt.xlabel('Beverage Category')
plt.ylabel('Reviews')
plt.title('Reviews of Beverage Categories')
plt.xticks(rotation=45)
plt.savefig("Reviews of Beverage Categories")
plt.show()

filtered_data = data[data['text'].str.lower().str.contains('friend')]
average_sentiment = filtered_data['sentiment'].mean()
standard_error = filtered_data['sentiment'].sem()  # sem() calculates the standard error of the mean
print("Average Sentiment of friend:", average_sentiment)
print("Standard Errorof friend:", standard_error)
print(filtered_data.shape[0])

filtered_data = data[data['text'].str.lower().str.contains('dance')]
average_sentiment = filtered_data['sentiment'].mean()
standard_error = filtered_data['sentiment'].sem()  # sem() calculates the standard error of the mean
print("Average Sentiment of dance:", average_sentiment)
print("Standard Errorof dance:", standard_error)
print(filtered_data.shape[0])

filtered_data = data[data['text'].str.lower().str.contains('music')]
average_sentiment = filtered_data['sentiment'].mean()
standard_error = filtered_data['sentiment'].sem()  # sem() calculates the standard error of the mean
print("Average Sentiment of music:", average_sentiment)
print("Standard Errorof music:", standard_error)
print(filtered_data.shape[0])

filtered_data = data[data['text'].str.lower().str.contains('wait')]
average_sentiment = filtered_data['sentiment'].mean()
standard_error = filtered_data['sentiment'].sem()  # sem() calculates the standard error of the mean
print("Average Sentiment of wait:", average_sentiment)
print("Standard Errorof wait:", standard_error)
print(filtered_data.shape[0])

filtered_data = data[data['text'].str.lower().str.contains('menu')]
average_sentiment = filtered_data['sentiment'].mean()
standard_error = filtered_data['sentiment'].sem()  # sem() calculates the standard error of the mean
print("Average Sentiment of menu:", average_sentiment)
print("Standard Errorof menu:", standard_error)
print(filtered_data.shape[0])

filtered_data = data[data['text'].str.lower().str.contains('bartender')]
average_sentiment = filtered_data['sentiment'].mean()
standard_error = filtered_data['sentiment'].sem()  # sem() calculates the standard error of the mean
print("Average Sentiment of bartender:", average_sentiment)
print("Standard Errorof bartender:", standard_error)
print(filtered_data.shape[0])

import pandas as pd
import matplotlib.pyplot as plt

# Given data
categories = ["friend", "dance", "music", "wait", "menu", "bartender"]
frequencies = [39963, 2064, 19870, 38107, 23963, 12531]
df = pd.DataFrame({'Category': categories, 'Frequency': frequencies})
sorted_df = df.sort_values('Frequency', ascending=False)

# Creating a bar plot
plt.figure(figsize=(10, 6))
plt.bar(sorted_df['Category'], sorted_df['Frequency'], color='skyblue')
plt.xlabel('Atmosphere')
plt.ylabel('Reviews')
plt.title('Reviews of Atmosphere')
plt.xticks(rotation=45)
plt.savefig("Reviews of Atmosphere")
plt.show()


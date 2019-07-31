# -*- coding: utf-8 -*-
"""
Created on Tue Jul 30 11:39:36 2019

@author: cmp2c
"""

import pandas as pd
import re

from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer

df = pd.read_csv("cleaned_body.csv")

analyzer = SentimentIntensityAnalyzer()

df.Body = df.Body.str.replace(r'\(http\S+' ,'')

df.Body = df.Body.str.replace(r'\[|\]|\(|\)|\?' ,'')     
                
sentimentScore = analyzer.polarity_scores(str(df.Body))

df['sentiment_scores'] = df['Body'].apply(analyzer.polarity_scores)

df[['neg','neu','pos','compound']] = df['sentiment_scores'].apply(pd.Series)
df.drop('sentiment_scores', axis=1, inplace = True)
df[['Id','compound']]

df[['Id','compound']].to_csv('cleaned_body_sentiment_scores.csv', index= False)
import pandas as pd
import requests
import json
import time


data = [] # list that holds scraped data

q_time = 1717505763 #max query time, June 4, 2024 12:56:03 PM
stop_q_time_at = 1531177447 #minimum query time, July 9, 2018 11:04:07 PM first WSB DD post

subreddit ="wallstreetbets"
targets = ['title', "link_flair_text", 'selftext', 'author', 'created_utc', 'upvote_ratio', 'score', "num_comments", "all_awardings", 'permalink']
targets = ",".join(targets) #so the API returns only what it needs
query_string = "" #if looking for more specific entry

def getPushshiftData(sub, quer_str, before, filters):
    url = 'https://api.pushshift.io/reddit/submission/search?q='+ quer_str+'&size=1000&subreddit='+sub+"&before="+ before + "&filter=" + filters 
    r = requests.get(url)
    if r.status_code==429: # error code for to many queries
      time.sleep(3) #pauses for 3 seconds to avoid error 429
      print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(q_time))) #prints query time stuck
      return getPushshiftData(sub, quer_str, before, filters) #trys again
    data = json.loads(r.text)
    return data['data']

while q_time>stop_q_time_at:
  holder = getPushshiftData(subreddit, query_string, str(q_time), targets) #gets data 
  data.append(pd.DataFrame(holder)) #converts to pandas and adds to data list
  if holder[len(holder)-1]["created_utc"] == q_time: #if duplicate found, stop querying
    break
  q_time = holder[len(holder)-1]["created_utc"]

sub_data = pd.concat(data) #join scraped data
sub_data = sub_data.drop_duplicates(subset=["permalink", "created_utc"]) # remove duplicates
sub_data = sub_data.loc[sub_data['link_flair_text'] == "DD"] # Only DD flair
sub_data = sub_data[['title', 'selftext', 'author', 'created_utc', 'upvote_ratio', 'score', "num_comments", "all_awardings", 'permalink']] #reorder cols

sub_data.to_csv("raw_wsb_dd_submissions.csv", index=False)
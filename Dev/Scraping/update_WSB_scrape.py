import pandas as pd
import requests
import json
import time


old = pd.read_csv("data/raw_wsb_dd_submissions.csv")

stop_q_time_at = max(old[""]) #minimum query time, January 1, 2018 12:00:00 AM


### Below is identical to previous
data = [] # list that holds scraped data
q_time = 1717505763 #max query time, June 4, 2024 12:56:03 PM
subreddit ="wallstreetbets"
targets = ['title', "link_flair_text", 'selftext', 'author', 'created_utc', 'upvote_ratio', 'score', "num_comments", "all_awardings", 'permalink']
targets = ",".join(targets)
query_string = ""

def getPushshiftData(sub, quer_str, before, filters):
    url = 'https://api.pushshift.io/reddit/submission/search?q='+ quer_str+'&size=1000&subreddit='+sub+"&before="+ before + "&filter=" + filters 
    r = requests.get(url)
    if r.status_code==429: # error code for to many queries
      time.sleep(3)
      print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(q_time))) #prints time stuck
      return getPushshiftData(sub, quer_str, before, filters) #try again
    data = json.loads(r.text)
    return data['data']

while q_time>stop_q_time_at:
   # holder time
  holder = getPushshiftData(subreddit, query_string, str(q_time), targets) #gets 
  data.append(pd.DataFrame(holder))
  if holder[len(holder)-1]["created_utc"] == q_time: #if duplicate found
    break
  q_time = holder[len(holder)-1]["created_utc"]
  #print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(q_time)))

sub_data = pd.concat(data) #join scraped data
#sub_data = sub_data.drop(columns=["index"]) # remove concat index col
sub_data = sub_data.drop_duplicates(subset=["full_link", "created_utc"]) # remove duplicates
sub_data = sub_data.reset_index() # reset index
sub_data = sub_data.loc[sub_data['link_flair_text'] == "DD"] # Only DD
sub_data = sub_data[['title', 'selftext', 'author', 'created_utc', 'upvote_ratio', 'score', "num_comments", "all_awardings", 'permalink']] #reorder cols
###


sub_data.to_csv("data/raw_wsb_dd_submissions.csv", index=False)
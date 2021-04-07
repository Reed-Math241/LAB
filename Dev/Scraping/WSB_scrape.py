import pandas as pd
import requests
import json
import time


data = [] # list that holds scraped data
recieved = 1 #recieved from pushshift 
q_time = 1717505763 #query time
subreddit ="wallstreetbets"
flair = "DD"
targets = ['title', "link_flair_text", 'selftext', 'author', 'created_utc', 'upvote_ratio', 'score', "num_comments", "num_crossposts", "all_awardings", 'full_link', ""]
targets = ",".join(targets)

def getPushshiftData(sub, before, filters):
    url = 'https://api.pushshift.io/reddit/search/submission/?size=1000&subreddit='+str(sub)+"&before="+ str(before) + "&filter=" + filters
    r = requests.get(url)
    data = json.loads(r.text)
    return data['data']

while recieved > 0 and q_time>(1417505763):
   # holder time
  holder = getPushshiftData(subreddit, str(q_time), targets)
  recieved = len(holder)
  data.append(pd.DataFrame(holder))
  #if holder[len(holder)-1]["retrieved_on"] == q_time:
  #  break
  q_time = holder[len(holder)-1]["created_utc"]
  print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(q_time)))

sub_data = pd.concat(data)
#sub_data = sub_data[['title', "link_flair_text", 'selftext', 'author', 'created_utc', 'upvote_ratio', 'score', "num_comments", "num_crossposts", "all_awardings", 'full_link']]

sub_data = sub_data.reset_index()

sub_data.to_csv("wsb_dd_submissions.csv", index=False)
import pandas as pd
import advertools

wsb_data = pd.read_csv("Data/raw_merged.csv")


def emoji_count_list(text):
    return advertools.extract_emoji(text)['top_emoji']

emojis = advertools.extract_emoji(wsb_data["title"].tolist())['emoji']



wsb_data["emojis"] = [list(set(x)) for x in emojis]
wsb_data["emoji_counts"] = [ "" if x==[] else [x.count(y) for y in set(x)] for x in emojis ]


wsb_data.to_csv("Data/raw_merged.csv", index=False)

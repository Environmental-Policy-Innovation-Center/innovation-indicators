# -*- coding: utf-8 -*-
"""
Created on Mon Oct 30 15:18:50 2023

@author: Reed
"""
import pandas as pd
import seaborn as sns
import numpy as np
from os import listdir
from os.path import isfile, join



filename = "archived_challenges_2010-2023.csv"
folder = r"C:\Users\Reed\Documents\EPIC\challenges\exports\\"

onlyfiles = [f for f in listdir(folder) if isfile(join(folder, f))]

df = pd.read_csv(folder+"Challenges.csv")
for exported in onlyfiles:
    if exported == "Challenges.csv":
        continue
    df_to_add = pd.read_csv(folder+exported)
    df = pd.concat([df,df_to_add])


df_add = df.copy()

department = []
sub_agency = []
key_agency_name = []
env_flag = []
start_year = []
prize_num = []
for i, r in df.iterrows():
    dept = r['Primary Agency Name'].split("-")[0].strip()
    department.append(dept)
    if len(r['Primary Agency Name'].split("-"))>1:
        sub_agency.append(r['Primary Agency Name'].split("-")[1].strip())
    else:
        sub_agency.append("")
    counter = 0
    for t in ["Department of the Interior","Department of Agriculture","Environmental Protection Agency","National Oceanic and Atmospheric Administration","Army Corps"]:
        if t in r['Primary Agency Name']:
            counter = counter+1
    
    if counter>0:
        env_flag.append("Environmental / Natural Resource Agencies")
        key_agency_name.append(r['Primary Agency Name'])
    else:
        env_flag.append("All Other Agencies")
        key_agency_name.append("")

    start_year.append(int(r['Challenge Start Date'].split("-")[0]))
    
    if r["Prize Amount"] == "No monetary prize for this challenge":
        prize_num.append(0)
    else:
        prize_num.append(float(r["Prize Amount"].replace("$","").replace(",","").split(".")[0]))

df_add["department"] = department
df_add["sub_agency"] = sub_agency
df_add["env_flag"] = env_flag
df_add["start_year"] = start_year
df_add["count"] = 1
df_add["key_agency_name"] = key_agency_name
df_add["prize_num"] = prize_num
df_add["duration"] = pd.to_datetime(df_add['Challenge End Date']) - pd.to_datetime(df_add['Challenge Start Date'])
df_add["duration_days"] = df_add["duration"].values.astype('timedelta64[D]') / np.timedelta64(1, 'D')

df_add.to_csv(folder+"challenges_Oct302023.csv")

#Enviro agencies vs. non-enviro over time
challenge_totals = df_add.groupby(["env_flag","start_year"]).count().reset_index()
sns.lineplot(data=challenge_totals,x="start_year",y="count", hue="env_flag")

#Just enviro agencies
df_add_filter = df_add.where(df_add["env_flag"]=="Environmental / Natural Resource Agencies").dropna(how="all")
challenge_totals_2 = df_add_filter.groupby(["start_year","key_agency_name"]).count().reset_index().sort_values("key_agency_name")
sns.lineplot(data=challenge_totals_2,x="start_year",y="count", hue="key_agency_name")

#Enviro agencies vs. non-enviro over time
challenge_totals_3 = df_add.groupby(['Primary Challenge Type','env_flag',"start_year"]).count().reset_index()
sns.catplot(kind="point",data=challenge_totals_3,x="start_year",y="count", hue='env_flag', col_wrap=3, col='Primary Challenge Type')

challenge_totals_4 = df_add.groupby(['env_flag',"start_year"]).agg({"prize_num":'mean'}).reset_index()
sns.catplot(kind="point",data=challenge_totals_4,x="start_year",y="prize_num", hue='env_flag').set(title="Average Prize Amount").set_xticklabels(rotation=30)

challenge_totals_5 = df_add.groupby(['env_flag',"start_year"]).agg({"duration_days":'mean',"prize_num":'mean'}).reset_index()
sns.catplot(kind="point",data=challenge_totals_5,x="start_year",y="duration_days", hue='env_flag').set(title="Average Prize Duration").set_xticklabels(rotation=30)

challenge_totals_6 = df_add.groupby(['env_flag','Primary Agency Name']).agg({"count":'sum'}).reset_index().sort_values(["count"])
sns.barplot(data=challenge_totals_6,x='Primary Agency Name',y="count", hue='env_flag').set(title="Average Prize Duration").set_xticklabels(rotation=30)

sns.jointplot(data=df_add,x="prize_num",y="duration_days", hue='env_flag')

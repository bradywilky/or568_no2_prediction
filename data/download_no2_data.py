# poythjon#!/usr/bin/env python
# coding: utf-8

# In[1]:


import sys
import subprocess


# In[2]:


# arguments should be provided in the following order:
#    date: YYYYMMDD
#    dsource: omi, tropomi, tropomi_hires
#    location: la, tpe, dl

args = sys.argv

date = args[1]
dsource = args[2]
location = args[3]


# In[3]:


bat_path = 'C:/Users/15714/Documents/repositories/public_sonbox/or568_term_project/data/download_no2_data.bat'
s3 = 's3://drivendata-competition-airathon-public-us/no2/train/ train/'
# bat_code = f'aws s3 cp {s3} --no-sign-request --recursive --exclude="*" --include="*{date}*{dsource}_{location}*"'
bat_code = 'aws s3 cp s3://drivendata-competition-airathon-public-us/no2/train/omi/ train/ --no-sign-request --recursive'
file = open(bat_path, 'w')
file.write(bat_code)
file.close()


# In[4]:


subprocess.call([bat_path])


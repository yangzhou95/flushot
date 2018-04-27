import os
import csv
import bz2
from csv import DictReader

with open('list_of_users.csv', 'rb') as f:
    user_screen_name = [row["User Screen Name"] for row in DictReader(f)]

print(user_screen_name)

count = 0
with open('compressed.json', 'wb') as file:
    for root, directories, filenames in os.walk('data4rachel_new/compressed'):
        for directory in directories:
            if(directory in user_screen_name):
                path =  os.listdir(root + "/" + directory + "/")
                count = count +1
                for filename in path:
                    full_path = root + "/" + directory + "/" + filename
                    print(full_path)
                    zipfile = bz2.BZ2File(full_path)  # open the file
                    data = zipfile.read()  # get the decompressed data
                    file.write(data)  # write a uncompressed file


print("total count: ", count)
# zipfile = bz2.BZ2File(filepath) # open the file
# data = zipfile.read() # get the decompressed data
# newfilepath = filepath[:-4] # assuming the filepath ends with .bz2
# open(newfilepath, 'wb').write(data) # write a uncompressed file

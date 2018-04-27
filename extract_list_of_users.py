import os
import csv
from csv import DictReader

result = []

with open('user_screen_name.csv', 'rb') as f:
    # reader = csv.reader(f)
    # scren_name_list = list(reader)
    user_screen_name = [row["User Screen Name"] for row in DictReader(f)]


with open('csvfile.csv', 'wb') as file:
    for root, directories, filenames in os.walk('data4rachel_new'):
        for directory in directories:
            if(directory in user_screen_name):
                result.append(directory)
                file.write(directory)
                file.write('\n')

                print(directory)


print(result)

import json
import csv
import codecs
import sys
import importlib
import re

input_data_filename= 'compressed.json'

if sys.version[0] == '3':
    importlib.reload(sys)
if sys.version[0] == '2':
    sys.setdefaultencoding("utf-8")

url = re.compile(r'http\S+')
plainText = re.compile('[^A-Za-z0-9]+')


if __name__ == "__main__":
    with open(input_data_filename, 'rb') as fp:
        with codecs.open('CommonTweetUser.csv', 'w', encoding='utf-8') as outF:
            fieldnames = ['User Name', 'User Screen Name', 'Text', 'Description', 'Created At', 'State']
            writer = csv.DictWriter(outF, lineterminator='\n', fieldnames=fieldnames)
            # writer = csv.DictWriter(outF, delimiter='\001', lineterminator='\n', fieldnames=fieldnames) # to set a delimiter
            writer.writeheader()

            for l in fp:
                raw_data = json.loads(l)
                print(raw_data)

                # id = str(raw_data.get('id', 'No ID'))
                text = raw_data.get('text', '')
                text = re.sub(url, '', text)
                text = re.sub(plainText, ' ', text).strip()

                user_data = raw_data['user']
                userName = user_data.get('name', '')
                userName = re.sub(url, '', userName)
                userName = re.sub(plainText, ' ', userName).strip()

                screen_name = user_data.get('screen_name', '')
                screen_name = re.sub(url, '', screen_name)
                screen_name = re.sub(plainText, ' ', screen_name).strip()

                created_at = user_data.get('created_at', '')

                description = user_data.get('description', '')
                # description = re.sub(url, '', description)
                # description = re.sub(plainText, ' ', description).strip()

                state = user_data.get('location', '')

                writer.writerow({
                                'User Name': userName,
                                'User Screen Name': screen_name,
                                'Text': text,
                                'Description': description,
                                'Created At': created_at,
                                'State': state
                                })

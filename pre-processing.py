import numpy as np
import string
import re
import csv

def pre_proccessing(fname, word):
    # open file with fname list
    data = []
    temp = []
    regx = re.compile('[%s]'%re.escape(string.punctuation))
    # loading each file
    for f in fname:
        file = open(f, 'r')
        # loading each line for oen file 
        for i in file:
            # store all words in temp in 1D list
            temp = temp + regx.sub('', i.replace('\n', '').lower()).split(' ')
        # store all words in data in mult-dimssion splits by files
        data.append([x for x in temp if x!= ''])
        file.close()        
    # find the key of files
    key = list(set(temp))
    for x in word:
        key.remove(x)
    # create a python dictionory and implement key and values
    freq = []
    for j in data:
        tmp = []
        for x in key:
            tmp = tmp + [j.count(x)]
        freq.append(tmp)
    #save freq, list word and file name as csv files
    with open('../../table.csv', 'w', newline='') as csvfile:
        writerhead = csv.DictWriter(csvfile, fieldnames = key, delimiter=',', quotechar='', quoting=csv.QUOTE_NONE)
        writerhead.writeheader()
        writerfreq = csv.writer(csvfile, delimiter=',', quotechar='', quoting=csv.QUOTE_NONE)
        for row in freq:
            writerfreq.writerow(row)
    csvfile.close()

if __name__ == '__main__':
	from sys import argv

	filename = argv[1:]
	word = []
    file = open('common_word.txt', 'r')
    for i in file:
        word = word + i.replace(',',' ').split(' ')
	table = pre_proccessing(filename, word)
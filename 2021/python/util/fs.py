
import sys
import regex

def read_nums(line, pattern='\d+'):
    return list(map(int, regex.findall(pattern, line)))

def readall(filepath):
    ret = ''
    with open(filepath) as f:
        ret = f.read()
    return ret
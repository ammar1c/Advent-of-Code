
import math, regex
from functools import reduce

from util import fs
def choose_optimal(xs):
    mi, mx = min(xs), max(xs)
    min_cost = math.inf
    for opt in range(mi, mx+1):
        cost = 0
        for x in xs:
            cost += abs(x-opt)
        min_cost = min(cost, min_cost)
    print(min_cost)

def choose_optimal_x1(xs):
    xs.sort()
    mid = xs[len(xs)//2]
    score = reduce(lambda x,y: x + abs(mid-y), xs, 0)
    print(score, mid)



def choose_optimalp2(xs):
    mi, mx = min(xs), max(xs)
    min_cost = math.inf
    for opt in range(mi, mx+1):
        cost = 0
        for x in xs:
            n = abs(x-opt)
            cost += (n+1) * n // 2
        min_cost = min(cost, min_cost)
    print(min_cost)

def read_nums(line, sep=' '):
    return list(map(int, regex.findall('\d+', line)))

def read_all(path):
    with open(path) as f:
        return f.read()
    return ''

if __name__ == '__main__':
    nums = read_nums(fs.readall('./data/day7-input.txt'))
    choose_optimal_x1(nums)
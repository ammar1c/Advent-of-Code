

import regex
from collections import Counter, defaultdict
def read_nums(line, sep=' '):
    return list(map(int, regex.findall('\d+', line)))

def read_all(path):
    with open(path) as f:
        return f.read()
    return ''

def solve_it(nums, days):
    cnt = Counter(nums)
    for i in range(days):
        new_cnt = defaultdict(int)
        for i in range(0, 9):
            if i == 0:
                v = cnt[0]
                new_cnt[8] += v
                new_cnt[6] += v
            else:
                new_cnt[i-1] += cnt[i]
        cnt = new_cnt
    print(sum(cnt.values()))

def main():

    nums = read_nums(read_all("./data/day6-input.txt"), sep=',')
    for i in range(256):
        new_nums = []
        for num in nums:
            if num == 0:
                new_nums.append(8)
                new_nums.append(6)
            else:
                new_nums.append(num-1)
        nums = new_nums
        if i % 10 == 0: print(i)
    print(len(nums))
if __name__ == '__main__':
    nums = read_nums(read_all("./data/day6-sample.txt"), sep=',')
    solve_it(nums, days=500)
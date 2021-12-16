
from util import fs


def solve1(nums):
    n, res = len(nums), 0
    for i in range(1, n):
        if nums[i] > nums[i-1]: res += 1
    return res

def solve1_zip(nums):
    res = 0
    for a, b in zip(nums, nums[1:]):
        if b > a: res += 1
    return res

def solve2(nums):
    res, n = 0, len(nums)
    for i in range(3, n):
        if nums[i] > nums[i-3]:
            res += 1
    return res

def main(filepath):
    lines = fs.readall(filepath).split("\n")
    nums = [int(line.strip()) for line in lines]
    result = solve1(nums)
    print("p1 =", result)
    result = solve2(nums)
    print("p2 =", result)


if __name__ == '__main__':

    main('./data/day1-input.txt')
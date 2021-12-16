
from util import fs


def solve1(lines):
    n = len(lines[0])
    epsilon = 0
    gamma = 0
    for i in range(n):
        v = 0
        for line in lines:
            if line[i] == '1': v += 1
            else: v -= 1
        bit = 0 if v < 0 else 1
        epsilon = epsilon << 1 | bit
        gamma   = gamma << 1 | (1-bit)
    return epsilon * gamma

def solve2(lines):
    def find(nums, most_common):
        n = len(nums[0])
        for i in range(n):
            v = 0
            for num in nums:
                if num[i] == '1': v += 1
                else: v -= 1
            b = '0' if v < 0 else '1'

            new_nums = []
            for num in nums:
                if most_common:
                    if num[i] == b: new_nums.append(num)
                else:
                    if num[i] != b: new_nums.append(num)
            nums = new_nums
            if len(nums) == 1: break
        return nums

    ox = int(find(lines, True)[0], 2)
    co2 = int(find(lines, False)[0], 2)

    return ox*co2


def main(filepath):
    lines = fs.readall(filepath).split("\n")
    p1 = solve1(lines)
    print("p1 = ", p1)
    p2 = solve2(lines)
    print("p2 = ", p2)

if __name__ == '__main__':
    main('./data/day3-input.txt')




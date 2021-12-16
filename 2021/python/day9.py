import copy

import regex
from functools import reduce
from math import prod

def part2(path):
    all = ''
    with open(path) as f:
        all = f.read()
    lines = all.split("\n")
    grid = [[*map(int, regex.findall("\d", line))] for line in lines]
    m, n = len(grid), len(grid[0])
    def find(x, y):
        if x < 0 or x >= m or y < 0 or y >= n or grid[x][y] == -1 or grid[x][y] == 9: return 0
        grid[x][y] = -1
        res = 1 + find(x+1, y) + find(x-1, y) + find(x, y-1) + find(x, y+1)
        return res

    indx = getLoLocationsIndices(grid)
    sizes = []
    for i, j in indx:
        sz = find(i,j)
        sizes.append(sz)

    sizes.sort(reverse=True)
    print(prod(sizes[:3]))


def getLoLocationsIndices(grid):
    m, n = len(grid), len(grid[0])
    result = []
    for i in range(m):
        for j in range(n):
            up     = grid[ i -1][j] if i > 0 else 10
            down   = grid[ i +1][j] if i < m- 1 else 10
            left = grid[i][j - 1] if j > 0 else 10
            right = grid[i][j + 1] if j < n - 1 else 10
            neigbours = [up, down, left, right]
            if all(map(lambda x: grid[i][j] < x, neigbours)):
                result.append((i, j))
    return result

def getLoLocations(grid):
    m, n = len(grid), len(grid[0])
    result = []
    for i in range(m):
        for j in range(n):
            up     = grid[ i -1][j] if i > 0 else 10
            down   = grid[ i +1][j] if i < m- 1 else 10
            left = grid[i][j - 1] if j > 0 else 10
            right = grid[i][j + 1] if j < n - 1 else 10
            neigbours = [up, down, left, right]
            if all(map(lambda x: grid[i][j] < x, neigbours)):
                result.append(grid[i][j])
    return result

def risk_level(low_heights):
    return reduce(lambda x, y: x + y+1, low_heights, 0)

def part1(path):
    all = ''
    with open(path) as f:
        all = f.read()
    lines = all.split("\n")
    grid = [[*map(int, regex.findall("\d", line))] for line in lines]
    locastions = getLoLocations(grid)
    print(risk_level(locastions))

if __name__ == '__main__':
    part2("./data/day9-input.txt")

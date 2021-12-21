from functools import lru_cache

import sys
import math
from heapq import heappush, heappop




def solve2d(grid, p):

    m, n = len(grid) * p, len(grid[0]) * p
    new_grid = [[0 for _ in range(n)] for _ in range(m)]

    def get_val(grid, i, j):
        if new_grid[i][j] != 0: return new_grid[i][j]
        m, n = len(grid), len(grid[0])
        x, y = i % m, j % n
        v = grid[x][y]
        while x != i:
            v , x = v+1, x + m
            if v == 10: v = 1
        while y != j:
            v, y = v+1, y + n
            if v == 10: v = 1
        new_grid[i][j] = v
        return v

    pq = [(0, (0, 0))]
    s = set()
    dist = [[math.inf for _ in range(n)] for _ in range(m)]
    dist[0][0] = 0
    while pq:
        (distxy, (x, y)) = heappop(pq)
        if (x,y) in s: continue
        s.add((x, y))
        for i, j in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            nx = x + i
            ny = y + j
            if (nx, ny) in s or nx < 0 or nx >= m or ny < 0 or ny >= n: continue
            dist[nx][ny] = min(distxy + get_val(grid, nx, ny), dist[nx][ny])
            if 0 <= nx < m and 0 <= ny < n:
                heappush(pq, (dist[nx][ny], (nx, ny)))

    return dist[m - 1][n - 1]





def print_grid(grid):
    m, n = len(grid), len(grid[0])
    for i in range(m):
        for j in range(n):
            print(grid[i][j], end="")
        print()




if __name__ == '__main__':
    with open("./data/day15-input.txt") as f:
        all = f.read()
        lines = all.split()
        grid = [[int(x) for x in line] for line in lines]
        print(solve2d(grid, 5))

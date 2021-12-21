
from heapq import heappush, heappop


def solve2d(grid, p):
    gm, gn = len(grid), len(grid[0])
    m, n = len(grid) * p, len(grid[0]) * p
    new_grid = [[0 for _ in range(n)] for _ in range(m)]

    for i in range(m):
        for j in range(n):
            dx, dy = i // gm, j // gn
            v = grid[i % gm][j % gn]
            v = (v + dx - 1) % 9 + 1
            v = (v + dy - 1) % 9 + 1
            new_grid[i][j] = v

    pq = [(0, (0, 0))]
    s = set()
    dist = [[999999999 for _ in range(n)] for _ in range(m)]
    dist[0][0] = 0
    while pq:
        (distxy, (x, y)) = heappop(pq)
        if (x, y) in s: continue
        s.add((x, y))
        for i, j in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            nx = x + i
            ny = y + j
            if (nx, ny) in s or nx < 0 or nx >= m or ny < 0 or ny >= n: continue
            dist[nx][ny] = min(distxy + new_grid[nx][ny], dist[nx][ny])
            if 0 <= nx < m and 0 <= ny < n:
                heappush(pq, (dist[nx][ny], (nx, ny))) # there's no decrease key, so we have to add unvisited vertex multiple times

    return dist[m - 1][n - 1]



if __name__ == '__main__':
    with open("./data/day15-input.txt") as f:
        all = f.read()
        lines = all.split()
        grid = [[int(x) for x in line] for line in lines]
        print(solve2d(grid, 5))

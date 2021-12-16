

def simulate_part2(grid, steps):
    m,n = len(grid), len(grid[0])
    def print_grid():
        print("\n".join(["".join(map(str, row)) for row in grid]))
    total = 0

    for step in range(1, steps+1):
        q = []
        visited = set()
        for r in range(m):
            for c in range(n):
                v = grid[r][c]
                if v == 9:
                    grid[r][c] = 0
                    q.append((r,c))
                else: grid[r][c] += 1
        # print(q)
        while q:
            r,c = q.pop(0)
            grid[r][c] = 0
            visited.add((r,c))
            for dx in [-1,0,1]:
                for dy in [-1,0,1]:
                    if dx == 0 and dy == 0: continue
                    nx, ny = r+dx, c+dy
                    if nx < 0 or nx >= m or ny < 0 or ny >= n: continue
                    neig = grid[nx][ny]
                    if neig == 0: continue
                    grid[nx][ny] = (grid[nx][ny] + 1) % 10
                    # print((r,c), "is increasing", (nx,ny))
                    if grid[nx][ny] == 0 and (nx,ny) not in visited:
                        q.append((nx, ny))
                        visited.add((nx, ny))
            # print((r,c), "is visited", q)
        total = sum([x == 0 for row in grid for x in row])
        if total == m * n:
            break
        # print(total)
        # print("-"*10)

    print(step)


def simulate(grid, steps):
    m,n = len(grid), len(grid[0])
    def print_grid():
        print("\n".join(["".join(map(str, row)) for row in grid]))
    total = 0

    for _ in range(steps):
        q = []
        visited = set()
        for r in range(m):
            for c in range(n):
                v = grid[r][c]
                if v == 9:
                    grid[r][c] = 0
                    q.append((r,c))
                else: grid[r][c] += 1
        # print(q)
        while q:
            r,c = q.pop(0)
            grid[r][c] = 0
            visited.add((r,c))
            for dx in [-1,0,1]:
                for dy in [-1,0,1]:
                    if dx == 0 and dy == 0: continue
                    nx, ny = r+dx, c+dy
                    if nx < 0 or nx >= m or ny < 0 or ny >= n: continue
                    neig = grid[nx][ny]
                    if neig == 0: continue
                    grid[nx][ny] = (grid[nx][ny] + 1) % 10
                    # print((r,c), "is increasing", (nx,ny))
                    if grid[nx][ny] == 0 and (nx,ny) not in visited:
                        q.append((nx, ny))
                        visited.add((nx, ny))
            # print((r,c), "is visited", q)
        total += sum([x == 0 for row in grid for x in row])
        # print(total)
        # print("-"*10)

    print(total)

if __name__ == '__main__':
    with open("./data/day11-input.txt") as f:
        all = f.read()
    lines = all.split("\n")
    grid = [[int(x) for x in line] for line in lines]
    # print(grid)
    simulate_part2(grid, 1000)
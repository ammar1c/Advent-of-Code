import copy

import regex
from collections import defaultdict


def readint(line):
    return int(regex.findall("\d+", line)[0])

def fold(grid, ftype, fline):
    cp = defaultdict(lambda: defaultdict(int))
    for x, row in grid.items():
        for y, _ in row.items():
            nx, ny = x, y
            if ftype == 'y' and y >= fline:
                ny = 2*fline - y
            elif ftype == 'x' and x >= fline:
                nx = 2*fline - x
            cp[nx][ny] = grid[x][y]
    return cp

def process(coord, folds):
    grid = defaultdict(lambda: defaultdict(int))
    for x, y in coord:
        grid[x][y] = 1
    for itype, iline in folds:
        grid = fold(grid, itype, iline)
    print_grid(grid)


def main_p1(path):

    with open(path) as f:
        all = f.read()
        lines = all.split("\n")
        coord = []
        i = 0
        while i < len(lines):
            if lines[i] == '' or 'fold' in lines[i]:
                i += 1
                break
            # print(lines[i])
            x, y = lines[i].split(',')
            x, y = int(x), int(y)
            coord.append((x, y))
            i += 1
        # read fold instructions
        fold  = []
        while i < len(lines):
            # print(lines[i])
            if "x=" in lines[i]:
                fold.append(("x", readint(lines[i])))
            else:
                fold.append(("y", readint(lines[i])))
            i += 1
        print(coord, fold)
        process(coord, fold)

def print_grid(grid):
    max_x, max_y = 0, 0
    for x, rows in grid.items():
        for y, _ in rows.items():
            max_x = max(x, max_x)
            max_y = max(y, max_y)
    for y in range(0, max_y+1):
        for x in range(0, max_x+1):
            print(u'\u2588' if grid[x][y] == 1 else ' ', end='')
        print()


# GJZGLUPJ
if __name__ == '__main__':
    main_p1("./data/day13-input.txt")
import regex
from collections import defaultdict

def main():

    all = ''
    with open("../data/day5/input.txt") as f:
        all = f.read()

    lines = all.split('\n')
    coords = []
    for line in lines:
        coords.append(list(map(int, regex.findall("\d+", line))))
    inter = set()
    board = defaultdict(lambda: defaultdict(int))

    for x1,y1,x2,y2 in coords:
        if x1 == x2:
            for i1 in range(min(y1, y2), max(y1, y2)+1):
                board[i1][x1] += 1
                if board[i1][x1] >= 2: inter.add((i1, x1))
        if y1 == y2:
            for i1 in range(min(x1, x2), max(x1, x2)+1):
                board[y1][i1] += 1
                if board[y1][i1] >= 2: inter.add((y1, i1))

    print(len(inter))

def print_board(b):
    for i in range(11):
        for j in range(11):
            if b[i][j] > 0:
                print(b[i][j], end=' ')
            else:
                print('.', end=' ')
        print()
    print()

def main2():

    all = ''
    with open("./data/day5-input.txt") as f:
        all = f.read()

    lines = all.split('\n')
    coords = []
    for line in lines:
        coords.append(list(map(int, regex.findall("\d+", line))))
    inter = set()
    board = defaultdict(lambda: defaultdict(int))

    for x1,y1,x2,y2 in coords:
        if x1 != x2 and y1 != y2:
            if abs(x2 - x1) != abs(y2 - y1): continue


        sx, sy = x1, y1
        dx, dy = 0, 0
        if x1 != x2:
            dx = -1 if x1 > x2 else 1
        if y1 != y2:
            dy = -1 if y1 > y2 else 1
        ex, ey = x1, y1
        while ex != x2 or ey != y2:
            board[ex][ey] += 1
            if board[ex][ey] >= 2: inter.add((ex, ey))
            ex += dx
            ey += dy
        if ex == x2 and ey == y2:
            board[ex][ey] += 1
            if board[ex][ey] >= 2: inter.add((ex, ey))
            # if abs(x1-x2) > 10: break

        # print_board(board)

    # print(inter)
    print(len(inter))

if __name__ == '__main__':
    main2()
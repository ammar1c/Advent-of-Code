import sys
from dataclasses import dataclass
from util import fs

@dataclass
class Cell:
    v: int
    m: bool

class Board:
    def __init__(self, id):
        self.id = id
        self.grid = [[Cell(j, False) for j in range(5)] for i in range(5)]

    @staticmethod
    def read_board(f, id):
        b = Board(id)
        for i in range(5):
            line = f.readline().strip()
            nums = fs.read_nums(line)
            for j in range(5):
                b.grid[i][j].v = nums[j]
        return b

    def mark(self, num):
        for i in range(5):
            for j in range(5):
                if self.grid[i][j].v == num:
                    self.grid[i][j] = Cell(self.grid[i][j].v, True)

    def check(self):
        for i in range(5):
            if all([c.m for c in self.grid[i]]): return [i, None]
        for j in range(5):
            if all([self.grid[i][j].m for i in range(5)]): return [None, j]
        return [None, None]

    def sum(self):
        r = 0
        for i in range(5):
            for j in range(5):
                if not self.grid[i][j].m:
                    r += self.grid[i][j].v
        return r

    def print(self):
        r = 0
        for i in range(5):
            for j in range(5):
                print((self.grid[i][j].v, int(self.grid[i][j].m)), end=' ')
            print()
        return r

def read_input(path):
    with open(path) as f:
        line = f.readline()
        nums = fs.read_nums(line)
        line = f.readline()
        print(nums)
        boards = []
        id = 0
        while line != '':
            boards.append(Board.read_board(f, id))
            id += 1
            line = f.readline()

        # for board in boards:
        #     board.print()
        #     print()
        for num in nums:
            for board in boards:
                board.mark(num)
                r, c = board.check()
                if r != None or c != None:
                    print(board.id, board.sum(),num, board.sum()*num)
                    return

def read_part2(path):
    with open(path) as f:
        line = f.readline()
        nums = fs.read_nums(line)
        line = f.readline()
        print(nums)
        boards = []
        id = 0
        while line != '':
            boards.append(Board.read_board(f, id))
            id += 1
            line = f.readline()

        # for board in boards:
        #     board.print()
        #     print()
        last = None
        won = set()
        for num in nums:
            for board in boards:
                board.mark(num)
                r, c = board.check()

                if r != None or c != None:
                    last = (board.id, board.sum(),num, board.sum()*num)
                    won.add(board.id)
                    if len(won) == len(boards):
                        break
            if len(won) == len(boards):
                break

        print(last)


if __name__ == '__main__':
    read_part2("./data/day4-input.txt")

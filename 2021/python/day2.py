
from util import fs

def solve1(lines):
    x, y = 0,0
    for line in lines:
        cm, z = line.strip().split(" ")
        cm = cm.lower()
        z = int(z)
        if cm.startswith("f"):
            x += z
        elif cm.startswith("d"):
            y += z
        else:
            y -= z
    return x*y


def solve2(lines):
    x, y = 0, 0
    aim = 0
    for line in lines:
        cm, z = line.strip().split(" ")
        cm = cm.lower()
        z = int(z)
        if cm.startswith("f"):
            x += z
            y += aim * z
        elif cm.startswith("d"):
            aim += z
        else:
            aim -= z
    return x*y

def main(filepath):
    lines = fs.readall(filepath).split("\n")
    p1 = solve1(lines)
    print("p1 = ", p1)
    p2 = solve2(lines)
    print("p2 = ", p2)



if __name__ == '__main__':
    main('input.txt')


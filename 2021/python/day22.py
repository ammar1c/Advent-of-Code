

import regex
from util import fs
from collections import Counter, defaultdict, namedtuple, deque
def read_nums(line, sep=' '):
    return list(map(int, regex.findall('[-]?\d+', line)))

Cube = namedtuple('Cube', ['toggle', 'x1', 'x2', 'y1', 'y2', 'z1', 'z2'])



def calc_overlap(cube1, cube2):
    vertices = [[x,y,z] for x in [cube1.x1, cube1.x2] for y in [cube1.y1, cube1.y2] for z in [cube1.z1, cube1.z2]]
    for vertex in vertices:
        if cube2.x1 <= vertex[0] <= cube2.x2 and \
           cube2.y1 <= vertex[1] <= cube2.y2 and \
           cube2.z1 <= vertex[2] <= cube2.z2:

            x1 = max(cube1.x1, cube2.x1)
            x2 = min(cube1.x2, cube2.x2)

            y1 = max(cube1.y1, cube2.y1)
            y2 = min(cube1.y2, cube2.y2)

            z1 = max(cube1.z1, cube2.z1)
            z2 = min(cube1.z2, cube2.z2)

            if x1 == x2 and y1 == y2 and z1 == z2: return None, None

            cube_x1 = Cube(cube1.toggle, *[cube1.x1, x1-1, cube1.y1, cube1.y2, cube1.z1, cube1.z2])
            cube_x2 = Cube(cube1.toggle, *[x2+1, cube1.x2, cube1.y1, cube1.y2, cube1.z1, cube1.z2])

            cube_y1 = Cube(cube1.toggle, *[cube1.x1, cube1.x2, cube1.y1, y1-1, cube1.z1, cube1.z2])
            cube_y2 = Cube(cube1.toggle, *[cube1.x1, cube1.x2, y2+1, cube1.y2, cube1.z1, cube1.z2])

            cube_z1 = Cube(cube1.toggle, *[cube1.x1, cube1.x2, cube1.y1, cube1.y2, cube1.z1, z1-1])
            cube_z2 = Cube(cube1.toggle, *[cube1.x1, cube1.x2, cube1.y1, cube1.y2, z2+1, cube1.z2])

            return (Cube(cube1.toggle and cube2.toggle, *[x1,x2,y1,y2,z1,z2]),
                    [cube_x1, cube_x2, cube_y1, cube_y2, cube_z1, cube_z2])
    return None, None

# def split_if_overlaps(cube1, cube2):
#     vertices = [[x,y,z] for x in [cube1.x1, cube1.x2] for y in [cube1.y1, cube1.y2] for z in [cube1.z1, cube1.z2]]
#     for vertex in vertices:
#         if cube2.x1 <= vertex[0] <= cube2.x2 and \
#            cube2.y1 <= vertex[1] <= cube2.y2 and \
#            cube2.z1 <= vertex[2] <= cube2.z2:
#
#             x1 = max(cube1.x1, cube2.x1)
#             x2 = min(cube1.x2, cube2.x2)
#
#             y1 = max(cube1.y1, cube2.y1)
#             y2 = min(cube1.y2, cube2.y2)
#
#             z1 = max(cube1.z1, cube2.z1)
#             z2 = min(cube1.z2, cube2.z2)
#
#             c1 = Cube(cube1.toggle, )
#
#             return Cube(cube1.toggle and cube2.toggle, *[x1,x2,y1,y2,z1,z2])
#     return None

def count_cuboids(cube):
    return (cube.x2 - cube.x1 + 1) * (cube.y2-cube.y1+1) * (cube.z2-cube.z1 + 1)


# def remove_overlaps(on_cubes):
#
#     found = True
#     overlaps = []
#     while found:
#         n = len(on_cubes)
#         for i in range(n):
#             left = on_cubes.popleft()
#             for j in range(j, )
#


def part2_nosplit(cubes):
    print(cubes)
    on_cubes = deque()
    res = 0

    for i, cube in enumerate(cubes):
        m = len(on_cubes)
        for j in range(m):
            on_cube = on_cubes.popleft()
            overlap, _ = calc_overlap(on_cube, cube)

            if not overlap:
                on_cubes.append(on_cube)
                continue




        if cube.toggle:
            on_cubes.append(cube)
            res += count_cuboids(cube)

    print(res)

def part2x(cubes):
    print(cubes)
    on_cubes = deque()
    res = 0

    for i, cube in enumerate(cubes):
        m = len(on_cubes)
        for j in range(m):
            on_cube = on_cubes.popleft()
            overlap, splits = calc_overlap(on_cube, cube)
            overlap1, splits1 = calc_overlap(cube, on_cube)

            print("overlap", overlap)
            print("overlap1", overlap1)
            if not overlap:
                on_cubes.append(on_cube)
                continue

            res -= count_cuboids(overlap)
            if cube.toggle:
                on_cubes.append(on_cube)
            else:
                for split in splits:
                    if split.x1 >= split.x2 or split.y1 >= split.y2 or split.z1 >= split.z2: continue
                    on_cubes.append(split)



        if cube.toggle:
            on_cubes.append(cube)
            res += count_cuboids(cube)

    print(res)



def part2(cubes):
    state = set()
    # print(ins_full)
    print(cubes)
    n = len(cubes)
    on_cubes = deque()
    res = 0
    overlaps_on = []
    on_cubes = deque()
    print("ok")
    for i, cube in enumerate(cubes):
        if cube.toggle: on_cubes.append(cube)
        else:
            n = len(on_cubes)
            for _ in range(n):
                on_cube = on_cubes.popleft()
                overlap, splits = calc_overlap(on_cube, cube)

                if not overlap:
                    on_cubes.append(on_cube)
                    continue
                print(overlap)
                print(on_cube)
                print(splits)
                print("non split", count_cuboids(on_cube), "overlap", count_cuboids(overlap))
                s = []
                for split in splits:
                    if split.x1 > split.x2 or split.y1 > split.y2 or split.z1 > split.z2: continue
                    print("split", split)
                    on_cubes.append(split)
                    s.append(split)
                print("split", sum([count_cuboids(x) for x in s]))

    final_list = []
    res = 0
    for j, cube in enumerate(on_cubes):
        m = len(final_list)
        for i in range(m):
            overlap, splits = calc_overlap(final_list[i], cube)
            if overlap:
                res -= count_cuboids(overlap)
        final_list.append(cube)
        res += count_cuboids(cube)


    print(on_cubes)
    print(final_list)
    print(res)
    print(sum([count_cuboids(on_cube) for on_cube in on_cubes]))






def apply(ins_full):
    state = set()
    # print(ins_full)
    for toggle, ins in ins_full:
        i = 0
        # while i < len(ins):
        #     if ins[i] < -50: ins[i] = -51
        #     if ins[i] > 50: ins[i] = 51
        #     if ins[i+1] < -50: ins[i+1] = -51
        #     if ins[i+1] > 50: ins[i+1] = 51
        #     i += 2
        print(ins)
        x = ins[0]

        while x <= ins[1]:
            if x < -50: x = -50;continue
            elif x > 50: break
            y = ins[2]
            while y <= ins[3]:
                if y < -50: y = -50;continue
                elif y > 50: break
                z = ins[4]
                while z <= ins[5]:
                    if z < -50: z = -50;continue
                    elif z > 50: break
                    if toggle: state.add((x, y, z))
                    elif (x,y,z) in state:
                        state.remove((x,y,z))
                    z += 1
                y += 1
            x += 1

    print(len(state))

def read_input():
    instructions = []
    with open("./data/day22-sample1.txt") as f:
        lines = f.read().split("\n")
        for line in lines:
            instructions.append(Cube(line.startswith("on"), *read_nums(line)))
    return instructions




if __name__ == '__main__':
    ins = read_input()
    # print(ins)
    part2(ins)
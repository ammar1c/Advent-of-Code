


def simulate(vx, vy, leftlimitx, rightlimit, toplimit, bottomlimit):
    ix, iy = 0, 0
    max_h = iy
    steps = 0
    found = False
    while iy >= bottomlimit and ix <= rightlimit:
        ix += vx
        iy += vy
        max_h = max(iy, max_h)
        if leftlimitx <= ix <= rightlimit and bottomlimit <= iy <= toplimit:
            found = True
            break
        steps += 1
        vy = vy - 1
        vx = max(vx-1, 0)
    return (found, max_h)


def part1(leftlimit, rightlimit, bottom, top):
    result = 0
    for vx in range(1, leftlimit):
        for vy in range(1, abs(bottom)):
            found, h = simulate(vx, vy, leftlimit, rightlimit, top, bottom)
            # print("max height for", vx, vy, "is", h)
            if found:
                result = max(h, result)
    return result

def part2(leftlimit, rightlimit, bottom, top):
    cnt = 0
    for vx in range(0, rightlimit+1):
        for vy in range(bottom-1, abs(bottom)+1):
            found, h = simulate(vx, vy, leftlimit, rightlimit, top, bottom)
            if found:
                cnt += 1
    return cnt

if __name__ == '__main__':


    print("part1 result", part1(244, 303, -91, -54))
    print("part2 result", part2(244, 303, -91, -54))

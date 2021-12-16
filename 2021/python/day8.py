

import regex

mappings = { 2:1, 4:4, 3:7, 7:8}

def find_strings(s):
    return regex.findall('\w+', s)

def parse_line(line: str):
    a, b = line.split("|")
    a, b  = find_strings(a), find_strings(b)
    return (a, b)


def decode(segments, output):


    def pop(cond):
        for i, seg in enumerate(segments):
            seg = set(seg)
            if cond(seg):
                segments.pop(i)
                return seg

    def xlen(i): return lambda y: len(y) == i

    one = pop(xlen(2))
    four = pop(xlen(4))
    seven = pop(xlen(3))
    eight = pop(xlen(7))
    three = pop(lambda x: len(x) == 5 and len(x & one) == 2)
    five  = pop(lambda x: len(x) == 5 and len(x & four) == 3)
    two   = pop(lambda x: len(x) == 5 and x != three and x != five)
    nine  = pop(lambda x: len(x) == 6 and len(x & four) == len(four))
    zero  = pop(lambda x: len(x) == 6 and len(x & one) == 2)
    six   = pop(lambda x: len(x) == 6 and len(x & one) != 2)

    mappings = [zero, one, two, three, four, five, six, seven, eight, nine]

    result = ''
    for z in output:
        for i, m in enumerate(mappings):
            if set(z) == m:
                result += str(i)
                break
    result = int(result)
    return result



def part1():
    with open("./data/day8-input.txt") as f :
        lines = f.read()
        result = 0
        for line in lines.split("\n"):
            r = parse_line(line)
            input, output = r[0], r[1]
            for segment in output:
                if len(segment) in mappings:
                    result += 1

        print(result)


def part2():
    with open("./data/day8-sample.txt") as f :
        lines = f.read()
        result = 0
        for line in lines.split("\n"):
            r = parse_line(line)
            input, output = r[0], r[1]
            result += decode(input, output)

        print(result)


if __name__ == '__main__':
    part2()
    # decode("fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef".split(), "cg cg fdcagb cbg".split())

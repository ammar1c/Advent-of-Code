

points = {
  ')': 3,
  ']': 57,
  '}': 1197,
  '>': 25137
}

closing_points = {
  ')': 1,
  ']': 2,
  '}': 3,
  '>': 4
}
def part2(path):
    def score(line):
        s = []
        for c in line:
            if c == '(': s.append(')')
            elif c == '{': s.append('}')
            elif c == '[': s.append(']')
            elif c == '<': s.append('>')
            else:
                if not s or s.pop() != c:
                    return 0
        s.reverse()

        rs = 0
        for d in s:
            rs = rs * 5 + closing_points[d]
        return rs

    with open(path) as f:
        all = f.read()
        lines = all.split("\n")
        res = 0
        scores = []
        for line in lines:
            line_score = score(line)
            if line_score == 0: continue
            # print(line_score)
            scores.append(line_score)
            # print(line, line_score)

        scores.sort()
        print(scores[len(scores)//2])


def part1(path):
    def score(line):
        s = []
        for c in line:
            if c == '(': s.append(')')
            elif c == '{': s.append('}')
            elif c == '[': s.append(']')
            elif c == '<': s.append('>')
            else:
                if not s or s.pop() != c:
                    return points[c]
        return 0

    with open(path) as f:
        all = f.read()
        lines = all.split("\n")
        res = 0
        for line in lines:
            line_score = score(line)
            res += line_score
        print(res)


if __name__ == '__main__':
    part2("./data/day10-input.txt")
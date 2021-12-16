import copy
import math
from collections import Counter

m = {}

def apply_trans(old_cp):

    cp = []
    for i in range(1, len(old_cp)):
        ft = "".join(old_cp[i-1:i+1])
        # print("st",ft, cp)
        if ft in m:
            if cp and ft[0] == cp[-1]:
                cp.extend(m[ft][1:])
            else:
                cp.extend(m[ft])
        else:
            cp.append(ft)
    return cp

def apply_trans_att2(polymer):

    def do_it(left, right):
        if left == right-1:
            return m[polymer[left:right+1]]
        # below left > right-1
        npolymer = ''
        for i in range(left, right):
            s = polymer[i:i+2]
            npolymer = do_it(left, i) + s + do_it(i, right)
        return npolymer

    return do_it(0, len(polymer)-1)



def calc_result(polymer):
    cnt = Counter(polymer)
    v = cnt.most_common()
    k1, v1 = v[0]
    k2, v2 = v[-1]
    print((k1,v1), (k2,v2))
    return (v, v1, v2, (v1-v2))




def count_stuff(two_cnt, m):
    res = defaultdict(int)
    for k, x in two_cnt.items():
        v = m[k]
        for i in range(2):
            res[v[i:i+2]] += x
    # print(res)
    return res




from collections import defaultdict

def main():

    with open("./data/day14-input.txt") as f:

        first, last = f.read().split("\n\n")
        polymer = first.strip()
        transformations = []
        last = last.split("\n")
        for line in last:
            line = line.strip()
            f, t = [x.strip() for x in line.split("->")]
            transformations.append((f,t))

        print(polymer)
        print(transformations)
        for ft, tt in transformations:
            m[ft] = "".join([ft[0],tt,ft[1]])
        two_cnt = defaultdict(int)
        for i in range(len(polymer)-1):
            two_cnt[polymer[i:i+2]] += 1

        print(two_cnt)

        print(m)
        polymer = list(polymer)
        print("polymer", "".join(polymer))
        pv1, pv2 = 1, 1
        for i in range(40):

            two_cnt = count_stuff(two_cnt, m)
            print(two_cnt)
            d = defaultdict(int)
            for k, v in two_cnt.items():
                for c in k:
                    d[c] += v
            for k, v in d.items():
                    d[k] = math.ceil(v / 2)

            print(d)
            vals = d.values()
            print(max(vals) - min(vals))



if __name__ == '__main__':
    main()
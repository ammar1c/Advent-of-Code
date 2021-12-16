import copy
import timeit
from collections import defaultdict



def count_paths_p2(adjacency_list):
    result = 0
    visited = defaultdict(int)
    def dfs(u):
        nonlocal result
        cnt = 0
        for key, value in visited.items():
            if key.islower() and value == 2:
                cnt += 1
                if cnt > 1: return

        if u.islower() and visited[u] == 2: return
        if u == "start" and visited[u] == 1: return
        if u == "end":
            result += 1
            return
        visited[u] += 1
        for v in adjacency_list[u]:
            dfs(v)
        visited[u] -= 1
    dfs("start")
    return result


def count_paths_with_stack(adjacency_list):
    visited = defaultdict(int)
    visited["start"] = 1
    stack = [("start", visited)]
    result = 0
    while stack:
        u, visited = stack.pop()
        if u == "end":
            result += 1
            continue
        for v in adjacency_list[u]:
            if v.islower() and visited[v] == 1: continue
            cp = copy.deepcopy(visited)
            cp[v] +=1
            stack.append((v, cp))
        # print(stack)

    return result


def count_paths(adjacency_list):
    result = 0
    visited = defaultdict(int)
    def dfs(u):
        nonlocal result
        if u.islower() and visited[u] == 1: return
        if u == "end":
            result += 1
            return
        visited[u] += 1
        for v in adjacency_list[u]:
            dfs(v)
        visited[u] -= 1
    dfs("start")
    return result

def main(path):
    adj_list = defaultdict(list)
    with open(path) as f:
        all = f.read()
        lines = all.split("\n")
        for line in lines:
            u, v = line.split("-")
            adj_list[u].append(v)
            adj_list[v].append(u)
    print(adj_list)
    tm = timeit.timeit(lambda: print(count_paths(adj_list)), number=10)
    print(tm)
    tm = timeit.timeit(lambda: print(count_paths_with_stack(adj_list)), number=10)
    print(tm)


if __name__ == '__main__':
    main("./data/day12-input.txt")
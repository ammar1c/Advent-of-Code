import math

from util import fs
from functools import reduce

class TreeNode:
    def __init__(self, val=-1, left=None, right=None, pred=None, succ=None):
        self.val = val
        self.left = left
        self.right = right
        self.pred = pred
        self.succ = succ

    def __repr__(self):
        if self.val != -1: return f"{self.val}"
        return f"[{self.left} {self.right}]"


def inorder(tree):
    if not tree: return []
    if not tree.left and not tree.right: return [tree.val]
    return inorder(tree.left) + inorder(tree.right)


def split(tree):
    x, y = tree.val // 2, math.ceil(tree.val / 2)
    tree.val = -1
    tree.left = TreeNode(x)
    tree.right = TreeNode(y)

    tree.left.succ = tree.right
    tree.right.pred = tree.left
    tree.left.pred = tree.pred
    if tree.pred:
        tree.pred.succ = tree.left
    tree.right.succ = tree.succ
    if tree.succ:
        tree.succ.pred = tree.right

    tree.succ = None
    tree.pred = None


def explode(tree, depth):
    if not tree: return None

    if depth == 4 and tree.val == -1:
        left, right = tree.left, tree.right

        tree.val = 0
        if left.pred:
            left.pred.val += left.val
            left.pred.succ = tree

        if right.succ:
            right.succ.val += right.val
            right.succ.pred = tree

        tree.left = None
        tree.right = None
        tree.succ = right.succ
        tree.pred = left.pred
        return tree
    else:
        return explode(tree.left, depth + 1) or explode(tree.right, depth + 1)

def split(tree):
    if not tree: return None
    if tree.val >= 10:
        x, y = tree.val // 2, math.ceil(tree.val / 2)
        tree.val = -1
        tree.left = TreeNode(x)
        tree.right = TreeNode(y)

        tree.left.succ = tree.right
        tree.right.pred = tree.left
        tree.left.pred = tree.pred
        if tree.pred:
            tree.pred.succ = tree.left
        tree.right.succ = tree.succ
        if tree.succ:
            tree.succ.pred = tree.right

        tree.succ = None
        tree.pred = None
        return tree
    return split(tree.left) or split(tree.right)

def reduce_tree(tree):
    if (re := explode(tree, 0) or split(tree)): return reduce_tree(tree)
    else: return None

def setup_succ_pred_ptrs(tree):
    pred = None
    root = tree
    stack = []
    while root or stack:
        while root:
            stack.append(root)
            root = root.left
        root = stack.pop()
        if root.val != -1:
            root.pred = pred
            if pred:
                pred.succ = root
            pred = root
        root = root.right


def add(tree1, tree2):
    n = TreeNode()
    n.left = tree1
    n.right = tree2
    setup_succ_pred_ptrs(n)
    return n


def to_tree(num):
    def dfs(num):
        left, right = num[0], num[1]
        tnode = TreeNode()
        if type(left) == int:
            tnode.left = TreeNode(left)
        else:
            tnode.left = to_tree(left)
        if type(right) == int:
            tnode.right = TreeNode(right)
        else:
            tnode.right = to_tree(right)

        return tnode

    tree = dfs(num)
    setup_succ_pred_ptrs(tree)
    return tree


def add_num_to_tree(left_tree, num):
    right_tree = to_tree(num)
    tree = add(left_tree, right_tree)
    setup_succ_pred_ptrs(tree)
    reduce_tree(tree)
    return tree


def magnitude(tree):
    if tree.val != -1: return tree.val
    return 3 * magnitude(tree.left) + 2 * magnitude(tree.right)


d="[[1,[1,2]],[3,4]]"

def parse(string):

    def parse(i):
        res = []
        comma = True
        while i < len(string):
            if string[i] == '[':
                pos, res2 = parse(i+1)
                res.append(res2)
                i += (pos-i)
            elif string[i].isdigit():
                j = i
                while i < len(string) and string[i].isdigit(): i += 1
                res.append(int(string[j:i]))
            elif string[i] == ']':
                if comma: return (i+1, res)
            elif string[i] == ',':
                comma = True
                i += 1
            else:
                i += 1
        return (i+1, res)
    return parse(0)[1][0]



# [[1,[2,3]],[3,0]]



def part2(lines):
    result = 0
    nums = [parse(num) for num in lines]
    for i in range(len(nums)):
        for j in range(len(nums)):
            if i != j:
                left_tree = to_tree(nums[i])
                right_tree = to_tree(nums[j])
                tree = add(left_tree, right_tree)
                reduce_tree(tree)
                result = max(result, magnitude(tree))
    print(result)

# [[2,3], 5]
def part1(lines):
    nums = [parse(num) for num in lines]
    final_tree = reduce(lambda x, y: add_num_to_tree(x, y), nums[1:], to_tree(nums[0]))
    print(magnitude(final_tree))


def main():
    lines = fs.readall("./data/day18-input.txt").split("\n")
    part1(lines)
    part2(lines)

if __name__ == '__main__':
    # print(parse("[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"))
    # print(eval("[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"))
    main()
import math

from util import fs


class TreeNode:
    def __init__(self, val=-1, left=None, right=None, pred=None, succ=None):
        self.val = val
        self.left = left
        self.right = right
        self.pred = pred
        self.succ = succ

    def __repr__(self):
        color = '\033[1;32;40m' if self.lev  == 4 else ''
        black = "\33[0m"
        if self.val != -1: return f"{self.val}"
        return f"{color}[{self.left} {self.right}]{black}"



def inorder(tree):

    if not tree: return []
    if not tree.left and not tree.right: return [tree.val]
    return inorder(tree.left) + inorder(tree.right)


# def explode(tree, level):
#     if not tree: return None
#     if level == 4 and tree.val == -1:
#         left, right = tree.left, tree.right
#         if left.pred:
#             left.pred.val += left.val
#             left.pred.succ = right.succ
#         if right.succ:
#             right.succ.val += right.val
#             right.succ.pred = left.pred
#         tree.val = 0
#         tree.left = None
#         tree.right = None
#         return tree
#     return explode(tree.left, level + 1) or explode(tree.right, level + 1)
#

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


def reduce(tree, level, itype):
    if not tree: return None
    if itype:
        if level == 4 and tree.val == -1:
            left, right = tree.left, tree.right

            tree.val = 0
            if left.pred:
                left.pred.val += left.val
                left.pred.succ = tree

                assert(tree.val != -1)
            if right.succ:
                right.succ.val += right.val
                right.succ.pred = tree
                assert (tree.val != -1)

            tree.left = None
            tree.right = None
            tree.succ = right.succ
            tree.pred = left.pred
            return tree
    else:
        if tree.val >= 10:
            split(tree)
            return tree
    return reduce(tree.left, level + 1, itype) or reduce(tree.right, level + 1, itype)


def bond(tree):
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

def greatest(tree):
    if tree.right: return greatest(tree.right)
    if tree.left: return greatest(tree.left)
    return tree
def smallest(tree):
    if tree.left: return smallest(tree.left)
    if tree.right: return smallest(tree.right)
    return tree

def add(tree1, tree2):
    n = TreeNode()
    n.left = tree1
    n.right = tree2
    # x0 = greatest(tree1)
    # x1 = smallest(tree2)
    # x0.succ = x1
    # x1.pred = x0
    return n



def to_tree(num,lev=1):
    left, right = num[0], num[1]
    tnode = TreeNode(lev=lev)
    if type(left) == int:
        tnode.left = TreeNode(left, lev=lev)
    else:
        tnode.left = to_tree(left, lev+1)
    if type(right) == int:
        tnode.right = TreeNode(right, lev=lev)
    else:
        tnode.right = to_tree(right, lev+1)

    return tnode

def recolor(tree, lev=0):
    if not tree: return
    tree.lev = lev
    recolor(tree.left, lev + 1)
    recolor(tree.right, lev + 1)

def verify(tree):
    pred = None
    root = tree
    stack = []
    while root or stack:
        while root:
            stack.append(root)
            root = root.left
        root = stack.pop()
        if root.val != -1:
            assert not root.pred or root.pred.val != -1
            assert not root.succ or root.succ.val != -1

        root = root.right
def add_line(left_tree, num):

    right_tree = to_tree(num)
    bond(right_tree)
    tree = add(left_tree, right_tree)
    bond(tree)
    recolor(tree)
    # while reduce(left_tree, 0): pass
    # while reduce(right_tree, 0): pass
    print("before", tree)
    i = 0

    while reduce(tree, 0, True) or reduce(tree, 0, False):
        recolor(tree)
        # print(i, tree)
        i += 1

    # print("after", tree)
    return tree

def magnitude(tree):
    if tree.val != -1: return tree.val
    return 3*magnitude(tree.left) + 2*magnitude(tree.right)

def part2(lines):
    result = 0
    nums = [eval(num) for num in lines]
    print(nums)
    for i in range(len(nums)):
        for j in range(len(nums)):
            if i != j:
                left_tree = to_tree(nums[i])
                right_tree = to_tree(nums[j])
                bond(left_tree)
                bond(right_tree)
                tree = add(left_tree, right_tree)
                bond(tree)
                while reduce(tree, 0, True) or reduce(tree, 0, False):
                    pass
                mag = magnitude(tree)
                # print(i,j, mag)
                # print(tree)
                result = max(result, magnitude(tree))
    print(result)



def part1(lines):
    num1 = eval(lines[0])
    left_tree = to_tree(num1)
    bond(left_tree)
    i = 1
    while i < len(lines):
        num2 = eval(lines[i])
        # print(num2)
        left_tree = add_line(left_tree, num2)
        i += 1
    print(magnitude(left_tree))

if __name__ == '__main__':
    num1 = eval("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")
    num2 = eval("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")

    lines = fs.readall("./data/day18-input.txt").split("\n")
    part2(lines)


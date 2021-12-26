import copy


class List:
    def __init__(self):
        self.backing = []
    def __index__(self, i): return self.backing[i]
    def head(self): return self.backing[0]
    def headOption(self): return self.backing[0] if self.size() > 0 else None
    def size(self): return len(self.backing)
    def __getitem__(self, i): return self.backing[i]
    def tail(self):
        l = List()
        l.backing = copy.deepcopy(self.backing)
        return l[1:]

    def mkString(self, prefix="", sep=",", suffix=""):
        return prefix + sep.join([str(x) for x in self.backing]) + suffix



class tests:

    @staticmethod
    def testMkString():
        list = List()

        print(list.mkString())

if __name__ == '__main__':

    tests.testMkString()
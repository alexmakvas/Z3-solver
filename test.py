import math
from z3 import *
def solve(a):
    ##################################################
    # reading input
    ##################################################
    f = open(a)
    system = f.read()
    li = list(system)
    for i in range(len(li)):
        if li[i] == '\n':
            li[i] = ' '
    system = ''.join(li)


    # print(system)
    ##################################################
    # classes representing equasions
    ##################################################
    class Equasions():
        array = []

        def __init__(self, left, right):
            self.left = left
            self.right = right

        def __str__(self):

            return f"{self.left} , {self.right}"

        def toZ3(self):
            self.array.append(self.left.toZ3())
            if type(self.right) == Equasions:  # normalizing output format, func similar to cons
                self.right.toZ3()
            else:
                self.array.append(self.right.toZ3())

            return self.array


    class Equal():  # implementation of Equality function
        def __init__(self, left, right):
            self.left = left
            self.right = right

        def __str__(self):
            return f"{self.left} = {self.right}"

        def toZ3(self):
            return self.left.toZ3() == self.right.toZ3()


    class Expr:
        def __add__(self, other):
            if isinstance(other, int) or isinstance(other, float):
                other = Con(other)

            if isinstance(other, str):
                other = Var(other)

            if isinstance(other, Expr):
                return Plus(self, other)

            print(f"Non-matching types for +: {type(self)} and {type(other)}")
            return None

        def __mul__(self, other):
            if isinstance(other, int) or isinstance(other, float):
                other = Con(other)

            if isinstance(other, str):
                other = Var(other)

            if isinstance(other, Expr):
                return Times(self, other)

            print(f"Non-matching types for *: {type(self)} and {type(other)}")
            return None


    class Con(Expr):
        def __init__(self, val: float):
            self.val = val

        def ev(self, env={}):
            return self.val

        def __str__(self):
            return str(self.val)

        def toZ3(self):
            return int(self.val)

        def __eq__(self, other):
            if isinstance(other, Con):
                return self.val == other.val

            return False

        def vars(self):
            return []


    class Var(Expr):
        def __init__(self, name: str):
            self.name = name

        def ev(self, env={}):
            return env[self.name]

        def __str__(self):
            return self.name

        def toZ3(self):
            return Int(self.name)

        def __eq__(self, other):
            if isinstance(other, Var):
                return self.name == other.name

            return False

        def vars(self):
            return [self.name]


    class BinOp(Expr):
        def __init__(self, left: Expr, right: Expr):
            self.left = left
            self.right = right

        def ev(self, env={}):
            return self.op(self.left.ev(env), self.right.ev(env))

        def __str__(self):
            return f"({self.left} {self.name} {self.right})"

        def __eq__(self, other):
            if isinstance(other, BinOp) and self.name == other.name:
                return self.left == other.left and self.right == other.right

            return False

        def vars(self):
            return self.left.vars() + self.right.vars()


    class Plus(BinOp):
        name = '+'

        def op(self, x, y):
            return x + y

        def toZ3(self):
            return self.left.toZ3() + self.right.toZ3()


    class Minus(BinOp):
        name = "-"

        def op(self, x, y):
            return x - y

        def toZ3(self):
            return (self.left.toZ3() - self.right.toZ3())


    class Times(BinOp):
        name = '*'

        def op(self, x, y):
            return x * y

        def toZ3(self):
            return self.left.toZ3() * self.right.toZ3()


    ##################################################
    # classes representing bools
    ##################################################

    class Booleans():
        array = []

        def __init__(self, left, right):
            self.left = left
            self.right = right

        def __str__(self):

            return f"{self.left} , {self.right}"

        def toZ3(self):  # normalazing Constraints output to simple array structure
            self.array.append(self.left.toZ3())
            if type(self.right) == Booleans:
                self.right.toZ3()
            else:
                self.array.append(self.right.toZ3())

            return self.array


    class IneqOp():  # function simillar to BinOp
        def __init__(self, left, right):
            self.left = left
            self.right = right

        def ev(self, env={}):
            return self.op(self.left.ev(env), self.right.ev(env))

        def __str__(self):
            return f"{self.left} {self.name} {self.right}"

        def vars(self):
            return self.left.vars() + self.right.vars()


    class Greater(IneqOp):  # > operation

        name = '>'

        def op(self, x, y):
            return x > y

        def toZ3(self):
            return self.left.toZ3() > self.right.toZ3()

    
    class Smaller(IneqOp):  # < operation

        name = '<'

        def op(self, x, y):
            return x < y

        def toZ3(self):
            return self.left.toZ3() < self.right.toZ3()


    class And1():  # and() operation
        def __init__(self, left, right):
            self.left = left
            self.right = right

        def __str__(self):
            return f"{self.left} and {self.right}"

        def toZ3(self):
            return And(self.left.toZ3(), self.right.toZ3())


    class Or1():  # Or() operation
        def __init__(self, left, right):
            self.left = left
            self.right = right

        def __str__(self):
            return f"{self.left} or {self.right}"

        def toZ3(self):
            return Or(self.left.toZ3(), self.right.toZ3())


    ##################################################
    # parser combinations
    ##################################################

    result = lambda p: p[0][0]
    rest = lambda p: p[0][1]


    class Parser:
        def __rshift__(self, other):
            return Seq(self, other)

        def __xor__(self, other):
            return OrElse(self, other)

        def parse(self, inp):
            return self.parser.parse(inp)

        def cons(x, xs):
            if type(x) == str and xs == []:
                return x
            if type(xs) == str:
                return x + xs
            return [x] + xs


    class Seq(Parser):
        def __init__(self, parser, and_then):
            self.parser = parser
            self.and_then = and_then

        def parse(self, inp):
            p = self.parser.parse(inp)
            if p != []:
                return self.and_then(result(p)).parse(rest(p))

            return []


    class OrElse(Parser):
        def __init__(self, parser1, parser2):
            self.parser1 = parser1
            self.parser2 = parser2

        def parse(self, inp):
            p = self.parser1.parse(inp)
            if p != []:
                return p

            return self.parser2.parse(inp)


    class ParseItem(Parser):
        def parse(self, inp):
            if inp == "":
                return []
            return [(inp[0], inp[1:])]


    class Return(Parser):
        def __init__(self, x):
            self.x = x

        def parse(self, inp):
            return [(self.x, inp)]


    class Fail(Parser):
        def parse(self, inp):
            return []


    ###########################################

    class ParseIf(Parser):
        def __init__(self, pred):
            self.parser = ParseItem() >> (lambda x:
                                          Return(x) if pred(x) else Fail())


    class ParseDigit(Parser):
        def __init__(self):
            self.parser = ParseIf(str.isdigit)


    class ParseSome(Parser):
        def __init__(self, parser):
            self.parser = parser >> (lambda x:
                                     ParseMany(parser) >> (lambda xs:
                                                           Return(Parser.cons(x, xs))))


    class ParseMany(Parser):
        def __init__(self, parser):
            self.parser = ParseSome(parser) ^ Return([])


    class ParseNat(Parser):
        def __init__(self):
            self.parser = ParseSome(ParseDigit()) >> (lambda ds: 
                                                      Return(int(ds)))


    class ParseChar(Parser):
        def __init__(self, c):
            self.parser = ParseIf(lambda x: x == c)


    class ParseInt(Parser):
        def __init__(self):
            self.parser = ParseChar('-') >> (lambda _:
                                             ParseNat() >> (lambda n:
                                                            Return(-n))) \
                          ^ ParseNat()




    class ParseSpace(Parser):
        def __init__(self):
            self.parser = ParseMany(ParseIf(str.isspace)) >> (lambda _:
                                                              Return([]))


    class ParseToken(Parser):
        def __init__(self, parser):
            self.parser = ParseSpace() >> (lambda _:
                                           parser >> (lambda x:
                                                      ParseSpace() >> (lambda _:
                                                                       Return(x))))


    class ParseNatural(Parser):
        def __init__(self):
            self.parser = ParseToken(ParseNat)


    class ParseInteger(Parser):
        def __init__(self):
            self.parser = ParseToken(ParseInt())


    class ParseString(Parser):
        def __init__(self, string):
            self.string = string
            self.parser = ParseChar(self.string[0]) >> (lambda x:
                                                        ParseString(self.string[1:]) >> (lambda xs:
                                                                                         Return(Parser.cons(x,
                                                                                                            xs)))) if self.string else Return(
                '')


    class ParseSymbol(Parser):
        def __init__(self, string):
            self.parser = ParseToken(ParseString(string))


    class ParseLower(Parser):
        def __init__(self):
            self.parser = ParseIf(str.islower)

    class ParseAlphanum(Parser):
        def __init__(self):
            self.parser = ParseIf(str.isalnum)


    class ParseIdent(Parser):
        def __init__(self):
            self.parser = ParseLower() >> (lambda x:
                                           ParseMany(ParseAlphanum()) >> (lambda xs:
                                                                          Return(Parser.cons(x, xs))))


    class ParseIdentifier(Parser):
        def __init__(self):
            self.parser = ParseToken(ParseIdent())


    class ParseFactor(Parser):
        def __init__(self):
            self.parser = ParseInteger() ^ ParseIdentifier() ^ \
                          (ParseSymbol("(") >> (lambda _:
                                                ParseAExpr() >> (lambda e:
                                                                 ParseSymbol("") >> (lambda _:
                                                                                     Return(e)))))


    class ParseTerm(Parser):
        def __init__(self):
            self.parser = (ParseFactor() >> (lambda n:
                                             ParseSymbol("*") >> (lambda _:
                                                                  ParseTerm() >> (lambda m:
                                                                                  Return(n * m))))) ^ ParseFactor()


    class ParseAExpr(Parser):
        def __init__(self):
            self.parser = (ParseTerm() >> (lambda n:
                                           ParseSymbol("+") >> (lambda _:
                                                                ParseAExpr() >> (lambda m:
                                                                                 Return(n + m))))) ^ ParseTerm()


    class ParseSEquasions(Parser):
        def __init__(self):
            self.parser = (ParseSEqual() >> (lambda n:
                                             ParseSymbol(",") >> (lambda _:
                                                                  ParseSEquasions() >> (lambda m:
                                                                                        Return(Equasions(n,
                                                                                                         m)))))) ^ ParseSEqual()


    class ParseSInteger(Parser):
        def __init__(self):
            self.parser = ParseInteger() >> (lambda n: Return(Con(n)))


    class ParseSIdentifier(Parser):
        def __init__(self):
            self.parser = ParseIdentifier() >> (lambda var: Return(Var(var)))


    class ParseSFactor(Parser):
        def __init__(self):
            self.parser = ParseSInteger() ^ ParseSIdentifier() ^ \
                          (ParseSymbol("(") >> (lambda _:
                                                ParseSExpr() >> (lambda e:
                                                                 ParseSymbol(")") >> (lambda _:
                                                                                      Return(e)))))


    class ParseSEqual(Parser):
        def __init__(self):
            self.parser = (ParseSExpr() >> (lambda n:
                                            ParseSymbol("=") >> (lambda _:
                                                                 ParseSExpr() >> (lambda m:
                                                                                  Return(Equal(n, m)))))) ^ ParseSExpr()


    class ParseSTerm(Parser):
        def __init__(self):
            self.parser = (ParseSFactor() >> (lambda n:
                                              ParseSymbol("*") >> (lambda _:
                                                                   ParseSTerm() >> (lambda m:
                                                                                    Return(Times(n, m)))))) ^ ParseSFactor()


    class ParseSExpr(Parser):
        def __init__(self):
            self.parser = (ParseSTerm() >> (lambda n:
                                            ParseSymbol("+") >> (lambda _:
                                                                 ParseSExpr() >> (lambda m:
                                                                                  Return(Plus(n, m)))))) ^ (
                                      ParseSTerm() >> (lambda n:
                                                       ParseSymbol("-") >> (lambda _:
                                                                            ParseSExpr() >> (lambda m:
                                                                                             Return(Minus(n, m))))))  ^  ParseSTerm()


    class ParseSBools(Parser):
        def __init__(self):
            self.parser = (ParseSBool() >> (lambda n:
                                            ParseSymbol(",") >> (lambda _:
                                                                 ParseSBools() >> (lambda m:
                                                                                   Return(Booleans(n, m)))))) ^ ParseSBool()


    class ParseSConj(Parser):
        def __init__(self):
            self.parser = (ParseSymbol("(") >> (lambda _:
                                                ParseSBool() >> (lambda e:
                                                                 ParseSymbol(")") >> (lambda _:
                                                                                      Return(
                                                                                          e))))) ^ ParseSSmaller() ^ ParseSGreater() ^ ParseSEquasions()


    class ParseSDisj(Parser):
        def __init__(self):
            self.parser = (ParseSConj() >> (lambda n:
                                            ParseString("and") >> (lambda _:
                                                                   ParseSDisj() >> (lambda m:
                                                                                    Return(And1(n, m)))))) ^ ParseSConj()


    class ParseSBool(Parser):
        def __init__(self):
            self.parser = (ParseSDisj() >> (lambda n:
                                            ParseString("or") >> (lambda _:
                                                                  ParseSBool() >> (lambda m:
                                                                                   Return(Or1(n, m)))))) ^ ParseSDisj()


    class ParseSSmaller(Parser):
        def __init__(self):
            self.parser = (ParseSExpr() >> (lambda n:
                                            ParseSymbol("<") >> (lambda _:
                                                                 ParseSExpr() >> (lambda m:
                                                                                  Return(Smaller(n, m))))))


    class ParseSGreater(Parser):
        def __init__(self):
            self.parser = (ParseSExpr() >> (lambda n:
                                            ParseSymbol(">") >> (lambda _:
                                                                 ParseSExpr() >> (lambda m:
                                                                                  Return(Greater(n, m))))))


    class Sys(Parser):  # System parser
        def __init__(self):
            self.parser = ParseString('Solve') >> (lambda _:
                                                   ParseSEquasions() >> (lambda e:
                                                                         ParseString('such that') >> (lambda _:
                                                                                                      ParseSBools() >> (
                                                                                                          lambda m:
                                                                                                          Return([e,
                                                                                                                  m]))))) ^ ParseString(
                'Solve') >> (lambda _:
                             ParseSEquasions() >> (lambda e: Return([e])))


    res = result(Sys().parse(system))  # parsing system of equasions&constraints
    print(res[0].toZ3())
    if rest(Sys().parse(system))[0] == '.':  # prevent partial parsing of system , to avoid mistakes such as parsing Solve x = 3 and y = 0
        equ = res[0].toZ3()  # Equasions
        if len(res) >= 2:  # If Constraints exist add them else leave array blank
            constr = res[1].toZ3()
        else:
            constr = []
    else:
        equ = []
        constr = []

    # print(equ)
    # print(constr)

    s = Solver()  # Z3 Solver

    if type(equ) == Equasions:  # If there is more than 1 Equasion s.add(all) , else s.add(one)
        for i in range(len(equ)):
            s.add(equ[i])
    else:
        s.add(equ)

    if type(constr) == Booleans:  # If there is more than 1 Constraint s.add(all) , else s.add(one)
        for i in range(len(constr)):
            s.add(constr[i])
    else:
        s.add(constr)

    if s.check() == sat:  # If system satifies Z3 syntax is correct and has a solution -> solve, else "No Solution!"
        return(s.model())
    else:
        return ('No solution!')
sol = solve('sys.txt')
print(sol)
from z3 import *

solution = Solver()

y0 = Int("y0")
k0 = Int("k0")
y  = Int("y")
t  = Int("t")
k  = Int("k")

solution.add(And(y==y0,k==k0,t==(y0-k0)))
solution.add(Not(And(t==y-k0,k==k0,ForAll([y,t],Implies(
                                            And(t==y-k0,k==k0),
                                                And(
                                                    Implies(
                                                        t > 0,
                                                        And(
                                                            t-1==y-1-k0,
                                                            k==k0
                                                        )
                                                    ),
                                                    Implies(Not(t>0),y<=k0)
                                            )
                                    )
                        )
                )))
print(solution.sexpr())

print(solution.check())
# print(solution.model())
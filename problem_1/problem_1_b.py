from z3 import * 

solution = Solver()

joe_age_1997          = Int('joe_age_1997')
ted_age_1997          = Int('ted_age_1997')
mary_age_1997         = Int('mary_age_1997')
he_is_older_20        = Xor(joe_age_1997 >20,ted_age_1997> 20)
went_to_college_early = Xor(joe_age_1997 <12,ted_age_1997< 12, mary_age_1997 <12)


solution.add(mary_age_1997+5 == joe_age_1997)
solution.add(mary_age_1997-8 == ted_age_1997)
solution.add(went_to_college_early)
solution.add(he_is_older_20)

while solution.check() == sat:
  print(solution.model())
  solution.add(Or(joe_age_1997 != solution.model()[joe_age_1997], ted_age_1997 != solution.model()[ted_age_1997], mary_age_1997 != solution.model()[mary_age_1997]))
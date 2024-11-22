theory X86_Main
  imports X86_Instantiation_C X86_Instantiation_B X86_Instantiation_S X86_Generic_Algo
begin



text \<open>The algorithm is defined here:\<close>
thm algo.algo.simps

text \<open>The main theorem (correctness of the algorithm) is defined here:\<close>
thm algo.main5

end


end
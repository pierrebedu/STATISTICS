# STATISTICS
Statistical main laws (illustrations)/ main statistical tests.

NO CLEAN CODE YET . in french, for personnal purposes => ////////DO NOT READ//////////////



I/ one sample statistical tests :
- shapiro normality test
- student t test for u=u0                         (student law)
- homemade sigma²=sigma0² procedure (not in R!)   (Xhi2 law)
- kolmogorov-smirnov ecdf comparison with any other law's distribution (empirical cumulative distribution function)  

II/ two samples tests :
a) Paired samples:
- shapiro on differences Xi-Yi
- student paired (= student on differences) u1=u2                          (student law)
- Wilcoxon signed ranks test (paired) Px=Py                                (asymptotical N(0;1) law)

b) Independant samples 
- shapiro for each sample
- fisher test V1=V2                                                         (fisher law)
- student (NOT paired) + equal variances:    u1=u2                            (student)
- student (NOT paired)+ NOT equal variances  u1=u2                           (asymptotical N(0; 1) law)
- wilcoxon rank test (not paired) Px=Py                                      ( asymptotical N(0;1) law)

- kolmogorov-smirnov ecdfs' comparison

III/ Xhi2 independancy test on a contingency table (2 qualitative variables)   (Xhi2 law)

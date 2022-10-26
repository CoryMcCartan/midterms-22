library(dplyr)
library(here)

# simulate RCV in AK ----------
sim_AK = function(dshare, N = 1000) {
    p_rep = rdirichlet(N, c(.3092, .2784) * 125) # 2022 special palin/begich shares
    p_round1 = cbind(dshare, (1 - dshare) * p_rep)
    elim_begich = p_round1[, 2] > p_round1[, 3]
    mean(elim_begich) # prob Palin beats Begich
    # where the ballots go if Begich is out -- last entry is spoiled
    realloc_begich = rdirichlet(N, c(16.449, 27.687, 11.374) * 7) # 2022 begich realloc
    # where the ballots go if Palin is out -- last entry is spoiled
    realloc_palin = rdirichlet(N, c(2, 40, 15) * 7) # guess few Palin->Peltola, many Palin->Begish, slightly higher exhaustion
    p_round2 = matrix(0, nrow=N, ncol=2)
    p_round2[elim_begich, ] = p_round1[elim_begich, c(1, 2)] + p_round1[elim_begich, 3] * realloc_begich[elim_begich, 1:2]
    p_round2[!elim_begich, ] = p_round1[!elim_begich, c(1, 3)] + p_round1[!elim_begich, 2] * realloc_palin[!elim_begich, 1:2]
    p_round2 = p_round2 / rowSums(p_round2)
    tibble(dshare = dshare,
           pr_dem = mean(p_round2[, 1] > 0.5))
}

res_AK = map_dfr(seq(0.35, 0.58, 0.002), sim_AK, N=10e3)
write_rds(res_AK, here("data/rcv/rcv_AK.rds"), compress="gz")


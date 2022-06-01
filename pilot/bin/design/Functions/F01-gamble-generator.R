gamble_combinations <- function(data = NULL, n_gambles){
  
  payout_combinations <- data.frame(permutations(length(data), n_gambles*2, data)) %>% 
    filter(X1 > 1, X2 < 1, X3 > 1, X4 < 1) %>% 
    rename("rw_left"  = X1, 
           "p_left"   = X2, 
           "rw_right" = X3,
           "p_right"  = X4) %>% 
    mutate(ev_right = rw_right * p_right,
           ev_left  = rw_left * p_left,
           dir_rw   = sign(rw_left - rw_right),
           dir_p    = sign(p_left - p_right),
           dif_rw   = abs(rw_left - rw_right),
           dif_p    = abs(p_left - p_right),
           dif_ev   = round(abs(ev_left - ev_right), 3)) 
  
  
}
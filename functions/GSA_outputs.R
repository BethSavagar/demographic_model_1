
GSA_output <- function(output_df, Vstart){
  
  pop_growth <- output_df[nrow(output_df),"sum_pop"] / output_df[1,"sum_pop"]
  tenyr_growth <- (output_df[t1, "sum_pop"]) / output_df[t2, "sum_pop"]
  
  wk2mnth <- 4.345 # weeks to months 
  wk_6m <- round(wk2mnth*6, 0)
  wk_12m <- round(wk2mnth*12, 0)
  Imm_6m <- output_df[Vstart+wk_6m, "prop_immune"]
  Imm_12m <- output_df[Vstart+wk_12m, "prop_immune"]
  Imm70_w <- output_df %>%
    filter(w>=Vstart, 
           prop_immune<0.7) %>%
    pull(w) %>% 
    min()
  
  output <- output_df %>% 
    filter(w==max(w)) %>%
    cbind(pop_growth=pop_growth,
          tenyr_growth = tenyr_growth, 
          imm_6m=Imm_6m,
          imm_12m=Imm_12m,
          imm70_w=Imm70_w) %>%
    select(w, 
           sum_pop, 
           pop_growth, 
           tenyr_growth,
           prop_immune, 
           imm_6m, 
           imm_12m,
           imm70_w,
           pR_noIm, 
           prop_inf, 
           starts_with("pf"),
           starts_with("pm"))
  return(output)
}

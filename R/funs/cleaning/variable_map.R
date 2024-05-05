enpro_dat %>% 
  arrange(adipose_fin_clip_clean) %>% 
  distinct(adipose_fin_clip_clean, `ADIPOSE FIN CLIP`)

variable_map <- enpro_dat %>% 
  arrange(adipose_fin_clip_clean) %>% 
  select(raw = `ADIPOSE FIN CLIP`, clean = adipose_fin_clip_clean) %>% 
  distinct() %>% 
  mutate(var = "ADIPOSE FIN CLIP")



ggplot()

tvp3 %>% 
  ggplot(.,aes(x = counseling_type,
               y = num_kill,
               group = num_kill
  ))+
  theme_bw()+
  geom_col(fill = "darkgreen")+
  coord_flip()+
  geom_smooth()

## The Text doesn't return without the frcode() applied. Try this:

tvp4 %>% 
  ggplot(.,aes(x = counseling_type,
               y = num_kill,
               group = counseling_type
               ))+
  theme_bw()+
  geom_col(fill = "darkgreen")+
  coord_flip()+
  geom_smooth()

# 


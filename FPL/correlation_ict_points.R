merged_gw <- read.csv(url("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2021-22/gws/merged_gw.csv"))
View(merged_gw)

by_players <- merged_gw %>% 
  group_by(name) %>% 
  mutate(next_gw = GW + 1) %>% 
  filter(ict_index != 0)
View(by_players)

df1 <- by_players
df2 <- by_players
merge(df1,df2,by.x=c("name", "GW"),by.y=c("name","next_gw"))

df <- inner_join(by_players,by_players, by.x = c("name", "GW"), by.y = c("name","next_gw")) %>% 
  merge(by_players, by.x = c("name", "GW"), by.y = c("name","next_gw")) %>% 
  select(name, ict_index.x, GW, next_gw, total_points.y)

require(plyr)
func <- function(df)
{
  return(data.frame(COR = cor(df$total_points.y, df$ict_index.x)))
}

cor_players <- na.omit(ddply(df, .(name), func))
head(cor_players)
cor_players <- cor_players %>% 
  arrange(-COR)
head(cor_players)

more_than__half <- merged_gw %>% 
  group_by(name) %>% 
  filter(ict_index != 0) %>% 
  summarize(minutes_played = sum(minutes)) %>% 
  filter(minutes_played >= 90 * max(merged_gw$GW) / 2)

positive_cor <- left_join(more_than__half,cor_players, by = "name") %>% 
  arrange(-COR)
negative_cor <- positive_cor %>% 
  arrange(COR)

p <- rbind(head(positive_cor,10),head(negative_cor,10)) %>% 
  arrange(-COR)
p <- p %>% 
  select(name,COR)
merged_gw_last <- merged_gw %>% 
  filter(GW == max(GW))

out <- left_join(p,merged_gw_last,by = "name") %>% 
  select(name,COR,ict_index)

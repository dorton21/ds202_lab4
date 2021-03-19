offOreOkl <- offClean %>%
  filter(Opponent_Opponent %in% c("Oregon", "Oklahoma")) %>%
  group_by(Name) %>%
  filter(stat == "Receiving_YDS")
offOklahoma <- offOreOkl %>%
  filter(Opponent_Opponent %in% c("Oklahoma")) %>%
  group_by(Name) %>%
  summarise(Average = mean(value))
offOregon <- offOreOkl %>%
  filter(Opponent_Opponent %in% c("Oregon"))
offOreOklAvg <- offOreOkl %>%
  left_join(offOklahoma)
offOreOklPivoted <- offOreOklAvg %>%
  pivot_wider(names_from = 'stat', values_from = 'value') %>%
  filter(Opponent_Opponent %in% c("Oregon")) %>%
  rename(Oklahoma_Receiving_YDS = Average,
         Oregon_Receiving_YDS = Receiving_YDS) %>%
  mutate(Oklahoma_Receiving_YDS = as.numeric(Oklahoma_Receiving_YDS),
         Oregon_Receiving_YDS = as.numeric(Oregon_Receiving_YDS))

ggplot(offOreOklPivoted, aes(x = Oklahoma_Receiving_YDS, y = Oregon_Receiving_YDS)) + 
  geom_point() + xlab("Average Oklahoma Receiving Yards") + ylab("Oregon Receiving Yards") + 
  ggtitle("Oklahoma Receiving Yards vs Oregon Receiving Yards by ISU in 2020")


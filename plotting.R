# This file contains functions to plot data.

goals_per_team = read.csv("data/goals_per_team.csv", header=T)

plot_goals_per_team = function() {
  ggplot(goals_per_team, aes(x=avg_goals_received, y=avg_goals_scored)) +
      geom_point(alpha=.6)
}


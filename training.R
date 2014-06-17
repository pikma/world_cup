library(ggplot2)

goals_per_team = read.csv("data/goals_per_team.csv", header=T)

plot_goals_per_team = function() {
  ggplot(goals_per_team, aes(x=avg_goals_received, y=avg_goals_scored)) +
      geom_point(alpha=.6)
}

matches = read.csv("data/all_matches.csv", header=T)
all_teams = union(levels(matches[["first_team"]]), levels(matches[["second_team"]]))
matches[["first_team"]] = as.integer(factor(matches[["first_team"]], levels=all_teams))
matches[["second_team"]] = as.integer(factor(matches[["second_team"]], levels=all_teams))

num_teams = length(all_teams)
num_matches = length(matches[,1])


lambda = function(offense, defense) {
  0.3 + max(0, offense - defense)
}
default_lambda = lambda(0, 0)

score_loss = function(offense_i, defense_j, num_goals_i_to_j) {
  - dpois(num_goals_i_to_j, lambda=lambda(offense_i, defense_j) , log=T)
}

poisson_substractive_model = function () {

  offense_params = c(runif(n=num_teams, min=0, max=3))
  defense_params = c(runif(n=num_teams, min=0, max=2))

  all_pars = c(offense_params, defense_params)

  loss = function(par) {
    match_losses = apply(matches, 1, function(match) {
      first_team = match[[1]]
      second_team = match[[2]]
      first_offense = par[first_team]
      second_offense = par[second_team]
      first_defense = par[num_teams + first_team]
      second_defense = par[num_teams + second_team]

      a = score_loss(first_offense, second_defense, match[[3]])
      b = score_loss(second_offense, first_defense, match[[4]])
      return(a+b)
    })
    res = sum(match_losses)
    res
  }

  gradient = function(par) {
    result = 0 * seq(length=2*num_teams)

    for (i in seq(1, num_matches)) {
      match = matches[i,]
      first_team = match[[1]]
      second_team = match[[2]]
      first_goals = match[[3]]
      second_goals = match[[4]]

      l = lambda(par[first_team], par[num_teams + second_team])
      if (l != default_lambda) {
        tmp = first_goals / l - 1
        result[first_team] = result[first_team] - tmp
        result[num_teams + second_team] = result[num_teams + second_team] + tmp
      }

      l = lambda(par[second_team], par[num_teams + first_team])
      if (l != default_lambda) {
        tmp = second_goals / l - 1
        result[second_team] = result[second_team] - tmp
        result[num_teams + first_team] = result[num_teams + first_team] + tmp
      }
    }
    result
  }

  loss(all_pars)
  res = optim(par=all_pars, fn=loss, method="L-BFGS-B", gr=gradient,
              control=list(maxit=1000, trace=1), lower=rep(0,length(all_pars)))
  res[["par"]]
}

team_index = function(team_str) {
  as.integer(factor(team_str, levels=all_teams))
}

print_team_info = function(par, team_str) {
  team = team_index(team_str)
  cat(team_str, ": ", par[team], " / ", par[num_teams + team])
}
predict = function(par, first_team_str, second_team_str) {
  print_team_info(par, first_team_str)
  print_team_info(par, second_team_str)
  first_team = team_index(first_team_str)
  second_team = team_index(first_team_str)

}

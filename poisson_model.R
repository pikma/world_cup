library(ggplot2)

matches = read.csv("data/all_matches.csv", header=T)

# We build one factor with all teams.
all_teams = union(levels(matches[["first_team"]]),
                  levels(matches[["second_team"]]))
matches[["first_team"]] = factor(matches[["first_team"]], levels=all_teams)
matches[["second_team"]] = factor(matches[["second_team"]], levels=all_teams)
# We need to convert the team to integers, because apply() will convert our data
# frame to a matrix, which will convert the factors to strings, whereas we want
# the team intexes.
matches[["first_team"]] = as.integer(matches[["first_team"]])
matches[["second_team"]] = as.integer(matches[["second_team"]])

num_teams = length(all_teams)
num_matches = length(matches[,1])

# The lambda parameter in the Poisson distribution of the number of goals from
# one team to another.
#
# Arguments:
#   offense: the offense parameter, in the model, of the team which scores.
#   defense: the defense parameter, in the model, of the team which receives the
#       goals.
#
# Returns:
#   A double l, such that the number of goals is sampled from Poisson(l).
lambda = function(offense, defense) {
  0.3 + max(0, offense - defense)
}
default_lambda = lambda(0, 0)

# Helper functions that return the offense/defense parameter, in the model,
# associated with a team.
#
# Arguments:
#   par: the vector of parameters.
#   team: integer.
#
# Returns:
#   a double.
offense = function(par, team) { par[team] }
defense = function(par, team) { par[num_teams + team] }

# The loss associated with a number of goals scored by one team to another.
#
# Arguments:
#   par: the vector of parameters.
#   team_from, team_to: integers.
#   num_goals: the number of goals scored by team_from against team_to
#       (integer).
#
# Returns:
#   a double.
score_loss = function(par, team_from, team_to, num_goals) {
  - dpois(num_goals,
          lambda=lambda(offense(par, team_from), defense(par, team_to)),
          log=T)
}

# The logarithm of the loss function, i.e. -log(P(data|parameters)).
#
# Arguments:
#   par: the vector of parameters.
#
# Returns:
#   a double.
loss = function(par) {
  match_losses = apply(matches, 1, function(match) {
    # The match vector is
    #    [first_team, second_team, first_team_goals, second_team_goals].
    first_team = match[1]
    second_team = match[2]
    a = score_loss(par, first_team, second_team, match[3])
    b = score_loss(par, second_team, first_team, match[4])
    return(a+b)
  })
  return(sum(match_losses))
}

# The gradient increment associated with a number of goals scored by one team to
# another.
#
# Arguments:
#   par: the vector of parameters.
#   team_from, team_to: integers.
#   num_goals: the number of goals scored by team_from against team_to
#       (integer).
#
# Returns:
#   a double.
gradient_increment = function(par, team_from, team_to, num_goals) {
  l = lambda(offense(par, team_from), defense(par, team_to))
  if (l == default_lambda) {
    return(0)
  } else {
    return(num_goals / l - 1)
  }
}

# The gradient of the loss function with respect to the parameters.
#
# Arguments:
#   par: the vector of parameters.
#
# Returns:
#   a vector of 2*num_teams + 1 gradient coordinates.
gradient = function(par) {
  result = seq(0, 0, len=2*num_teams)
  for (i in seq(1, num_matches)) {
    match = matches[i,]
    first_team = match[[1]]
    second_team = match[[2]]
    first_goals = match[[3]]
    second_goals = match[[4]]

    increment = gradient_increment(par, first_team, second_team, first_goals)
    result[first_team] = result[first_team] - increment
    result[num_teams + second_team] =
        result[num_teams + second_team] + increment

    increment = gradient_increment(par, second_team, first_team, second_goals)
    result[second_team] = result[second_team] - increment
    result[num_teams + first_team] =
        result[num_teams + first_team] + increment
  }
  result
}

# Trains a Poisson model that models the matches' results.
#
# Each team i has an offense parameter o_i and a defense parameter d_i. The
# number of goals scored by team i against team j is assumed to be sampled from
# a distribution Poisson(c + max(0, o_i - d_j)), where c is a constant (fixed,
# not learnt).
#
# This function finds the set of parameters that maximizes the data.
#
# Returns:
#   A model as returned by the function 'optim'. It is a list with components:
#     par: the set of parameters that maximizes the likelihood of the data.
#     value: -log(P(data | parameters)).
#     (and other things, see ?optim for more details).
train_model = function () {
  # The parameters are in a vector where the first num_teams elements are the
  # offense params of each team, and the next num_teams elements are the defense
  # params of each team.

  # The initial parameters are initialized randomly.
  offense_params = c(runif(n=num_teams, min=0, max=3))
  defense_params = c(runif(n=num_teams, min=0, max=2))
  all_pars = c(offense_params, defense_params)

  optim(par=all_pars, fn=loss, method="L-BFGS-B", gr=gradient,
              control=list(maxit=1000, trace=1), lower=rep(0,length(all_pars)))
}

# Arguments:
#   team_str: the name of the team (a string).
team_index = function(team_str) {
  as.integer(factor(team_str, levels=all_teams))
}

# Arguments:
#   par: the vector of parameters.
#   team_str: the name of the team (a string).
print_team_info = function(par, team_str) {
  team = team_index(team_str)
  cat(team_str, ": ", par[team], " / ", par[num_teams + team])
}

# Arguments:
#   par: the vector of parameters.
#
# Returns:
#   A data frame with columns:
#     team: a factor.
#     offense: the offense parameter (a double).
#     defense: the defense parameter (a double).
params_data_frame = function(par) {
  result = data.frame(team=all_teams,
                      offense=seq(0, 0, len=num_teams),
                      defense=seq(0, 0, len=num_teams))
  for (i in seq(1, num_teams)) {
    team = as.integer(result[i, 'team'])
    result[i, 'offense'] = offense(par, team)
    result[i, 'defense'] = defense(par, team)
  }
  result
}

main = function() {
 model = train_model()
 write.csv(params_data_frame(model[['par']]),
           file="data/poisson_model.csv",
           row.names=FALSE)
}

main()

get_pbest = function( ibest_position, ibest_error, costs, position ){
    
    replace_ = min( costs ) < min( ibest_error )
    
    if ( replace_ ){
        pbest = position[ , rep( which.min( costs ), length( costs ) ) ]
    } else{
        pbest = ibest_position[ , rep( which.min( ibest_error ), length(ibest_error) ) ]
    }
    pbest
}

initialise_pso = function( pop_size, init_bounds, PSO_dims, PSO_mat_size ){
    
    position = matrix( NaN, nrow = PSO_dims, ncol = pop_size )
    rownames( position ) = names( init_bounds )
    velocity = position
    
    position[ "resp_prob1", ] = runif( pop_size, init_bounds$resp_prob1[1], init_bounds$resp_prob1[2] )
    position[ "resp_prob2", ] = runif( pop_size, init_bounds$resp_prob2[1], init_bounds$resp_prob1[2] - position[ "resp_prob1", ] )
    position[ "prob_remember", ] = runif( pop_size, init_bounds$prob_remember[1], init_bounds$prob_remember[2] )
    position[ "prob_rft_forget", ] = runif( pop_size, init_bounds$prob_rft_forget[1], init_bounds$prob_rft_forget[2] )
    position[ "transition_prob", ] = runif( pop_size, init_bounds$transition_prob[1], init_bounds$transition_prob[2] )
    position[ "prp_learn_prob", ] = runif( pop_size, init_bounds$prp_learn_prob[1], init_bounds$prp_learn_prob[2] )
    
    velocity[ "resp_prob1", ] = runif( pop_size, init_bounds$resp_prob1[1], init_bounds$resp_prob1[2] )
    velocity[ "resp_prob2", ] = runif( pop_size, init_bounds$resp_prob2[1], init_bounds$resp_prob1[2] - position[ "resp_prob1", ] )
    velocity[ "prob_remember", ] = runif( pop_size, init_bounds$prob_remember[1], init_bounds$prob_remember[2] )
    velocity[ "prob_rft_forget", ] = runif( pop_size, init_bounds$prob_rft_forget[1], init_bounds$prob_rft_forget[2] )
    
    velocity[ "transition_prob", ] = runif( pop_size, init_bounds$transition_prob[1], init_bounds$transition_prob[2] )
    velocity[ "prp_learn_prob", ] = runif( pop_size, init_bounds$prp_learn_prob[1], init_bounds$prp_learn_prob[2] )

    ibest_cost = rep( Inf, pop_size )
    ibest_position = position
    
    pbest_cost = Inf
    pbest_position = rep( NaN, PSO_dims )
    
    list( position = position, velocity = velocity, ibest_cost = ibest_cost, ibest_position = ibest_position,
        pbest_cost = pbest_cost, pbest_position = pbest_position )
}

clamp = function(x, low, hi){
    x[ x < low ] = low
    x[ x > hi ] = hi
    x
}

# pso_bounceback = function( PSO_dims, position, param_bounds ){
#     row = 1
#     for ( row in 1:PSO_dims ){
#         lower_bound = param_bounds[[row]][1]
#         upper_bound = param_bounds[[row]][2]
#         violations = which( position[ row, ] < lower_bound | position[ row, ] > upper_bound )
#         
#         for ( k in seq_along(violations) ){
#             violated_trial = position[ row, violations[k] ]
#             while ( { low = violated_trial < lower_bound; high = violated_trial > upper_bound; low | high } ){
#                 if ( high ) violated_trial = 2*upper_bound - violated_trial
#                 if ( low ) violated_trial = 2*lower_bound - violated_trial
#             }
#             position[ row, violations[k] ] = violated_trial
#         }
#     }
#     position
# }

inner_random = function( PSO_dims, position, velocity, param_bounds ){

    trial_position = position + velocity
    
    for ( row in 1:PSO_dims ){
        lower_bound = param_bounds[[row]][1]
        upper_bound = param_bounds[[row]][2]
        lower_violations = which( trial_position[ row, ] < lower_bound )
        upper_violations = which( trial_position[ row, ] > upper_bound )
        
        trial_position[ row, lower_violations ] = runif( length(lower_violations), lower_bound, position[ row, lower_violations ] )
        trial_position[ row, upper_violations ] = runif( length(upper_violations), position[ row, upper_violations ], upper_bound )
        
    }
    new_velocity = trial_position - position
    
    list( position = trial_position, velocity = new_velocity )
}

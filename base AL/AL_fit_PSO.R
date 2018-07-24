##### Load libraries #####
library( data.table )
library( CAB2 )
library( compiler )
library( foreach )

cluster = T
if ( cluster ){
    library( doMPI )
    cl = startMPIcluster()
    registerDoMPI( cl )
    cat( "Cluster size", clusterSize(cl), "\n" )
} else{
    library( doParallel )
    cl = makeCluster( 8, outfile = "output.txt" )
    registerDoParallel( cl )
}

# Expose packages to each node
package = foreach( i = 1, .packages = c("CAB2", "data.table", "compiler" ) ) %dopar% {
    (.packages())
}
(package)
rm(package)

###################################

##### Load functions and data #####
source( "AL.do_rapidly_changing_fitting.R" )
source( "AL_new_helper_fitting.R" )
# Load "expt_info"
load( "expt_info.RData" )
# Load "fitting_set"
load( "fitting_set.RData" )
# Load "fitting_summarise"
source( "data_summary_fitting.R" )
source( "recode_components.R" )
source( "pso_helper.R" )

###################################

##### Data identifiers and subset the data #####
if ( cluster ){
    subject = as.numeric( Sys.getenv( "SLURM_ARRAY_TASK_ID" ) )
} else{
    subject = 3
}
cat( "Subject", subject, "\n" )
file_name = paste0( "subject_", subject, "_model_fits.RData" )
expt_info = expt_info[ Subject == subject & Condition == 3 ]
pretrain_expt_info = make_pretraining_expt_info( expt_info )
fitting_set = fitting_set[[ subject ]]
recode_comps( fitting_set )
################################################

##### Parameters and variables #####
variable_names = get_variable_names( fitting_set )
cost_names = c( unlist( variable_names ), "overall" )

param_bounds = list(
    resp_prob1 = c( 0, 1 ), resp_prob2 = c( 0, 1 ),
    prob_remember = c( 0, 1 ), prob_rft_forget = c( 0, 1 )
)
# Init upper bound for resp_prob2 depends on resp_prob1
init_bounds = list(
    resp_prob1 = c( 0, 0.03 ), resp_prob2 = c( 0, 0.03 ),
    prob_remember = c( 0.5, 1 ), prob_rft_forget = c(0, 0.1 )
)
####################################

##### PSO #####
# Main settings #
pop_size = 62
iterations = 150
flat_w = 70
#################
PSO_dims = length( param_bounds )
PSO_mat_size = PSO_dims * pop_size
w_max = 0.9; w_min = 0.4
c1 = 2; c2 = 2
threshold = 1.01
vmax = 0.05; vmin = -0.05

PSO_initial = initialise_pso( pop_size, init_bounds, PSO_dims, PSO_mat_size )
position = PSO_initial$position
velocity = clamp( PSO_initial$velocity, vmin, vmax )
ibest_cost = PSO_initial$ibest_cost
ibest_position = PSO_initial$ibest_position
pbest_cost = PSO_initial$pbest_cost
pbest_position = PSO_initial$pbest_position

return_matrix = data.table( iteration = rep( 1:iterations, each = pop_size ) )
###############

##### Run PSO #####
for ( i_ in 1:iterations ){
    w = max( w_max - (w_max-w_min)/flat_w * (i_-1), w_min )
    start_time = Sys.time()
    cat( "Iteration", i_, "\n" )
    
    costs_ = foreach( n = 1:pop_size ) %dopar% {
        AL.pre_train_fitting = cmpfun( AL.pre_train_fitting, options = list( optimize = 3 ) )
        AL.do_fitting = cmpfun( AL.do_fitting, options = list( optimize = 3 ) )
        
        params = position[ , n ]
        org = make_org_fitting( params )
        
        # Pretraining
        for ( session_ in 1:15 ) AL.pre_train_fitting( org, pretrain_expt_info, params )
        # Experiment
        model_event_records = lapply( 1:35, function( session_, org_, params_ ){
            expt_info_ = expt_info[ session_ ]
            AL.do_fitting( org_, expt_info_, params_ )
        }, org_ = org, params_ = params )
        model_event_records = rbindlist( model_event_records, idcol = "session" )
        add_component_labels( model_event_records )
        # Compute costs
        costs = fitting_summarise( model_event_records, fitting_set, variable_names )
        costs[ , names(params) := as.list(params) ]
        
        if ( is.nan( costs$overall ) ) break
        
        costs
    }
    costs_ = rbindlist( costs_ )
    
    return_matrix[ iteration == i_, names( costs_ ) := costs_ ]
    overall_costs = costs_$overall
    
    # Update ibest
    update_particles = overall_costs/ibest_cost < threshold
    update_particles[ is.na(update_particles) ] = T
    
    ibest_cost[ update_particles ] = overall_costs[ update_particles ]
    ibest_position[ , update_particles ] = position[ , update_particles ]
    
    if ( any( ibest_cost < pbest_cost ) ){
        new_pbest_index = which.min( ibest_cost )
        pbest_cost = ibest_cost[ new_pbest_index ]
        pbest_position = ibest_position[ ,new_pbest_index ]
    }
    
    # Update velocity
    r1 = runif( PSO_mat_size, 0, c1 ); r2 = runif( PSO_mat_size, 0, c2 )
    velocity = w * velocity + r1 * (ibest_position - position) + r2 * (pbest_position - position)
    velocity = clamp( velocity, vmin, vmax )
    # Update position
    new_positions = inner_random( PSO_dims, position, velocity, param_bounds )
    position = new_positions$position
    velocity = new_positions$velocity
    
    if ( i_ %% 10 == 1 ) save( return_matrix, file = file_name )
    
    print( difftime( Sys.time(), start_time, units = "secs" ) )
}

save( return_matrix, file = file_name )

##### Tidy up #####
if ( cluster ){
    closeCluster(cl)
    mpi.quit()
} else{
    stopCluster(cl)
}

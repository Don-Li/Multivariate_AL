fitting_summarise = function( model_event_records, model_data_summary, variable_names ){
    
    predicted_summary = model_event_records[ ,{
        counts = factorial_counts( event, 11:12, 101:108, 1:2, 10 )
        probs = get_probs(counts)
        
        irts = ixyi( event, time, 1:2, 1:2, c(99, 11:12, 101:108 ) )
        irt_counts = irts$ixyi_counts
        irt_probs = irt_counts/sum(irt_counts)
        
        prps = ixyi( event, time, 11:12, 1:2, c(99,101:108) )
        prp_counts = prps$ixyi_counts
        prp_probs = prp_counts/sum(prp_counts)
        
        iris = ixyi( event, time, 11:12, 11:12, c(99,101:108) )
        iri_counts = iris$ixyi_counts
        iri_probs = iri_counts/sum(iri_counts)
        
        list( predicted = list( counts, 
                irts$ixyi, irt_counts,
                prps$ixyi ),
            var_names = variable_names[[1]] )
    }, by = "component" ]
    
    pp = model_event_records[ ,{
        pp_counts = factorial_time_bin_2( event, time, 11:12, 101:108, 1:2, 60, 1, 2.5 )
        list( predicted = pp_counts,
            var_names = variable_names[[2]],
            component = 99 )
    } ]
    
    predicted_summary = rbindlist( list( predicted_summary, pp ), use.names = T )
    model_data_summary[ predicted_summary, predicted := i.predicted, on = c("var_names","component") ]
    
    model_data_summary[ , {
        var_names_ = c( paste0( var_names, "_", component ), "overall" )
        cost = vector( "numeric", .N+1 )
        for ( i in seq_along(cost) ){
            if ( i == length(cost) ){
                cost[i] = exp( sum( log( cost[-length(cost)] ) ) / length( cost-1 ) )
            } else{
                if ( var_names[i] %in% c( "irt", "prp", "iri" ) ){
                    if ( is.null( predicted[[i]] ) ){
                        cost[i] = Inf
                    } else{
                        cost[i] = ks( values[[i]], predicted[[i]] )
                    }
                } else{
                    if ( length(predicted[[i]]) == 0 ){
                        cost_ = Inf
                    } else{
                        cost_ = sum( abs( values[[i]] - predicted[[i]]) )
                        if ( is.nan(cost_) ) cost_ = Inf
                    }
                    cost[i] = cost_
                }
            }
        }
        return_table = as.data.table( as.list(cost) )
        names( return_table ) = var_names_
        return_table
    } ]
}

get_probs = function(x){
    z = x[,1]/rowSums(x)
    z[ is.nan(z) ] = 0.5
    z
}
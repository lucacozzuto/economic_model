process makePlot {

    container 'biocorecrg/econ_r:0.1'

    errorStrategy = 'ignore'
    publishDir(params.output, mode:'copy') 

    tag { id }
    
    input:
    tuple val(id), path(res), val(params)

    output:
    tuple val(id), path("*.pdf")
    
    script:
    """
    generalized_R_graph.R -title \"Price averages\" -input ${res} -output ${id} -params \"${params}\" 
    """
}


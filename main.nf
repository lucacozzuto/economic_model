#!/usr/bin/env nextflow

nextflow.enable.dsl=2

/*
===========================================================
 pipeline about economic model

 @authors
 Luca Cozzuto <lucacozzuto@gmail.com>
 Nicola Visonà <n.visona@unimc.it>
=========================================================== 
*/

/*
*/

params.help            = false
params.resume          = false


log.info """

╔═╗╔═╗╔═╗╔╗╔
║╣ ║  ║ ║║║║
╚═╝╚═╝╚═╝╝╚╝                                                                                       
====================================================
repetitions                 : ${params.repetitions}
maxcpus                     : ${params.maxcpus}
values                      : ${params.values}
graph_params                : ${params.graph_params}
template                    : ${params.template}
model                       : ${params.model}
output (output folder)      : ${params.output}
"""


if (params.help) {
    log.info 'This is the the econ pipeline'
    log.info '\n'
    exit 1
}
if (params.resume) exit 1, "Are you making the classical --resume typo? Be careful!!!! ;)"


nlogo = file(params.model)
xmlfile = file(params.template)

if( !nlogo.exists() ) exit 1, "Missing Industrial-Revolution.nlogo file!"
if( !xmlfile.exists() ) exit 1, "Missing template xml file!"

// Setting the reference genome file and the annotation file (validation)
values = file(params.values)
if( !values.exists() ) exit 1, "Missing values file: '${params.values}'. Specify the path with --values parameter"


outputfolder    	= "${params.output}"
Channel.from(params.graph_params).set{graph_pars}

// Create a channel for values 
Channel
    .from(values.readLines())
    .map { line ->
        list = line.split("\t")
            if (list.length <2) {
			  error "ERROR!!! Values file has to be tab separated\n" 
	        }        
	        if (list[0]!= "") {
	        	param_name = list[0]
			initial_val = list[1]
			final_val = list[2]
			step_val = list[3]
			[ param_name, initial_val, final_val, step_val ]
        }  
    }.set{ pipe_params}


include { runModel } from "${projectDir}/modules/model.nf"
include { xmlMod } from "${projectDir}/modules/xmlmod.nf"
include { joinFiles } from "${projectDir}/modules/joinfiles.nf"
include { makePlot } from "${projectDir}/modules/rplot.nf"

Experiments = Channel.of( "testing1" )

pipe_params.map {
	def BigDecimal start = Float.parseFloat(it[1])
	def BigDecimal fin = Float.parseFloat(it[2])
	def BigDecimal step = Float.parseFloat(it[3])
	def ranges = []
	for (i = fin; i > start; i-=step) {
		ranges.push(i)
	}
	ranges.push(start)
	[it[0], ranges]
	
}.transpose().set{reshaped_pars}

batches_r = (params.repetitions/params.maxcpus)
batches = batches_r.round()
real_rep = batches*params.maxcpus

cpu_channel = Channel.of( params.maxcpus )


println "We will do ${batches} batches: ${real_rep} repetitions!"

n_batches = Channel.from( 1..batches )


workflow {
   xml_files = xmlMod (reshaped_pars.combine(cpu_channel), xmlfile)
   xml_files.combine(Experiments).combine(n_batches).map{
   		["${it[0]}__${it[5]}", it[1], it[2], it[3], it[4]]
   }.set{data_for_model}
   
   res_model = runModel(data_for_model, nlogo)
   res_model.map{
   		def ids = it[0].split("__")
   		[ids[0], it[1]]
   }.groupTuple().set{files_pieces}
   
   concat_res = joinFiles(files_pieces)
   makePlot(concat_res.combine(graph_pars))
//   res_model.groupTuple().view()
}

workflow.onComplete {
    println "Pipeline completed!"
    println "Started at  $workflow.start" 
    println "Finished at $workflow.complete"
    println "Time elapsed: $workflow.duration"
    println "Execution status: ${ workflow.success ? 'OK' : 'failed' }"
}

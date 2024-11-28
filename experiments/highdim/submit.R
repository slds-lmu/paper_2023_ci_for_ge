library(batchtools)

reg = loadRegistry(Sys.getenv("RESAMPLE_PATH_HIGHDIM"), writeable = TRUE)

while (TRUE) { 
    print(getStatus())
    jt = unwrap(getJobTable())

    jt_big = jt[task_name == "highdim_1600",]
    jt_very_big = jt[task_name == "highdim_3200" | task_name == "highdim_6400", ]
    jt_small = jt[task_name != "highdim_1600" & task_name != "highdim_3200" & task_name != "highdim_6400", ]

    f = function(jt, resources) {
        jt = jt[job.id %nin% findRunning()[[1]]]
        jt = jt[job.id %nin% findDone()[[1]]]
        jt = jt[job.id %nin% findQueued()[[1]]]

        chunks = batchtools::chunk(jt$job.id, chunk.size = 50, shuffle = TRUE)

        tbl = data.table(job.id = jt$job.id, chunk = chunks) 

        submitJobs(tbl, resources = resources)
    }


    f(jt_big, list(walltime = 3600 * 2, memory = 512 * 64L, partition = "mb"))
    f(jt_very_big, list(walltime = 3600 * 2, memory = 512 * 128L, partition = "mb"))
    f(jt_small, list(walltime = 3600 * 2, memory = 512 * 16L, partition = "mb"))

    Sys.sleep(60 * 10)
}

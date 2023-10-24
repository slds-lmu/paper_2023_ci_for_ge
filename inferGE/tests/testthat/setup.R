lg = lgr::get_logger("mlr3")
old_threshold = lg$threshold
old_plan = future::plan()
lg$set_threshold("warn")

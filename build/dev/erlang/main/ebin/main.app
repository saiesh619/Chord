{application, main, [
    {vsn, "1.0.0"},
    {applications, [gleam_erlang,
                    gleam_otp,
                    gleam_stdlib,
                    gleam_time,
                    gleeunit,
                    randomlib]},
    {description, ""},
    {modules, [chord_math,
               chord_msgs,
               chord_node,
               chord_stats,
               chord_supervisor,
               chord_util,
               main,
               main@@main]},
    {registered, []}
]}.

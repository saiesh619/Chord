{application, main, [
    {vsn, "1.0.0"},
    {applications, [gleam_erlang,
                    gleam_otp,
                    gleam_stdlib,
                    gleam_time,
                    gleeunit,
                    randomlib]},
    {description, ""},
    {modules, [chord_node,
               chord_supervisor,
               main]},
    {registered, []}
]}.

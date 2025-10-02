{application, gleeunit, [
    {vsn, "1.6.1"},
    {applications, [gleam_stdlib]},
    {description, "A simple test runner for Gleam, using EUnit on Erlang"},
    {modules, [gleeunit,
               gleeunit@internal@gleam_panic,
               gleeunit@internal@reporting,
               gleeunit@should,
               gleeunit_ffi,
               gleeunit_gleam_panic_ffi,
               gleeunit_progress]},
    {registered, []}
]}.

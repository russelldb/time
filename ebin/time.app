{application, time,
 [{description, "Time tracking application"},
  {vsn, "0.0.1"},
  {modules, [time, time_sup, time_app]},
  {registered, [time, time_sup]},
  {applications, [kernel, stdlib]},
  {mod, {time_app,[]}},
  {env, []}]}.


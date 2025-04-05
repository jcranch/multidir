# Things to do

* Should we enforce, when parsing a Proj, that config variables have
  names which are alphanumeric+underscore only (... because the shell
  environment will need that)?

* Ability to pass extra config on the command-line to run/tasks? (Not
  so much use for "run" perhaps)

* Switch to commands being a list rather than a single string (start
  with ReadConfig and change command, in Specification). Have defaults
  to read a single string in the TOML?

* Flag for echoing commands during run jobs?

* A more full-featured `recipes.toml` example file would be desirable.

# 0.5.0 2019-05-20 New York, NY, USA

- Add solutions to problems:
  - 111
  - 148
  - 359
  - 662
  - 666
- Performance improvements to some solutions
- OCamlformat all the things!
- Switch from Core.Command to Cmdliner for CLI interface
  - Solutions are now run by the `run` command, not top-level commands.
  - Solutions can be listed by the `list` command.
- Upgrade to Jane Street libs v0.12.0
- Use Base-style map and set interfaces, preparing to reduce the dependencies to
  just Base where possible.

# 0.4.0 2018-11-17 New York, NY, USA

- Initial release using topkg.

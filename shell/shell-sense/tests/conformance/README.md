# Native completion conformance

`cases.tsv` is the shared provider contract. Every listed case runs against
the live native provider implementation for Zsh, Fish, and Bash. A shell test
may not silently skip one of these rows.

Labels and normalized kinds are recorded per shell because native APIs can
represent the same edit differently. Filesystem cases additionally require
the typed resource to end in the shared canonical suffix after one trailing
slash is removed. `-` means the case must not claim a filesystem resource.

`capabilities.tsv` records intentional differences in the shells' supported
public APIs. `required` means the shell must have a regression test;
`unsupported` means Shell Sense must not synthesize that capability;
`limited` requires the limitation to remain documented and covered by the
strongest faithful test path.

In particular, ZLE and Fish expose safe editor callbacks for idle
documentation refresh. Bash's public Readline integration does not: running
the mailbox decoder from a signal trap can corrupt Readline's stack. The Bash
client therefore consumes delayed documentation on the next Shell Sense
action, such as documentation scrolling or toggling.

The files are deliberately dependency-free TSV so all three shells can read
the same source of truth without spawning a JSON/TOML parser in provider
tests. Tabs and newlines are not valid inside a conformance field.

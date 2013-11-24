xmonad-javran
=============

Javran's xmonad config

Note: please use `make` in this project instead of `xmonad --recompile`
because the latter might not add my customized configs (e.g. `JavranXMonad`)
into its library search path properly.

`StreamConvert` usage:

Description:

Read from `stdin`, load template from text file, and write pretty formatted lines to `stdout`

Input stream:

* input stream should be a valid JSON object per line.

* `{time_1}` indicates to use function bound to String `time` to process the data.
In addition, `1` is an optional tag.

* `{date}` indicates to use function bound to String `date` to process the data.

* `{time_}` `{time}` and `{time_1}` are all valid, however only the last one has a tag.

* the usage of tag is explained in `Output`.

Template:

* For now, `StreamConvert` uses `dzen2` syntax to color everything.

* The only argument to `StreamConvert` is a text file, the template.

* `StreamConvert` reads all lines from template, removing newlines and concatenate them together.
So you don't need to and shouldn't give a `\` at the end of each line.

* Whitespaces counts(of course newline is an exception), so make sure not to have any unintended whitespace in the template.

* to escape, use `!`-sign. `StreamConvert` will treat any single character right after this `!` literally.
(i.e. `!!` for `!`, `![` for `["`)
Currently only two characters have special meanings, they are `[` and `{`.

Output:

* `{date_1}` gives the converted result of `{date_1}` given in `stdin`.
If the tag is not given. The first occurence in the `stdin` will be used.

* `[#FF0000]` means to color everything after it to color `#FF0000`

* Any `[...]` will shadow previous color settings.

* Depend on the slot functions' implementation, color settings might be changed.
So use `[..]` explicitly if anything unwanted happened.

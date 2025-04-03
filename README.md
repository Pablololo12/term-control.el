# term-control.el
A simple emacs package to take control of pop-up terminals, based on [toggle-term.el](https://github.com/justinlime/toggle-term.el)

# Usage

* `term-control-switch-to-term`
Switches to one of the terminals open or creates a new one if the name does not exists
The terminal is created at the bottom

* `term-control-switch-to-term-ver`
Switches to one of the terminals open or creates a new one if the name does not exists
The terminal is created on the left

* `term-control-toggle`
Toggles the last used terminal, opens at the bottom

* `term-control-toggle-ver`
Toggles the last used terminal, opens on the left

# Configuration

* `term-control-vsize` defaults to 33
The percentaje of screen terminals at the bottom will take when open

* `term-control-vsize` defaults to 50
The percentaje of screen terminals on the left will take when open

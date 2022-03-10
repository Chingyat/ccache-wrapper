# ccache-wrapper

ccache-wrapper.so is a shared library that implements its own execve functions,
which automatically invokes ccache when the executable is recognized as a C/C++ compiler.

ccache-wrapper.so should be loaded before libc using `LD_PRELOAD`.
The `ccache-run` script is provided to do that.

## Install

```sh

make prefix=$HOME/.local install

```

## License

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.



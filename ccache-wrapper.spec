#
# spec file for package ccache-wrapper
#
# Copyright (c) 2022 SUSE LLC
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.

# Please submit bugfixes or comments via https://bugs.opensuse.org/
#


Name:           ccache-wrapper
Version:        0.1
Release:        1
Summary:	Automagically invoking ccache.
License:        GPL-3-or-later
URL:            https://github.com/Chingyat/ccache-wrapper
Source:         %{name}-%{version}.tar.gz

%description
ccache-wrapper.so is a shared library that implements its own execve functions, which automatically invokes ccache when the executable is recognized as a C/C++ compiler.

%prep
%setup -q

%build
%make_build prefix=/usr

%install
%make_install prefix=/usr libdir=%{_lib}

%post
%postun

%files
%license LICENSE
%doc README.md
%{_bindir}/ccache-run
%{_libdir}/%{name}/ccache-wrapper.so

%changelog


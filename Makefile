EXTRA_CFLAGS := -fPIC -Wall -Wextra -std=gnu89
EXTRA_LIBS = -ldl

CFLAGS += $(EXTRA_CFLAGS)
LDLIBS += $(EXTRA_LIBS)

targets := ccache-wrapper.so ccache-run
objs := ccache-wrapper.o

prefix ?= /usr/local
libdir := lib
bindir := bin

DESTDIR ?= 


.PHONY: all clean install ccache-run

all: $(targets)

clean:
	-$(RM) $(targets) $(objs)

ccache-wrapper.so: $(objs)
	$(LINK.o) -shared $^ $(LOADLIBES) $(LDLIBS) -o $@
	
ccache-run: ccache-run.in
	sed "s,@libdir@,$(libdir),g" $< | \
		sed "s,@prefix@,$(prefix),g" >$@
	chmod a+x $@

install: $(targets)
	install -m755 -d $(DESTDIR)$(prefix)/$(libdir)/ccache-wrapper
	install -m755 -t $(DESTDIR)$(prefix)/$(libdir)/ccache-wrapper ccache-wrapper.so
	install -m755 -d $(DESTDIR)$(prefix)/$(bindir)
	install -m755 -t $(DESTDIR)$(prefix)/$(bindir) ccache-run

	


NAME=elada
VERSION=1.8.7

DIST_DIR=ada-el-$(VERSION)
DIST_FILE=ada-el-$(VERSION).tar.gz

MAKE_ARGS += -XEL_BUILD=$(BUILD)

-include Makefile.conf

STATIC_MAKE_ARGS = $(MAKE_ARGS) -XEL_LIBRARY_TYPE=static
SHARED_MAKE_ARGS = $(MAKE_ARGS) -XEL_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XUTILADA_BASE_BUILD=relocatable -XUTIL_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XXMLADA_BUILD=relocatable
SHARED_MAKE_ARGS += -XLIBRARY_TYPE=relocatable

include Makefile.defaults

# Build executables for all mains defined by the project.
build-test::	lib-setup
	cd regtests && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)

# Build and run the unit tests
test:	build
	bin/el_harness -l $(NAME): -xml el-aunit.xml

samples:
	cd samples && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)

install-samples:
	$(MKDIR) -p $(samplesdir)/samples
	cp -rp $(srcdir)/samples/*.ad[sb] $(samplesdir)/samples/
	cp -p $(srcdir)/samples.gpr $(samplesdir)
	cp -p $(srcdir)/config.gpr $(samplesdir)

$(eval $(call ada_library,$(NAME),.))
$(eval $(call alire_publish,alire.toml,el/elada,elada-$(VERSION).toml))

.PHONY: samples

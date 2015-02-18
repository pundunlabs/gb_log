include ../vsn.mk
VSN=$(GB_LOG_VSN)

EBIN = ../ebin

RM ?= rm -f

APP_NAME = gb_log
APP_FILE= $(APP_NAME).app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

MODULES = \
	gb_log \
	gb_log_sup \
	gb_log_app

ERL_FILES = $(MODULES:%=%.erl)
ERL_BEAMS = $(MODULES:%=$(EBIN)/%.beam)

INCLUDE_DIRS = \
	../include \
	../../gb_conf/include

ERL_INCLUDE = $(INCLUDE_DIRS:%=-I %)
.PHONY: all clean beams

all: $(EBIN) $(APP_TARGET) beams

beams: $(ERL_BEAMS)

$(EBIN)/%.beam: %.erl
	erlc $(ERL_INCLUDE) -o $(EBIN) $<

$(EBIN):
	mkdir $@

$(APP_TARGET): $(APP_SRC) 
	@set -e; mods=`echo $(MODULES) | sed -e 's; ;,;g'` ; \
	sed -e "s;%MODULES%;$$mods;" $< | \
	sed -e 's;%VSN%;$(VSN);' > $@

clean:
	$(RM) ../ebin/*.app
	$(RM) ../ebin/*.beam

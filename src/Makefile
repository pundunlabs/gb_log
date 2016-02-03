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
	gb_log_app \
	gb_log_cfg \
	gb_log_oam \
	gb_log_el \
	gb_trace \

ERL_FILES = $(MODULES:%=%.erl)
ERL_BEAMS = $(MODULES:%=$(EBIN)/%.beam)

HRL_FILES = ../include/gb_log.hrl

INCLUDE_DIRS = \
	../include \
	../../gb_conf/include

ERL_FLAGS = -Wall
ERL_INCLUDE = $(INCLUDE_DIRS:%=-I %)
.PHONY: all clean beams

all: $(EBIN) $(APP_TARGET) beams

beams: $(ERL_BEAMS)

gb_log_cfg.erl: gb_log_cfg.yrl
	erlc $<

$(EBIN)/%.beam: %.erl $(HRL_FILES)
	erlc $(ERL_FLAGS) $(ERL_INCLUDE) -o $(EBIN) $<

$(EBIN):
	mkdir $@

$(APP_TARGET): $(APP_SRC)
	@set -e; mods=`echo $(MODULES) | sed -e 's; ;,;g'` ; \
	sed -e "s;%MODULES%;$$mods;" $< | \
	sed -e 's;%VSN%;$(VSN);' > $@

clean:
	$(RM) ../ebin/*.app
	$(RM) ../ebin/*.beam

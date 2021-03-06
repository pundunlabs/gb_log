include ./vsn.mk
VSN=$(GB_LOG_VSN)
APP_NAME = gb_log

SUBDIRS = src c_src

.PHONY: all subdirs $(SUBDIRS) edoc eunit clean

all: subdirs

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

edoc:
	erl -noshell -run edoc_run application "'$(APP_NAME)'" \
               '"."' '[{def,{vsn,"$(VSN)"}}, {source_path, ["src", "test"]}]'

eunit:
	erl -noshell -pa ebin \
	-eval 'eunit:test("ebin",[verbose])' \
	-s init stop

clean:
	rm -f ./ebin/*

realclean: clean


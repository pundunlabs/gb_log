PRIV_OBJS = ../priv/objs
PRIV_BIN = ../priv/bin

UDP_LOG_SERVER = daemonize \
		 logwrap \
		 udp_server \

UDP_LOG_SERVER_OBJECTS = $(UDP_LOG_SERVER:%=$(PRIV_OBJS)/%.o)

TARGETS = $(PRIV_BIN) \
	  $(PRIV_OBJS) \
	  $(PRIV_BIN)/log_server \


.PHONY: all

all: $(TARGETS)

# Dependencies
$(PRIV_OBJS)/daemonize.o: daemonize.h
$(PRIV_OBJS)/logwrap.o: logwrap.h

show:
	@echo $(TARGETS)
	@echo $(PRIV_OBJS)
	@echo $(UDP_LOG_SERVER_OBJECTS)

clean:
	rm $(PRIV_BIN)/log_server $(UDP_LOG_SERVER_OBJECTS)

$(PRIV_OBJS):
	mkdir -p $@

$(PRIV_BIN):
	mkdir -p $@

$(PRIV_OBJS)/%.o: %.c
	$(CC) -c -O2 -g -Wall $< -o $@

$(PRIV_BIN)/log_server: $(UDP_LOG_SERVER_OBJECTS)
	$(CC) -o $@ $^


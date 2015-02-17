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

# Depencies
demonize.c: damonize.h
logwrap.c: logwrap.h

show:
	@echo $(TARGETS)
	@echo $(PRIV_OBJS)
	@echo $(UDP_LOG_SERVER_OBJECTS)


$(PRIV_OBJS):
	mkdir -p $@

$(PRIV_BIN):
	mkdir -p $@

$(PRIV_OBJS)/%.o: %.c
	$(CC) -c -Wall $< -o $@

$(PRIV_BIN)/log_server: $(UDP_LOG_SERVER_OBJECTS)
	$(CC) -o $@ $^


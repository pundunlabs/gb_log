#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <libgen.h>
#include <string.h>
#include <signal.h>
#include "logwrap.h"
#include "daemonize.h"

void usage(char **argv) {
    printf("Usage: %s -d <logdir> -l <logname>\n",	     basename(argv[0]));
    printf("       %s -p <port> -d <logdir> -l <logname>\n", basename(argv[0]));
    printf("       %s -s <log size> (size in megabytes of log file before wrapping)\n", basename(argv[0]));
    printf("       %s -g <log generations> (number generations of log file before wrapping)\n", basename(argv[0]));
    printf("       %s -f (do not run as daemon)\n", basename(argv[0]));
    printf("       %s -m (write meta info to log i.e when deamon started and when wrapping)\n", basename(argv[0]));
    printf("       %s -h (write remote host and port to log)\n", basename(argv[0]));
}

char* getarg(char param, int argc, char **argv) {
    int i;
    for (i=1; i<argc; i++) {
	if (argv[i][0] == '-' && argv[i][1] == param) {
	    if (argv[i+1])
		return argv[i+1];
	}
    }
    return NULL;
}

/* check if flag is set or not */
char getopt(char param, int argc, char **argv) {
    int i;
    for (i=1; i<argc; i++) {
	if (argv[i][0] == '-' && argv[i][1] == param) {
	    return 1;
	}
    }
    return 0;
}

int getport(int argc, char **argv) {
    char *portarg;
    if((portarg = getarg('p', argc, argv))) {
	return atoi(portarg);
    }
    /* if not in args return default port */
    return 32000;
}

static void logsize(int argc, char **argv) {
    char *logsizep;
    unsigned int logsize;
    if ((logsizep = getarg('s', argc, argv))) {
	logsize = atoi(logsizep);
	set_max_logsize(logsize);
    }
}

static void loggenerations(int argc, char **argv) {
    char *loggenp;
    unsigned int loggens;
    if ((loggenp = getarg('g', argc, argv))) {
	loggens = atoi(loggenp);
	set_log_generations(loggens);
    }
}

void sig_handler(int signo) {
    if (signo == SIGUSR1)
	wrap_log();
}

int main(int argc, char **argv)
{
    int sockfd,n;
    struct sockaddr_in servaddr,cliaddr;
    socklen_t len;
    char from[512];
    int	 from_len;
    char *dirname;
    char *filename;
    char mesg[1024*1024];
    int buffsize = 1024 * 1024;
    int port;
    char write_remote_info;

    if (argc < 4) {
	usage(argv);
	exit(EXIT_SUCCESS);
    }

    len = sizeof(cliaddr);

    dirname = getarg('d', argc, argv);
    filename = getarg('l', argc, argv);

    port = getport(argc, argv);

    logsize(argc, argv);
    loggenerations(argc, argv);

    printf("port %d dir %s logname %s\n", port, dirname, filename);

    /* check if forground requested */
    if(!getopt('f',argc, argv)) {
	daemonize();
    }
    /* set write meta info accordingly */
    set_write_meta_info_msg(getopt('m', argc, argv));

    /* find and open the right log */
    start_log(dirname, filename);

    write_remote_info = getopt('h', argc, argv);

#define SIG_ERROR_MSG "ERROR register signalhandler for SIGUSR1\n"
    if (signal(SIGUSR1, sig_handler) == SIG_ERR)
	write_log_ext(SIG_ERROR_MSG, sizeof(SIG_ERROR_MSG)-1);

    sockfd=socket(AF_INET,SOCK_DGRAM,0);

    bzero(&servaddr,sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr=htonl(INADDR_ANY);
    servaddr.sin_port=htons(port);
    if (bind(sockfd,(struct sockaddr *)&servaddr,sizeof(servaddr))) {
	printf("error binding to socket\n");
	exit(EXIT_FAILURE);
    }

    if (setsockopt(sockfd, SOL_SOCKET, SO_RCVBUF, &buffsize, sizeof(buffsize)) == -1) {
	printf("Could not set big receive buffer\n");
    }

    for (;;)
    {
	if ((n = recvfrom(sockfd,mesg,sizeof(mesg),0,(struct sockaddr *)&cliaddr,&len)) == 0) {
	    continue;
	}

	/* put new line at max buffer */
	mesg[sizeof(mesg)-1] = '\n';

	if (write_remote_info) {
	    from_len = sprintf(from,"== %s:%d ==\n", inet_ntoa(cliaddr.sin_addr), ntohs(cliaddr.sin_port));
	    write_log_ext(from, from_len);
	}
	write_log_ext(mesg, n);
    }
}

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <libgen.h>
#include <string.h>
#include "logwrap.h"
#include "daemonize.h"

void usage(char **argv) {
    printf("Usage: %s -d <logdir> -l <logname>\n",	     basename(argv[0]));
    printf("       %s -p <port> -d <logdir> -l <logname>\n", basename(argv[0]));
    printf("       %s -s <log size> (size in megabytes of log file before wrapping)\n", basename(argv[0]));
    printf("       %s -g <log generations> (number generations of log file before wrapping)\n", basename(argv[0]));
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

    daemonize();

    /* find and open the right log */
    start_log(dirname, filename);

    sockfd=socket(AF_INET,SOCK_DGRAM,0);

    bzero(&servaddr,sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr=htonl(INADDR_ANY);
    servaddr.sin_port=htons(port);
    bind(sockfd,(struct sockaddr *)&servaddr,sizeof(servaddr));

    if (setsockopt(sockfd, SOL_SOCKET, SO_RCVBUF, &buffsize, sizeof(buffsize)) == -1) {
	printf("Could not set big receive buffer\n");
    }

    for (;;)
    {
	if ((n = recvfrom(sockfd,mesg,sizeof(mesg)-1,0,(struct sockaddr *)&cliaddr,&len)) == 0) {
	    continue;
	}
	mesg[n] = '\n';
	mesg[n+1] = '\0';
	from_len = sprintf(from,"== %s:%d ==\n", inet_ntoa(cliaddr.sin_addr), ntohs(cliaddr.sin_port));
	write_log_ext(from, from_len);
	write_log_ext(mesg, n);
    }
}

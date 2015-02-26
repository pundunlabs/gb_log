#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <libgen.h>
#include <string.h>
#include "logwrap.h"
#include "daemonize.h"

void usage(char **argv) {
    printf("Usage: %s -d <logdir> -l <logname>\n", basename(argv[0]));
    printf("       %s -p <port> -d <logdir> -l <logname>\n", basename(argv[0]));
}

char* getarg(char param, int argc, char **argv) {
    int i;
    for (i=1; i<argc; i++) {
	if (argv[i][0] == '-' && argv[i][1] == param) {
	    if(argv[i+1])
		return argv[i+1];
	}
    }
    usage(argv);
    exit(EXIT_FAILURE);
}

int getport(int argc, char **argv) {
    char *portarg;
    portarg = getarg('p', argc, argv);
    return atoi(portarg);
}

int main(int argc, char **argv)
{
    int sockfd,n;
    struct sockaddr_in servaddr,cliaddr;
    socklen_t len;
    char *dirname;
    char *filename;
    char mesg[3000];
    int buffsize = 1024 * 1024;
    int port;

    dirname = getarg('d', argc, argv);
    filename = getarg('l', argc, argv);

    if (argc > 5 )
	port = getport(argc, argv);
    else
	port = 32000;

    printf("port %d dir %s logname %s\n", port, dirname, filename);

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

    daemonize();

    for (;;)
    {
	len = sizeof(cliaddr);
	n = recvfrom(sockfd,mesg,3000,0,(struct sockaddr *)&cliaddr,&len);
	write_log_ext(mesg, n);
    }
}

#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <strings.h>
#include <stdlib.h>
#include <libgen.h>

#include "logwrap.h"
#include "daemonize.h"

void usage(char **argv) {
    printf("Usage: %s <logdir> <logname>\n", basename(argv[0]));
}

int main(int argc, char**argv)
{
    int sockfd,n;
    struct sockaddr_in servaddr,cliaddr;
    socklen_t len;
    char *dirname;
    char *filename;
    char mesg[3000];

    if (argc < 3) {
	usage(argv);
	exit(EXIT_FAILURE);
    }
    
    dirname = argv[1];
    filename = argv[2];

    daemonize();
    /* find and open the right log */
    start_log(dirname, filename);

    sockfd=socket(AF_INET,SOCK_DGRAM,0);

    bzero(&servaddr,sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr=htonl(INADDR_ANY);
    servaddr.sin_port=htons(32000);
    bind(sockfd,(struct sockaddr *)&servaddr,sizeof(servaddr));

    for (;;)
    {
	len = sizeof(cliaddr);
	n = recvfrom(sockfd,mesg,3000,0,(struct sockaddr *)&cliaddr,&len);
	write_log_ext(mesg, n);
    }
}

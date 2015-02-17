/*
 _________________________________________________________________________
 Copyright (C) 2010 by
 MobileArts
 S-111 61 Stockholm
 Sweden
 Email:    info@mobilearts.com
 Homepage: http://www.mobilearts.com
 This file contains confidential information that remains the property
 of MobileArts, and may not be used, copied, circulated, or distributed 
 without prior written consent of MobileArts.
 _________________________________________________________________________
 %%%
 Revision:         '$Id$'
 Author:           'Jonas Falkevik'
 This version:     '$Revision$'
 Last modified:    '$Date$'
 Last modified by: '$Author$'
 _________________________________________________________________________
   
 _________________________________________________________________________
*/
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "daemonize.h"

void daemonize() {
    pid_t pid;
    int i;

    pid = fork();
    if (pid > 0) 
	exit(EXIT_SUCCESS);
    if (pid < 0)
	exit(EXIT_FAILURE);

    pid = fork();
    if (pid > 0) 
	exit(EXIT_SUCCESS);
    if (pid < 0)
	exit(EXIT_FAILURE);

    /* Close all open filedescriptors */
    for (i=0; i<1024; i++) {
	close(i);
    }

    /* move away from inherited process group */
    if (setsid() < 0) {
	exit(EXIT_FAILURE);
    }
 
}

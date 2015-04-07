#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include <stdarg.h>
#include <dirent.h>
#include <sys/time.h>

#include "logwrap.h"

#define LOGBUF_SIZE 1024
char logbuf[LOGBUF_SIZE];

/* Dir structure could be bigger than FILENAME_MAX 
   but this is good enough for now
*/
char _log_wrap_log_dir[FILENAME_MAX];
char _log_wrap_log_file[FILENAME_MAX];
int log_fd;
int log_num;

/* prototypes */
void write_log(int *logfdp, int *log_nump, char *buf, int len);
void log_print(char *format, ...);
int open_log(int flags);

static int write_to_log(int logfd, char *buf, int len) {
    int res, left;
    left = len;
    
    for(;;) {
	res = write(logfd, buf, left);
	if (res == left) {
	    return len;
	}
	if (res < 0) {
	    return -1;
	}
	
	left -= res;
	buf += res;
    }
}

void write_log(int *logfdp, int *log_nump, char *buf, int len) {
    int size;
    
    size = lseek(*logfdp, 0, SEEK_END);
    if (size + len  > LOG_MAXSIZE) {
	close(*logfdp);
	move_logs();
	*logfdp = open_log(O_RDWR|O_CREAT|O_TRUNC);
    }
    write_to_log(*logfdp, buf, len);
}

void write_log_ext(char *buf, int len) {
    write_log(&log_fd, &log_num, buf, len);
}

void log_print(char *format, ...) {
    va_list ap;
    time_t now;
    struct tm *tmptr;
    struct timeval tv;
    int res;

    gettimeofday(&tv, NULL);
    now = tv.tv_sec;
    tmptr = localtime(&now);
    res = strftime(logbuf, LOGBUF_SIZE, "%F %T.", tmptr);
    res += snprintf(logbuf + res, LOGBUF_SIZE, "%3ld (%d) ", (long int) tv.tv_usec, getpid());
    va_start(ap, format);
    res += vsprintf(logbuf + res, format, ap);
    va_end(ap);
    
    write_log(&log_fd, &log_num, logbuf, res);
}

void log_plain_print(char *format, ...) {
    va_list ap;
    int res;

    va_start(ap, format);
    res = vsprintf(logbuf, format, ap);
    va_end(ap);

    write_log(&log_fd, &log_num, logbuf, res);
}

void move_logs() {
    char frf[FILENAME_MAX];
    char tof[FILENAME_MAX];

    int i;

    /* delete / remove the oldest generation */
    snprintf(tof, sizeof(tof), "%s/%s.%d", _log_wrap_log_dir,
   				           _log_wrap_log_file,
    					    LOG_GENERATIONS);
    unlink(tof);

    for (i=LOG_GENERATIONS; i>1; i--) {
	snprintf(frf, sizeof(frf), "%s/%s.%d", _log_wrap_log_dir,
					       _log_wrap_log_file,
					       i-1);
	snprintf(tof, sizeof(tof), "%s/%s.%d", _log_wrap_log_dir,
					       _log_wrap_log_file,
    					       i);
	rename(frf, tof);
    }
    
    snprintf(frf, sizeof(frf), "%s/%s", _log_wrap_log_dir,
					_log_wrap_log_file);
    snprintf(tof, sizeof(tof), "%s/%s.%d", _log_wrap_log_dir,
					   _log_wrap_log_file,
    					    i);
    rename(frf,tof);
}

int open_log(int flags) {
    char buf[FILENAME_MAX];
    int lfd;
    time_t now;
    struct tm *tmptr;
    int res;

    snprintf(buf, sizeof(buf), "%s/%s", _log_wrap_log_dir,
					_log_wrap_log_file);

    if ((lfd = open(buf, flags, LOG_PERM)) < 0) {
	fprintf(stderr, "cant open log file %s\n", buf);
	return -1;
    }

    now = time(NULL);
    tmptr = localtime(&now);
    res = strftime(buf, LOGBUF_SIZE, "%F %T LOGGING STARTED\n", tmptr);
    
    write_log(&lfd, &log_num, buf, res);
    
    return lfd;
}

void start_log(char *log_dir, char *log_file) {
    strncpy(_log_wrap_log_file, log_file, FILENAME_MAX);
    strncpy(_log_wrap_log_dir, log_dir, FILENAME_MAX);

    log_fd = open_log(O_RDWR|O_APPEND|O_CREAT);
}


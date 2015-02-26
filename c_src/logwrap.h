#ifndef __logwrap_h__
#define __logwrap_h__

#define LOG_GENERATIONS 30
#define LOG_MAXSIZE 16000000
#define LOG_PERM    0664

void write_log_ext(char *buf, int len);
void write_log(int *logfdp, int *log_nump, char *buf, int len);
int open_log(int flags);
void log_print(char *format, ...);
void log_plain_print(char *format, ...);
void move_logs();
void start_log(char *log_dir, char *log_file);

#endif
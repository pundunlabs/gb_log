#ifndef __logwrap_h__
#define __logwrap_h__

#define LOG_GENERATIONS 30
#define LOG_MAXSIZE (16 * 1024 * 1024)
#define LOG_PERM    0664

void write_log_ext(char *buf, int len);
void write_log(int *logfdp, int *log_nump, char *buf, int len);
int open_log(int flags);
void log_print(char *format, ...);
void log_plain_print(char *format, ...);
void set_write_meta_info_msg(char flag);
void wrap_log();
void move_logs();
void start_log(char *log_dir, char *log_file);
void set_log_generations(unsigned int set_log_generations);
void set_max_logsize(unsigned int set_log_maxsize);

#endif

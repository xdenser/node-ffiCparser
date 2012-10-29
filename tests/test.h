struct _iobuf {
        char *_ptr;
        int   _cnt;
        char *_base;
        int   _flag;
        int   _file;
        int   _charbuf;
        int   _bufsiz;
        char *_tmpfname;
        };
typedef struct _iobuf FILE;

int params_2(int a, char* b);

typedef struct AVFrac {
    int64_t val, num, den;
} AVFrac;



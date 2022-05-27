#ifndef _REGEXP_H_
#define _REGEXP_H_
/*
 * Definitions etc. for regexp(3) routines.
 *
 * Caveat:  this is V8 regexp(3) [actually, a reimplementation thereof],
 * not the System V one.
 */
#define NSUBEXP  10
typedef struct regexp {
        char *startp[NSUBEXP];
        char *endp[NSUBEXP];
        char regstart;          /* Internal use only. */
        char reganch;           /* Internal use only. */
        char *regmust;          /* Internal use only. */
        int regmlen;            /* Internal use only. */
        char program[1];        /* Unwarranted chumminess with compiler. */
} regexp;

#ifdef __cplusplus
extern "C" regexp *regcomp(const char *exp);
extern "C" int regexec(const regexp *prog, const char *str);
extern "C" void regsub(const regexp *prog, const char *source, char *dest);
extern "C" void regerror(const char *msg);
#elif defined(__STDC__)
extern regexp *regcomp(const char *exp);
extern int regexec(const regexp *prog, const char *str);
extern void regsub(const regexp *prog, const char *source, char *dest);
extern void regerror(const char *msg);
#else /* Old Style C */
extern regexp *regcomp();
extern int regexec();
extern void regsub();
extern void regerror();
#endif
#endif /* !_REGEXP_H_ */
